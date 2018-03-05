#### Project 2
#### Author: Erik Jonsson (23621)

######################################################################

#### set up the assignment

#get working directory
getwd()

#set working directory
setwd("D:/Erik/Dokument/764 local files/project 2") 

#get relevant libraries
library(tree)
library(randomForest)
library(rpart)
library(party)
library(lubridate)
library(ROCR)
library(e1071)
library(leaps)

## import data with accepted loans
loansacc = read.csv("loans_accepted.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = TRUE) #import data with accepted loans

######################################################################

#### clean the data

## some recoding into NA

loansacc$last_fico_range_high[loansacc$last_fico_range_high == "0"] = NA
loansacc$last_fico_range_low[loansacc$last_fico_range_low == "0"] = NA
loansacc$verification_status_joint[loansacc$verification_status_joint == ""] = NA

## some obvious removals of observations

# drop observations where loan_status is unknown
loansacc = loansacc[!(loansacc$loan_status == ""),]

# # drop observations from 2007
# loansacc = loansacc[!(substring(loansacc$issue_d, 5, 9) == "2007"),]

# drop joint loans
loansacc = loansacc[!(loansacc$application_type != "Individual"),]

## some dropping of columns

# drop all colums with more than 20% NAs
loansacc = loansacc[, -which(colMeans(is.na(loansacc)) > 0.2)]

#drop some columns that are not needed because of redundancy or not possible to analyze
loansacc$emp_title = NULL
loansacc$desc = NULL
loansacc$title = NULL
loansacc$zip_code = NULL

# drop columns that contain ex-post data
loansacc$collection_recovery_fee = NULL
loansacc$funded_amnt = NULL
loansacc$funded_amnt_inv = NULL
loansacc$grade = NULL
loansacc$initial_list_status = NULL
loansacc$int_rate = NULL
loansacc$last_credit_pull_d = NULL
loansacc$last_fico_range_high = NULL
loansacc$last_fico_range_low = NULL
loansacc$last_pymnt_amnt = NULL
loansacc$last_pymnt_d = NULL
loansacc$next_pymnt_d = NULL
loansacc$out_prncp = NULL
loansacc$out_prncp_inv = NULL
loansacc$policy_code = NULL
loansacc$pymnt_plan = NULL
loansacc$recoveries = NULL
loansacc$sub_grade = NULL
loansacc$total_pymnt = NULL
loansacc$total_pymnt_inv = NULL
loansacc$total_rec_int = NULL
loansacc$total_rec_late_fee = NULL
loansacc$total_rec_prncp = NULL
loansacc$installment = NULL
loansacc$tot_coll_amt = NULL
loansacc$collection_recovery_fee = NULL
loansacc$chargeoff_within_12_mths = NULL
loansacc$hardship_flag = NULL
loansacc$hardship_type = NULL
loansacc$hardship_reason = NULL
loansacc$hardship_status = NULL
loansacc$hardship_start_date = NULL
loansacc$hardship_end_date = NULL
loansacc$hardship_loan_status = NULL
loansacc$payment_plan_start_date = NULL
loansacc$debt_settlement_flag = NULL
loansacc$debt_settlement_flag_date = NULL
loansacc$settlement_status = NULL
loansacc$settlement_date = NULL

## clean the data further

# create variable for average of the two fico ranges and drop old ficos
loansacc$fico = (loansacc$fico_range_high + loansacc$fico_range_low)/2
loansacc$fico_range_high = NULL
loansacc$fico_range_low = NULL

# create recoded version of the loan_status variable and drop old variable
loansacc$fully_paid[loansacc$loan_status == "Default" | loansacc$loan_status == "Charged Off" | loansacc$loan_status == "Does not meet the credit policy. Status:Charged Off"] = "default"
loansacc$fully_paid[loansacc$loan_status == "Fully Paid" | loansacc$loan_status == "Does not meet the credit policy. Status:Fully Paid"] = "fully_paid"
loansacc$fully_paid[loansacc$loan_status == "Late (16-30 days)" | loansacc$loan_status == "Late (31-120 days)" | loansacc$loan_status == "In Grace Period" | loansacc$loan_status == "Current"] = "ongoing"
loansacc$loan_status = NULL

# drop ongoing loans
loansacc = loansacc[!(loansacc$fully_paid == "ongoing"),]

# convert the recoded status variable to factor
loansacc$fully_paid = as.factor(loansacc$fully_paid)

# create recoded version of the state variable accordning to regions and drop old variable, convert to factor
Pacific = c("WA", "OR", "CA", "AK", "HI")
Mountain = c("NV", "ID", "MT", "WY", "UT", "CO", "AZ", "NM")
Midwest = c("ND", "SD", "NE", "KS", "MN", "IA", "MO")
East_North_Central = c("WI", "IL", "IN", "OH", "MI")
West_South_Central = c("OK", "AR", "TX", "LA")
East_South_Central = c("KY", "TN", "MS", "AL")
South_Atlantic = c("DE", "MD", "VA", "DC", "WV", "NC", "SC", "GA", "FL")
Middle_Atlantic = c("NY", "PA", "NJ")
New_England = c("ME", "NH", "VT", "MA", "CT", "RI")
loansacc$state_region = with(loansacc,
  ifelse(addr_state %in% Pacific, "Pacific",
  ifelse(addr_state %in% Mountain, "Mountain", 
  ifelse(addr_state %in% Midwest, "Midwest",
  ifelse(addr_state %in% East_North_Central, "East_North_Central",
  ifelse(addr_state %in% West_South_Central, "West_South_Central",
  ifelse(addr_state %in% East_South_Central, "East_South_Central",
  ifelse(addr_state %in% South_Atlantic, "South_Atlantic",
  ifelse(addr_state %in% Middle_Atlantic, "Middle_Atlantic",
  ifelse(addr_state %in% New_England, "New_England",
  "other")))))))))
)
loansacc$addr_state = NULL
loansacc$state_region = as.factor(loansacc$state_region)

# convert issue_d to date
loansacc$issue_d = as.character(loansacc$issue_d)
loansacc$issue_d = paste(loansacc$issue_d, "-01", sep = "")
loansacc$issue_d = parse_date_time(loansacc$issue_d, "myd")

# convert earliest credit line to date
loansacc$earliest_cr_line = as.character(loansacc$earliest_cr_line)
loansacc$earliest_cr_line = paste(loansacc$earliest_cr_line, "-01", sep = "")
loansacc$earliest_cr_line = parse_date_time(loansacc$earliest_cr_line, "myd")

# create variable for time since earliest credit line and convert to numeric
loansacc$time_since_first_credit = loansacc$issue_d - loansacc$earliest_cr_line
loansacc$time_since_first_credit = as.numeric(loansacc$time_since_first_credit)

#drop earliest credit line
loansacc$earliest_cr_line = NULL

# some transformations and dropping of old vars
# log+1-transforming variables with range higher than 1000
loansacc$log_annual_inc = log10(loansacc$annual_inc + 1)
loansacc$annual_inc = NULL
loansacc$log_loan_amnt = log10(loansacc$loan_amnt + 1)
loansacc$loan_amnt = NULL
loansacc$log_revol_bal = log10(loansacc$revol_bal + 1)
loansacc$revol_bal = NULL
loansacc$log_tot_cur_bal = log10(loansacc$tot_cur_bal + 1)
loansacc$tot_cur_bal = NULL
loansacc$log_total_rev_hi_lim = log10(loansacc$total_rev_hi_lim + 1)
loansacc$total_rev_hi_lim = NULL
loansacc$log_avg_cur_bal = log10(loansacc$avg_cur_bal + 1)
loansacc$avg_cur_bal = NULL
loansacc$log_bc_open_to_buy = log10(loansacc$bc_open_to_buy + 1)
loansacc$bc_open_to_buy = NULL
loansacc$log_deling_amnt = log10(loansacc$delinq_amnt + 1)
loansacc$delinq_amnt = NULL
loansacc$log_tot_hi_cred_lim = log10(loansacc$tot_hi_cred_lim + 1)
loansacc$tot_hi_cred_lim = NULL
loansacc$log_total_bal_ex_mort = log10(loansacc$total_bal_ex_mort + 1)
loansacc$total_bal_ex_mort = NULL
loansacc$log_total_bc_limit = log10(loansacc$total_bc_limit + 1)
loansacc$total_bc_limit = NULL
loansacc$log_total_il_high_credit_limit = log10(loansacc$total_il_high_credit_limit + 1)
loansacc$total_il_high_credit_limit = NULL
loansacc$log_time_since_first_credit = log10(loansacc$time_since_first_credit + 1)
loansacc$time_since_first_credit = NULL

# copy the dataset for use in OLS task
loansacc.ols = loansacc

# drop issue_d and loan id
loansacc$issue_d = NULL
loansacc$id = NULL

######################################################################

#### start modelling

## set the seed
set.seed(1)

##split dataset
training = sample(dim(loansacc)[1], dim(loansacc)[1]/2)
loansacc.training = loansacc[training, ]
loansacc.testing = loansacc[-training, ]

#################### SOME TREE ALGORITHMS ####################

## create and show the tree
treegini1.loansacc = rpart(fully_paid ~., data = loansacc.training, method = "class", parms = list(split = "gini"), control = rpart.control(minsplit = 10, minbucket = 3, cp = 0.0001))
plot(treegini1.loansacc)
text(treegini1.loansacc, pretty = 0)
printcp(treegini1.loansacc)
plotcp(treegini1.loansacc)

# prune the tree
treegini1.loansacc$cptable[which.min(treegini1.loansacc$cptable[,"xerror"]),"CP"]
prune1 = treegini1.loansacc$cptable[which.min(treegini1.loansacc$cptable[,"xerror"]),"CP"]

# run tree again
treegini2.loansacc = rpart(fully_paid ~., data = loansacc.training, method = "class", parms = list(split = "gini"), control = rpart.control(minsplit = 10, minbucket = 3, cp = prune1))
plot(treegini2.loansacc)
text(treegini2.loansacc, pretty = 0)
printcp(treegini2.loansacc)
plotcp(treegini2.loansacc)

# prune the tree
treegini2.loansacc$cptable[which.min(treegini2.loansacc$cptable[,"xerror"]),"CP"]

#test the model
predicted1 = predict(treegini2.loansacc, loansacc.testing, type = "class")
confusion1 = table(loansacc.testing$fully_paid, predicted1)
print(confusion1)

# calculate rocr curve
predicted.gini.prob = predict(treegini2.loansacc, type = "prob" , loansacc.testing)[,2]
rocr.gini = prediction(predicted.gini.prob, loansacc.testing$fully_paid)
rocr.gini.perf = performance(rocr.gini, "tpr", "fpr")

# plot the rocr curve
plot(rocr.gini.perf, main="ROC Curve for gini tree", col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

# compute area under rocr curve
auc.gini = performance(rocr.gini,"auc")
auc.gini = unlist(slot(auc.gini, "y.values"))
minauc.gini = min(round(auc.gini, digits = 2))
maxauc.gini = max(round(auc.gini, digits = 2))
minauct.gini = paste(c("min(AUC) = "), minauc.gini, sep="")
maxauct.gini = paste(c("max(AUC) = "), maxauc.gini, sep="")
print(auc.gini)

#################### RANDOMFOREST ####################

# roughfix
loansacc.training.roughfix = na.roughfix(loansacc.training)
loansacc.testing.roughfix = na.roughfix(loansacc.testing)

# create forest
randforest.loansacc = randomForest(fully_paid ~., data = loansacc.training, mtry = 20, ntree = 250, importance = TRUE, na.action = na.roughfix)

# plot forest - error vs no of trees
plot(randforest.loansacc)
legend("center", colnames(randforest.loansacc$err.rate),col=1:4,cex=0.8,fill=1:4)

# plot forest - variable importance
varImpPlot(randforest.loansacc)

# make predcition for test data based on model
predicted.randforest = predict(randforest.loansacc, loansacc.testing)

#evaluate with confusion matrix
confusion.rf = table(loansacc.testing$fully_paid, predicted.randforest)
print(confusion.rf)

# calculate rocr curve
predicted.randforest.prob = predict(randforest.loansacc, type = "prob" , loansacc.testing)[,2]
rocr.randforest = prediction(predicted.randforest.prob, loansacc.testing$fully_paid)
rocr.randforest.perf = performance(rocr.randforest, "tpr", "fpr")

# plot the rocr curve
plot(rocr.randforest.perf, main="ROC Curve for Random Forest", col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

# compute area under rocr curve
auc = performance(rocr.randforest,"auc")
auc = unlist(slot(auc, "y.values"))
minauc = min(round(auc, digits = 2))
maxauc = max(round(auc, digits = 2))
minauct = paste(c("min(AUC) = "), minauc, sep="")
maxauct = paste(c("max(AUC) = "), maxauc, sep="")
print(auc)

#################### OLS ####################

# convert issue_d to year adn drop old var
loansacc.ols$issue_d = substring(loansacc.ols$issue_d, 5, 9)
loansacc.ols$year = as.factor(loansacc.ols$issue_d)
loansacc.ols$issue_d = NULL

# recode year into several dummies
year. = factor(loansacc.ols$year)
dummies_year = model.matrix(~year.)

# recode term into dummy
term. = factor(loansacc.ols$term)
dummies_term = model.matrix(~term.)

# recode emp_length into several dummies
emp_length. = factor(loansacc.ols$emp_length)
dummies_emp_length = model.matrix(~emp_length.)

# recode home ownership into several dummies
home_ownership. = factor(loansacc.ols$home_ownership)
dummies_home_ownership = model.matrix(~home_ownership.)

# recode verification status into several dummies
verification_status. = factor(loansacc.ols$verification_status)
dummies_verification_status = model.matrix(~verification_status.)

# recode purpose into several dummies
purpose. = factor(loansacc.ols$purpose)
dummies_purpose = model.matrix(~purpose.)

# recode state region into several dummies
state_region. = factor(loansacc.ols$state_region)
dummies_state_region = model.matrix(~state_region.)

mergetest = merge(loansacc.ols, dummies_year, by = 0, all = TRUE)

# run regression
fit = lm(fully_paid ~ log_annual_inc , data = loansacc.ols)

#################### SUPORT VECTOR MACHINES ####################
svm.loansacc = svm(fully_paid ~., data = loansacc.training)
svm.loansacc.pred = predict(svm.loansacc,loansacc.testing,type="class")

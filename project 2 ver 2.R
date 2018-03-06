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
library(lubridate)
library(ROCR)

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

# drop joint loans
loansacc = loansacc[!(loansacc$application_type != "Individual"),]

## some dropping of columns

# drop all colums with more than 20% NAs
loansacc = loansacc[, -which(colMeans(is.na(loansacc)) > 0.2)]

#drop some columns that are not needed because of redundancy or not possible to analyze
loansacc$id = NULL
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
loansacc$disbursement_method = NULL
loansacc$application_type = NULL

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

#move status variable to first place
loansacc = loansacc[,c(58,1:57)]

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
loansacc$issue_d_2 = loansacc$issue_d
loansacc$issue_d_2 = as.character(loansacc$issue_d_2)
loansacc$issue_d_2 = paste(loansacc$issue_d_2, "-01", sep = "")
loansacc$issue_d_2 = parse_date_time(loansacc$issue_d_2, "myd")

# convert earliest credit line to date
loansacc$earliest_cr_line = as.character(loansacc$earliest_cr_line)
loansacc$earliest_cr_line = paste(loansacc$earliest_cr_line, "-01", sep = "")
loansacc$earliest_cr_line = parse_date_time(loansacc$earliest_cr_line, "myd")

# create variable for time since earliest credit line and convert to numeric
loansacc$time_since_first_credit = loansacc$issue_d_2 - loansacc$earliest_cr_line
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

# drop issue_d
loansacc$issue_d = NULL
loansacc$issue_d_2 = NULL

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

library(party)
library(e1071)
library(leaps)

# convert issue_d to year and drop old var
loansacc.ols$issue_d = substring(loansacc.ols$issue_d, 5, 9)
loansacc.ols$year = as.factor(loansacc.ols$issue_d)
loansacc.ols$issue_d = NULL

# recode fully_paid into integer dummy
loansacc.ols$fully_paid_int = ifelse(loansacc.ols$fully_paid == "fully_paid", 1, ifelse(loansacc.ols$fully_paid == "default", 0, 0))

##split dataset
training.ols = sample(dim(loansacc.ols)[1], dim(loansacc.ols)[1]/2)
loansacc.training.ols = loansacc.ols[training.ols, ]
loansacc.testing.ols = loansacc.ols[-training.ols, ]

# run regression
fit = lm(fully_paid_int ~ log_annual_inc + year + term + emp_length + home_ownership + verification_status + purpose + state_region, data = loansacc.training.ols, x = TRUE)

#################### NEURAL NETWORKS ####################

library(devtools)

devtools::install_github("rstudio/keras")

library(keras)
library(tensorflow)

install_keras()
install_tensorflow()

#create dataset specifically for neural network part
loansacc.netw = loansacc

## some data cleaning

# some numerical transofrmations
loansacc.netw$fully_paid = as.numeric(loansacc.netw$fully_paid)
loansacc.netw$fully_paid = loansacc.netw$fully_paid - 1

loansacc.netw$term_60m[loansacc.netw$term == "60 months" ] = 1
loansacc.netw$term_60m[loansacc.netw$term != "60 months" ] = 0
loansacc.netw$term_60m = as.numeric(loansacc.netw$term_60m)
loansacc.netw$term = NULL

loansacc.netw$emp_ovr10[loansacc.netw$emp_length == "10+ years" ] = 1
loansacc.netw$emp_ovr10[loansacc.netw$emp_length != "10+ years" ] = 0
loansacc.netw$emp_ovr10 = as.numeric(loansacc.netw$emp_ovr10)
loansacc.netw$emp_length = NULL

loansacc.netw$rent[loansacc.netw$home_ownership == "RENT" ] = 1
loansacc.netw$rent[loansacc.netw$home_ownership != "RENT" ] = 0
loansacc.netw$rent = as.numeric(loansacc.netw$rent)
loansacc.netw$home_ownership = NULL

loansacc.netw$verified[loansacc.netw$verification_status != "Not Verified" ] = 1
loansacc.netw$verified[loansacc.netw$verification_status == "Not Verified" ] = 0
loansacc.netw$verified = as.numeric(loansacc.netw$verified)
loansacc.netw$verification_status = NULL

#some dropping of columns
loansacc.netw$state_region = NULL
loansacc.netw$purpose = NULL # we would like to keep this by recoding it but there isn't time

# create matrix version of loansacc dataset and remove column names
loansacc.netw = as.matrix(loansacc.netw)

# normalize
loansacc.netw[,2:52] = normalize(loansacc.netw[,2:52])

#create training and test data
samp = sample(2, nrow(loansacc.netw), replace=TRUE, prob = c(0.5, 0.5))
loansacc.netw.training = loansacc.netw[samp == 1, 1:55]
loansacc.netw.testing = loansacc.netw[samp == 2, 1:55]
loansacc.netw.training.target = loansacc.netw[samp == 1, 1]
loansacc.netw.testing.target = loansacc.netw[samp == 2, 1]

# "One hot encoding", make matrix categorical
loansacc.netw.training.target.labels = to_categorical(loansacc.netw.training.target)
loansacc.netw.testing.target.labels = to_categorical(loansacc.netw.testing.target)

# construct the model
model.netw = keras_model_sequential()
model.netw %>% layer_dense(units = 50, activation = 'relu', input_shape = c(55)) %>% layer_dense(units = 2, activation = 'softmax')

# compile and fit model
model.netw %>% compile(loss = 'binary_crossentropy', optimizer = 'adam', metrics = 'accuracy')

# fit the model, store the model
history = model.netw %>% fit(loansacc.netw.training, loansacc.netw.training.target.labels, epochs = 20, batch_size = 50, validation_split = 0,2)

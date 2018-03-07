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
library(party)
library(e1071)
library(leaps)
library(MASS)
library(ggplot2)
library(rJava)
library(xlsxjars)
library(xlsx)

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

# drop some columns with only one unique value
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

# plot1
jpeg('plot1 - gini tree 1.jpg')
plot(treegini1.loansacc)
dev.off()
plot(treegini1.loansacc)

# table1 ################ NOT DONE YET ##################
table1 = printcp(treegini1.loansacc)


# plot2
jpeg('plot2 - gini tree 1.jpg')
plotcp(treegini1.loansacc)
dev.off()
plotcp(treegini1.loansacc)


# prune the tree
treegini1.loansacc$cptable[which.min(treegini1.loansacc$cptable[,"xerror"]),"CP"]
prune1 = treegini1.loansacc$cptable[which.min(treegini1.loansacc$cptable[,"xerror"]),"CP"]

# run tree again
treegini2.loansacc = rpart(fully_paid ~., data = loansacc.training, method = "class", parms = list(split = "gini"), control = rpart.control(minsplit = 10, minbucket = 3, cp = prune1))


# plot3
jpeg('plot3 - gini tree 2.jpg')
plot(treegini2.loansacc)
dev.off()
plot(treegini2.loansacc)

# table2 ################ NOT DONE YET ##################
table2 = printcp(treegini2.loansacc)

# plot4
jpeg('plot4 - gini tree 2.jpg')
plotcp(treegini2.loansacc)
dev.off()
plotcp(treegini2.loansacc)

# prune the tree
treegini2.loansacc$cptable[which.min(treegini2.loansacc$cptable[,"xerror"]),"CP"]

#test the model
predicted1 = predict(treegini2.loansacc, loansacc.testing, type = "class")
confusion1 = table(loansacc.testing$fully_paid, predicted1)

# table2 ################ NOT DONE YET ##################
print(confusion1)

# calculate rocr curve
predicted.gini.prob = predict(treegini2.loansacc, type = "prob" , loansacc.testing)[,2]
rocr.gini = prediction(predicted.gini.prob, loansacc.testing$fully_paid)
rocr.gini.perf = performance(rocr.gini, "tpr", "fpr")

# plot5
jpeg('plot 5 - gini tree 2 roc.jpg')
plot(rocr.gini.perf, main="ROC Curve for gini tree", col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")
dev.off()
plot(rocr.gini.perf, main="ROC Curve for gini tree", col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

# compute area under rocr curve
auc.gini = performance(rocr.gini,"auc")
auc.gini = unlist(slot(auc.gini, "y.values"))
minauc.gini = min(round(auc.gini, digits = 2))
maxauc.gini = max(round(auc.gini, digits = 2))
minauct.gini = paste(c("min(AUC) = "), minauc.gini, sep="")
maxauct.gini = paste(c("max(AUC) = "), maxauc.gini, sep="")

####### IMPORTANT VALUE ####### 
print(auc.gini)

#################### RANDOMFOREST ####################

# roughfix
loansacc.training.roughfix = na.roughfix(loansacc.training)
loansacc.testing.roughfix = na.roughfix(loansacc.testing)

# create forest
randforest.loansacc = randomForest(fully_paid ~., data = loansacc.training, mtry = 25, ntree = 400, importance = TRUE, na.action = na.roughfix)

# plot6
jpeg('plot 6 - random forest error v. no trees.jpg')
plot(randforest.loansacc)
legend("center", colnames(randforest.loansacc$err.rate),col=1:4,cex=0.8,fill=1:4)
dev.off()
plot(randforest.loansacc)
legend("center", colnames(randforest.loansacc$err.rate),col=1:4,cex=0.8,fill=1:4)

# plot 7
jpeg('plot 7 - random forest variable importance.jpg')
varImpPlot(randforest.loansacc)
dev.off()
varImpPlot(randforest.loansacc)

# make predcition for test data based on model
predicted.randforest = predict(randforest.loansacc, loansacc.testing)

#evaluate with confusion matrix
confusion.rf = table(loansacc.testing$fully_paid, predicted.randforest)

# table3 ################ NOT DONE YET ##################
print(confusion.rf)

# calculate rocr curve
predicted.randforest.prob = predict(randforest.loansacc, type = "prob" , loansacc.testing)[,2]
rocr.randforest = prediction(predicted.randforest.prob, loansacc.testing$fully_paid)
rocr.randforest.perf = performance(rocr.randforest, "tpr", "fpr")

# # plot 8
jpeg('plot 8 - random forest roc.jpg')
plot(rocr.randforest.perf, main="ROC Curve for Random Forest", col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")
dev.off()
plot(rocr.randforest.perf, main="ROC Curve for Random Forest", col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

# compute area under rocr curve
auc = performance(rocr.randforest,"auc")
auc = unlist(slot(auc, "y.values"))
minauc = min(round(auc, digits = 2))
maxauc = max(round(auc, digits = 2))
minauct = paste(c("min(AUC) = "), minauc, sep="")
maxauct = paste(c("max(AUC) = "), maxauc, sep="")

####### IMPORTANT VALUE #######
print(auc)

#################### OLS ####################

# convert issue_d to year and drop old var
loansacc.ols$issue_d = substring(loansacc.ols$issue_d, 5, 9)
loansacc.ols$year = as.factor(loansacc.ols$issue_d)
loansacc.ols$issue_d = NULL

# recode fully_paid into integer dummy
loansacc.ols$fully_paid_int = ifelse(loansacc.ols$fully_paid == "fully_paid", 1, ifelse(loansacc.ols$fully_paid == "default", 0, 0))

#split dataset
training.ols = sample(dim(loansacc.ols)[1], dim(loansacc.ols)[1]/2)
loansacc.training.ols = loansacc.ols[training.ols, ]
loansacc.testing.ols = loansacc.ols[-training.ols, ]

# run regression
fit1 = lm(fully_paid_int ~ log_annual_inc + year + term + emp_length + home_ownership + purpose + verification_status + state_region, data = loansacc.training.ols, x = TRUE)

# recode home_ownership into rent-dummy-variable
loansacc.ols$rent[loansacc.ols$home_ownership == "RENT" ] = "rent"
loansacc.ols$rent[loansacc.ols$home_ownership != "RENT" ] = "not_rent"
loansacc.ols$rent = as.factor(loansacc.ols$rent)
loansacc.ols$home_ownership = NULL

#split the dataset again
training.ols = sample(dim(loansacc.ols)[1], dim(loansacc.ols)[1]/2)
loansacc.training.ols = loansacc.ols[training.ols, ]
loansacc.testing.ols = loansacc.ols[-training.ols, ]

# run regression again
fit2 = lm(fully_paid_int ~ log_annual_inc + year + term + emp_length + verification_status + purpose + state_region + rent, data = loansacc.training.ols, x = TRUE)

# make predictions based on test data and omit NAs
pred = predict(fit2, loansacc.testing.ols)
pred = na.omit(pred)

# trim the testing data to be of same number of rows as predicted data
g = nrow(loansacc.testing.ols) - length(pred)
g = as.numeric(g)
loansacc.testing.ols = loansacc.testing.ols[-sample(1:nrow(loansacc.testing.ols), g), ]

#calculate rmse
sqrt(mean((pred - loansacc.testing.ols$fully_paid_int)^2))

#################### SUPPORT VECTOR MACHINES ####################

#create data frame for svm part
loansacc.svm = loansacc

## some data cleaning

# normalize numeric variables
ind1 = sapply(loansacc.svm, is.numeric)
loansacc.svm[ind1] = lapply(loansacc.svm[ind1], scale)

# some reductions in factor levels
loansacc.svm$emp_ovr10[loansacc.svm$emp_length == "10+ years" ] = "10+"
loansacc.svm$emp_ovr10[loansacc.svm$emp_length != "10+ years" ] = "10-"
loansacc.svm$emp_ovr10 = as.factor(loansacc.svm$emp_ovr10)
loansacc.svm$emp_length = NULL

loansacc.svm$rent[loansacc.svm$home_ownership == "RENT" ] = "rent"
loansacc.svm$rent[loansacc.svm$home_ownership != "RENT" ] = "not_rent"
loansacc.svm$rent = as.factor(loansacc.svm$rent)
loansacc.svm$home_ownership = NULL

loansacc.svm$verified[loansacc.svm$verification_status != "Not Verified" ] = "verified"
loansacc.svm$verified[loansacc.svm$verification_status == "Not Verified" ] = "not_verified"
loansacc.svm$verified = as.factor(loansacc.svm$verified)
loansacc.svm$verification_status = NULL

#draw sample
loansacc.svm.sample = loansacc.svm[sample(nrow(loansacc), 70000),]

#omit NAs
loansacc.svm.sample = na.omit(loansacc.svm.sample)

# split data
svm.training = sample(dim(loansacc.svm.sample)[1], dim(loansacc.svm.sample)[1]/2)
loansacc.svm.training = loansacc.svm.sample[svm.training, ]
loansacc.svm.testing = loansacc.svm.sample[-svm.training, ]

#remove columns with only 1 unique values
keep1 = apply(loansacc.svm.training[1:57], 1, function(x) length(unique(x[!is.na(x)])) != 1)
loansacc.svm.training = loansacc.svm.training[keep1, ]
keep2 = apply(loansacc.svm.testing[1:57], 1, function(x) length(unique(x[!is.na(x)])) != 1)
loansacc.svm.testing = loansacc.svm.testing[keep2, ]

#run model
svm.loansacc = svm(fully_paid ~., data = loansacc.svm.training)

# make predictions
pred.svm = predict(svm.loansacc, loansacc.svm.testing)

#evaluate with confusion matrix
confusion.svm = table(loansacc.svm.testing$fully_paid, pred.svm)
print(confusion.svm)

# calculate rocr curve
predicted.svm = predict(svm.loansacc, type = "prob", loansacc.svm.testing)
predicted.svm = as.data.frame(predicted.svm)
predicted.svm$predicted.svm = as.numeric(predicted.svm$predicted.svm)
rocr.svm = prediction(predicted.svm, loansacc.svm.testing$fully_paid)
rocr.svm.perf = performance(rocr.svm, "tpr", "fpr")

# plot the rocr curve
plot(rocr.svm.perf, main="ROC Curve for SVM", col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

# compute area under rocr curve
auc.svm = performance(rocr.svm,"auc")
auc.svm = unlist(slot(auc.svm, "y.values"))
minauc.svm = min(round(auc.svm, digits = 2))
maxauc.svm = max(round(auc.svm, digits = 2))
minauct.svm = paste(c("min(AUC) = "), minauc.svm, sep="")
maxauct.svm = paste(c("max(AUC) = "), maxauc.svm, sep="")
print(auc.svm)

#################### SUPPORT VECTOR MACHINES V2 - NOT IMPLEMENTED DUE TO COMPUTATION TIME ####################
# 
# svm2.loansacc = loansacc
# 
# ## some data cleaning
# 
# # normalize numeric variables
# ind2 = sapply(svm2.loansacc, is.numeric)
# svm2.loansacc[ind2] = lapply(svm2.loansacc[ind2], scale)
# 
# # some reductions in factor levels
# svm2.loansacc$emp_ovr10[svm2.loansacc$emp_length == "10+ years" ] = "10+"
# svm2.loansacc$emp_ovr10[svm2.loansacc$emp_length != "10+ years" ] = "10-"
# svm2.loansacc$emp_ovr10 = as.factor(svm2.loansacc$emp_ovr10)
# svm2.loansacc$emp_length = NULL
# 
# svm2.loansacc$rent[svm2.loansacc$home_ownership == "RENT" ] = "rent"
# svm2.loansacc$rent[svm2.loansacc$home_ownership != "RENT" ] = "not_rent"
# svm2.loansacc$rent = as.factor(svm2.loansacc$rent)
# svm2.loansacc$home_ownership = NULL
# 
# svm2.loansacc$verified[svm2.loansacc$verification_status != "Not Verified" ] = "verified"
# svm2.loansacc$verified[svm2.loansacc$verification_status == "Not Verified" ] = "not_verified"
# svm2.loansacc$verified = as.factor(svm2.loansacc$verified)
# svm2.loansacc$verification_status = NULL
# 
# #split dataset
# svm2.training = sample(dim(svm2.loansacc)[1], dim(svm2.loansacc)[1]/2)
# svm2.loansacc.training = svm2.loansacc[svm2.training, ]
# svm2.loansacc.testing = svm2.loansacc[-svm2.training, ]
# 
# # run the model
# svm2.loansacc = svm(fully_paid ~., data = svm2.loansacc.training)
# 
# # make predictions
# pred.svm2 = predict(svm2.loansacc, svm2.loansacc.testing)
# 
# # trim the testing data to be of same number of rows as predicted data
# h = nrow(svm2.loansacc.testing) - length(pred.svm2)
# h = as.numeric(h)
# svm2.loansacc.testing = svm2.loansacc.testing[-sample(1:nrow(svm2.loansacc.testing), h), ]
# 
# #evaluate with confusion matrix
# confusion.svm2 = table(svm2.loansacc.testing$fully_paid, pred.svm2)
# print(confusion.svm2)
# 
# ## calculate rocr curve
# predicted.svm2 = predict(svm2.loansacc, type = "prob", svm2.loansacc.testing)
# predicted.svm2 = as.data.frame(predicted.svm2)
# 
# # trim the testing data to be of same number of rows as predicted data
# j = nrow(svm2.loansacc.testing) - nrow(predicted.svm2)
# j = as.numeric(j)
# svm2.loansacc.testing = svm2.loansacc.testing[-sample(1:nrow(svm2.loansacc.testing), j), ]
# 
# predicted.svm2$predicted.svm2 = as.numeric(predicted.svm2$predicted.svm2)
# rocr.svm2 = prediction(predicted.svm2, svm2.loansacc.testing$fully_paid)
# rocr.svm2.perf = performance(rocr.svm2, "tpr", "fpr")
# 
# # plot the rocr curve
# plot(rocr.svm2.perf, main="ROC Curve for SVM", col=2, lwd=2)
# abline(a=0, b=1, lwd=2, lty=2, col="gray")
# 
# # compute area under rocr curve
# auc.svm2 = performance(rocr.svm2,"auc")
# auc.svm2 = unlist(slot(auc.svm2, "y.values"))
# minauc.svm2 = min(round(auc.svm2, digits = 2))
# maxauc.svm2 = max(round(auc.svm2, digits = 2))
# minauct.svm2 = paste(c("min(AUC) = "), minauc.svm2, sep="")
# maxauct.svm2 = paste(c("max(AUC) = "), maxauc.svm2, sep="")
# print(auc.svm2)

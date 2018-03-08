#### Project 2
#### Authors: Erik Jonsson (23621), BrunoWisniewski (23653), Jingcheng Zhao (23048), Adrian Bjurefalk (23519)

######################################################################

#### set up the assignment

#get working directory
wd = getwd()

#set working directory
setwd(wd)

#install relevant packages
install.packages("tree")
install.packages("randomForest")
install.packages("lubridate")
install.packages("ROCR")
install.packages("party")
install.packages("e1071")
install.packages("leaps")
install.packages("MASS")
install.packages("ggplot2")
install.packages("caret")

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
library(caret)

## import data with accepted loans
loansacc = read.csv("loans_accepted.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = TRUE) #import data with accepted loans

######################################################################

#### clean the data

# some recoding into NA
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

# drop columns that contain ex-post data, "leaking" data from future
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

# drop issue_d
loansacc$issue_d = NULL
loansacc$issue_d_2 = NULL

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

#################### RANDOMFOREST - BEST MODEL ####################

# set the seed
set.seed(1)

# split dataset
training = sample(dim(loansacc)[1], dim(loansacc)[1]/2)
loansacc.training = loansacc[training, ]
loansacc.testing = loansacc[-training, ]

# roughfix, how to treat NAs
loansacc.training.roughfix = na.roughfix(loansacc.training)
loansacc.testing.roughfix = na.roughfix(loansacc.testing)

# create the forest
randforest.loansacc = randomForest(fully_paid ~., data = loansacc.training, mtry = 25, ntree = 400, importance = TRUE, na.action = na.roughfix)

# make predcitions and produc confusion matrix
predicted.randforest = predict(randforest.loansacc, loansacc.testing)
confusion = confusionMatrix(predicted.randforest, loansacc.testing$fully_paid)
print(confusion)

# produce the rocr curve
predicted.randforest.prob = predict(randforest.loansacc, type = "prob" , loansacc.testing)[,2]
rocr.randforest = prediction(predicted.randforest.prob, loansacc.testing$fully_paid)
rocr.randforest.perf = performance(rocr.randforest, "tpr", "fpr")

# plot the rocr curve
plot(rocr.randforest.perf, main="ROC Curve for Random Forest", col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

# compute area under rocr curve and print area under curve
auc = performance(rocr.randforest,"auc")
auc = unlist(slot(auc, "y.values"))
minauc = min(round(auc, digits = 2))
maxauc = max(round(auc, digits = 2))
minauct = paste(c("min(AUC) = "), minauc, sep="")
maxauct = paste(c("max(AUC) = "), maxauc, sep="")
print(auc)

#################### RANDOMFOREST - SUMMARY ####################

#### OUR RANDOM FOREST HAS ACCURACY OF:
print(confusion$accuracy)

#### OUR RANDOM FOREST HAS AUC OF:
print(auc)

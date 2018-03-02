#### Project 2
#### Author: Erik Jonsson (23621)

######################################################################

#### set up the assignment

#get working directory
getwd()

#set working directory
setwd("C:/Users/erikj/Documents/764 local files/project 2") 

#get relevant libraries
library(tree)
library(randomForest)
library(rpart)
library(party)
library(lubridate)

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

#drop earliest credit line and issue_d
loansacc$earliest_cr_line = NULL
loansacc$issue_d = NULL

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

#########

## create and show the tree
treeinformation1.loansacc = rpart(fully_paid ~., data = loansacc.training, method = "class", parms = list(split = "information"), control = rpart.control(minsplit = 10, minbucket = 3, cp = 0.0001))
plot(treeinformation1.loansacc)
text(treeinformation1.loansacc, pretty = 0)
printcp(treeinformation1.loansacc)
plotcp(treeinformation1.loansacc)

# prune the tree
treeinformation1.loansacc$cptable[which.min(treeinformation1.loansacc$cptable[,"xerror"]),"CP"]
prune2 = treeinformation1.loansacc$cptable[which.min(treeinformation1.loansacc$cptable[,"xerror"]),"CP"]

# runt tree again
treeinformation2.loansacc = rpart(fully_paid ~., data = loansacc.training, method = "class", parms = list(split = "information"), control = rpart.control(minsplit = 10, minbucket = 3, cp = prune1))
plot(treeinformation2.loansacc)
text(treeinformation2.loansacc, pretty = 0)
printcp(treeinformation2.loansacc)
plotcp(treeinformation2.loansacc)

# prune the tree
treeinformation2.loansacc$cptable[which.min(treeinformation2.loansacc$cptable[,"xerror"]),"CP"]

#test the model
predicted2 = predict(treeinformation2.loansacc, loansacc.testing, type = "class")
confusion2 = table(loansacc.testing$fully_paid, predicted2)
print(confusion2)

#################### RANDOMFOREST ####################


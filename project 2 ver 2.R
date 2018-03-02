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

######################################################################

#### start modelling

## set the seed
set.seed(1)

## split the dataset
training = sample(dim(loansacc)[1], dim(loansacc)[1]/2)
loansacc.training = loansacc[training, ]
loansacc.testing = loansacc[-training, ]

#################### SOME TREE ALGORITHMS ####################

# ## create and show the tree
# tree1.loansacc = tree(fully_paid ~ ., data = loansacc.training, method = "class")
# summary(tree1.loansacc)
# plot(tree1.loansacc)
# text(tree1.loansacc, pretty = 0)

## create and show the tree
tree2.loansacc = rpart(fully_paid ~ ., data = loansacc.training, method = "class")
summary(tree2.loansacc)
plot(tree2.loansacc)
text(tree2.loansacc, pretty = 0)

#################### END OF SOME TREE ALGORITHMS ####################

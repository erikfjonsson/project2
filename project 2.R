#### Project 2
#### Author: Erik Jonsson (23621)

######################################################################

## set up the assignment

#get working directory
getwd()

#set working directory
setwd("C:/Users/erikj/Documents/764 local files/project 2") 

#get relevant libraries
library(tree)
library(randomForest)
library(rpart)
library(party)

######################################################################

## import data with accepted loans
loansacc = read.csv("loans_accepted.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = TRUE) #import data with accepted loans

######################################################################

## clean the data

# drop observations where loan_status is unknown
loansacc = loansacc[!(loansacc$loan_status == ""),]

# drop observations from 2007
loansacc = loansacc[!(substring(loansacc$issue_d, 5, 9) == "2007"),]

# drop joint loans
loansacc = loansacc[!(loansacc$application_type != "Individual"),]

# drop all colums with more than 32 factor levelss
loansacc = loansacc[sapply(loansacc, nlevels) <= 32]

# drop all colums with more than 10% NAs
loansacc = loansacc[, -which(colMeans(is.na(loansacc)) > 0.2)]

# drop some columns
loansacc$funded_amnt = NULL
loansacc$funded_amnt_inv = NULL
loansacc$int_rate = NULL
loansacc$grade = NULL
loansacc$installment = NULL
loansacc$pymnt_plan = NULL
loansacc$initial_list_status = NULL
loansacc$out_prncp = NULL
loansacc$out_prncp_inv = NULL
loansacc$total_pymnt = NULL
loansacc$total_pymnt_inv = NULL
loansacc$total_rec_prncp = NULL
loansacc$total_rec_int = NULL
loansacc$total_rec_late_fee = NULL
# loansacc$total_acc = NULL
loansacc$recoveries = NULL
loansacc$collection_recovery_fee = NULL
loansacc$last_pymnt_amnt = NULL
loansacc$last_fico_range_high = NULL
loansacc$last_fico_range_low = NULL
loansacc$collections_12_mths_ex_med = NULL
loansacc$policy_code = NULL
loansacc$application_type = NULL
# loansacc$acc_now_delinq = NULL
loansacc$application_type = NULL
loansacc$verification_status_joint = NULL
# loansacc$bc_open_to_buy = NULL
# loansacc$bc_util = NULL
loansacc$chargeoff_within_12_mths = NULL
loansacc$delinq_amnt = NULL
# loansacc$mo_sin_old_il_acct = NULL
# loansacc$mo_sin_old_rev_tl_op = NULL
# loansacc$mo_sin_rcnt_rev_tl_op = NULL
# loansacc$mo_sin_rcnt_tl = NULL
# loansacc$mths_since_recent_bc = NULL
# loansacc$mths_since_recent_inq = NULL
# loansacc$mths_since_recent_bc = NULL
# loansacc$mths_since_recent_inq = NULL
loansacc$num_tl_120dpd_2m = NULL
loansacc$num_tl_30dpd = NULL
loansacc$hardship_flag = NULL
loansacc$hardship_type = NULL
loansacc$hardship_reason = NULL
loansacc$hardship_status = NULL
loansacc$hardship_start_date = NULL
loansacc$hardship_end_date = NULL
loansacc$hardship_loan_status = NULL
loansacc$payment_plan_start_date = NULL
loansacc$settlement_status = NULL
loansacc$debt_settlement_flag = NULL

loansacc$term = NULL

# create variable for average of the two fico ranges
loansacc$fico = (loansacc$fico_range_high + loansacc$fico_range_low)/2

#drop old ficos
loansacc$fico_range_high = NULL
loansacc$fico_range_low = NULL

# # create recoded version of the loan_status variable
loansacc$fully_paid[loansacc$loan_status == "Default" | loansacc$loan_status == "Charged Off" | loansacc$loan_status == "Does not meet the credit policy. Status:Charged Off"] = "default"
loansacc$fully_paid[loansacc$loan_status == "Fully Paid" | loansacc$loan_status == "Does not meet the credit policy. Status:Fully Paid"] = "fully_paid"
loansacc$fully_paid[loansacc$loan_status == "Late (16-30 days)" | loansacc$loan_status == "Late (31-120 days)" | loansacc$loan_status == "In Grace Period" | loansacc$loan_status == "Current"] = "ongoing"

# #drop ongoing loans
loansacc = loansacc[!(loansacc$fully_paid == "ongoing"),]

# convert the recoded variable to factor
loansacc$fully_paid = as.factor(loansacc$fully_paid)

# # drop old loan_status variable
loansacc$loan_status = NULL


######################################################################

## set the seed
set.seed(1)


## split the dataset
training = sample(dim(loansacc)[1], dim(loansacc)[1]/2)
loansacc.training = loansacc[training, ]
loansacc.testing = loansacc[-training, ]

#################### SOME TREE ALGORITHMS ####################

## create and show the tree
tree1.loansacc = tree(loan_status ~ ., data = loansacc.training, method = "class")
summary(tree1.loansacc)
plot(tree1.loansacc)
text(tree1.loansacc, pretty = 0)

# ## create and show the tree
# tree2.loansacc = rpart(fully_paid ~ ., data = loansacc.training, method = "class")
# summary(tree2.loansacc)
# plot(tree2.loansacc)
# text(tree2.loansacc, pretty = 0)
# 
# ## create and show the tree
# tree3.loansacc = ctree(fully_paid ~ ., data = loansacc.training,)
# summary(tree3.loansacc)
# plot(tree3.loansacc)

#################### END OF SOME TREE ALGORITHMS ####################

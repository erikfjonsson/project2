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
loansacc$total_acc = NULL
loansacc$recoveries = NULL
loansacc$collection_recovery_fee = NULL
loansacc$last_pymnt_amnt = NULL
loansacc$last_fico_range_high = NULL
loansacc$last_fico_range_low = NULL
loansacc$collections_12_mths_ex_med = NULL
loansacc$policy_code = NULL
loansacc$application_type = NULL
loansacc$acc_now_delinq = NULL
loansacc$application_type = NULL
loansacc$verification_status_joint = NULL
loansacc$bc_open_to_buy = NULL
loansacc$bc_util = NULL
loansacc$chargeoff_within_12_mths = NULL
loansacc$delinq_amnt = NULL
# loansacc$mo_sin_old_il_acct = NULL
# loansacc$mo_sin_old_rev_tl_op = NULL
# loansacc$mo_sin_rcnt_rev_tl_op = NULL
# loansacc$mo_sin_rcnt_tl = NULL
# loansacc$mths_since_recent_bc = NULL
# loansacc$mths_since_recent_inq = NULL
loansacc$num_accts_ever_120_pd
loansacc$num_actv_bc_tl


# DTI hur gör vi?
# FICO ny variable

######################################################################

## set the seed
set.seed(1)


## split the data set

# split the dataset
training = sample(dim(loansacc)[1], dim(loansacc)[1]/2) #process for splitting the dataset

loansacc.training = loansacc[training, ] #creates the training dataset

loansacc.testing = loansacc[-training, ] #creates the testing dataset


# create and plot the tree
tree.loansacc = tree(loan_status ~ ., data = loansacc.training) #creates a tree

plot(tree.loansacc) # plot the tree

text(tree.loansacc, pretty = 0) # formatting the graph

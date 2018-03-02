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

## some obvious removals

# drop observations where loan_status is unknown
loansacc = loansacc[!(loansacc$loan_status == ""),]

# # drop observations from 2007
# loansacc = loansacc[!(substring(loansacc$issue_d, 5, 9) == "2007"),]

# drop joint loans
loansacc = loansacc[!(loansacc$application_type != "Individual"),]

# drop all colums with more than 20% NAs
loansacc = loansacc[, -which(colMeans(is.na(loansacc)) > 0.2)]

## some recoding into NA

loansacc$last_fico_range_high[loansacc$last_fico_range_high == "0"] = NA
loansacc$last_fico_range_low[loansacc$last_fico_range_low == "0"] = NA
loansacc$verification_status_joint[loansacc$verification_status_joint == ""] = NA

## again drop all colums with more than 20% NAs

loansacc = loansacc[, -which(colMeans(is.na(loansacc)) > 0.2)]
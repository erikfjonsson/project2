#### Project 2
#### Author: Erik Jonsson (23621)

## set up the files

#get working directory
getwd()

#set working directory
setwd("C:/Users/erikj/Documents/764 local files/project 2") 

#get relevant libraries
library(tree)
library(randomForest)

#import data with accepted loans
loansacc = read.csv("loans_accepted.csv", header=TRUE, sep=",", dec="." )


## clean the data

# remove observations with unknown year
loansacc = loansacc[!(loansacc$issue_d == ""),]

# remove observations from 2007 due to changed credit model
loansacc = loansacc[!(substring(loansacc$issue_d, 5, 9) == "2007"),]

# change issue_d column to display only year instead of year and month and rename it, should we do this?
# loansacc$issue_d = substring(loansacc$issue_d, 5, 9)
# colnames(loansacc)[16] = "year"

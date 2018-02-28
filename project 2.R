#### Project 2
#### Author: Erik Jonsson (23621)

## set up the files

getwd() #get working directory
setwd("C:/Users/erikj/Documents/764 local files/project 2") #set working directory

loansacc_original = read.csv("loans_accepted.csv", header=TRUE, sep=",", dec="." ) #import data with accepted loans


## create data frame

loansacc = data.frame(loansacc_original, stringsAsFactors = TRUE)


## clean the data

# change issue_d column to display only year instead of year and month, should we do this?
# loansacc$issue_d = substring(loansacc$issue_d, 5, 9)

# remove observations with unknown year
loansacc = loansacc[!(loansacc$issue_d == ""),]

# remove observations from 2007 due to changed credit model
loansacc = loansacc[!(substring(loansacc$issue_d, 5, 9) == "2007"),]
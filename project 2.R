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
library(DBI)
library(RSQLite)

######################################################################

## import data with accepted loans

# connect the DB
db = dbConnect(SQLite(), dbname="loans.sqlite")
tables = dbListTables(db)[1]

# exclude sqlite sequence 
tables = tables[tables != "sqlite_sequence"]
lDataFrames = vector("list", length=length(tables))

# load table with loans accepted
lDataFrames[[1]] = dbGetQuery(conn=db, statement=paste("SELECT * FROM '", tables[[1]], "'", sep=""))

# create dataframe
loansacc = lDataFrames[[1]]

#disconnect
dbDisconnect(db)

#remove
rm(db, lDataFrames)

######################################################################

## further clean the data

########## CODE NO LONGER NEEDED SINCE WE INSTEAD IMPORTED FROM THE SQL-DB ##########

# # import data with accepted loans
# loansacc = read.csv("loans_accepted.csv", header=TRUE, sep=",", dec="." )

# # remove observations with unknown year
# loansacc = loansacc[!(loansacc$issue_d == ""),]
# 
# # remove observations from 2007 due to changed credit model
# loansacc = loansacc[!(substring(loansacc$issue_d, 5, 9) == "2007"),]
# 
# # change issue_d column to display only year instead of year and month and rename it, should we do this?
# # loansacc$issue_d = substring(loansacc$issue_d, 5, 9)
# # colnames(loansacc)[16] = "year
# 
# # replace zeros with NA for two columns
# loansacc[loansacc$last_fico_range_high == "0", 51] = NA
# loansacc[loansacc$last_fico_range_low == "0", 52] = NA

########## END OF CODE NO LONGER NEEDED SINCE WE INSTEAD IMPORTED FROM THE SQL-DB ##########

# remove columns with many NAs
loansacc[ , colSums(is.na(loansacc)) > 1000]

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

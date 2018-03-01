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

db = dbConnect(SQLite(), dbname="loans.sqlite")
tables = dbListTables(db)

# exclude sqlite sequence
tables = tables[tables != "sqlite_sequence"]

lDataFrames = vector("list", length=length(tables))

# create a data frame for table 3
lDataFrames[[3]] = dbGetQuery(conn=db, statement=paste("SELECT * FROM '", tables[[3]], "'", sep=""))

# create dataframe
loansacc = lDataFrames[[3]]

#disconnect
dbDisconnect(db)

#remove
rm(db, lDataFrames, tables)

## clean the data further

# remove unit measure from term column
loansacc$term = substring(loansacc$term, 2, 3)

# convert som strings to numeric
loansacc$id = as.numeric(loansacc$id)
loansacc$term = as.numeric(loansacc$term)

#convert some strings

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

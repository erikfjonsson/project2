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

# drop all colums with more than 32 factor levels
loansacc = loansacc[sapply(loansacc, nlevels) <= 32]

# convert term to numeric, SHOULD WE REALLY DO THIS, PERHAPS BETTER AS FACTOR LEVELS?
loansacc$term = substring(loansacc$term, 2, 3)
loansacc$term = as.numeric(loansacc$term)

# drop all colums with more than 10% NAs, IS THIS CORRECT?
loansacc = loansacc[, -which(colMeans(is.na(loansacc)) > 0.1)]

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
rm(list=ls())
setwd("/home/ramela/Documents/Master/MVA/ML_project/")

stock_names <- c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "XOM", "GE", "GS", "HD", "INTC", "IBM", "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UNH", "UTX", "VZ", "V", "WMT", "DIS")
window <- 7
window_str <- sprintf("%d",window)
stock <- 1

curr_dataset <- read.csv(paste(stock_names[stock], "_", window_str, ".csv", sep=""), row.names = 1, sep = ";", header = TRUE)
curr_dataset$IncrementDayCategorical <- as.factor(curr_dataset$IncrementDayCategorical)
curr_dataset$IncrementPreCategorical <- as.factor(curr_dataset$IncrementPreCategorical)

###########################################
# Partitioning 
###########################################

#70% train  30% test
n <- nrow(curr_dataset)   

set.seed(250)
amount_train <- 0.7
amount_test <- 0.2
amount_val <- 0.1

ntrain <- round(amount_train * n)
ntest <- round(amount_test * n)
nval <- n - ntrain - ntest

all_ind <- 1:n
train_ind <- sample(all_ind, ntrain)
train_data <- curr_dataset[train_ind,]
all_ind1 <- all_ind[-train_ind]
test_ind <- sample(all_ind1, ntest)
test_data <- curr_dataset[test_ind,]
val_ind <- all_ind[-c(test_ind, train_ind)]
val_data <- curr_dataset[val_ind,]

#merging all stocks in 1 list of dataframes
d<-list(d1)
#d<-list(d1,d2,d3,d4)


# Levels of the training and testing set are not the same, it is not possible to predict the exact change in... 

library(randomForest)

y <- curr_dataset$IncrementDayCategorical
x <- curr_dataset$[, -which(names(curr_dataset) %in% c("IncrementDayCategorical", "IncrementPerCategorical", ))]

rf<-list()
for (i in 1:s) {
  set.seed(100)
  rf[[i]]<- randomForest(trainset_y_cat[[i]]~ ., data=trainset_x[[i]], mtry=3, importance=TRUE, xtest=testset_x[[i]], ytest=testset_y_cat[[i]])
  print(rf[[i]])
  rf[[i]]$forest
  #varImpPlot(rf)
}


summary(trainset_y_cat[[1]])
summary(testset_y_cat[[1]])
str(trainset_y_cat[[1]])

trainset_y_cat[[1]]<-droplevels(trainset_y_cat[[1]])
testset_y_cat[[1]]<-droplevels(testset_y_cat[[1]])
rf[[1]]<- randomForest(trainset_y_cat[[1]]~ ., data=trainset_x[[1]], mtry=3, importance=TRUE, xtest=testset_x[[1]], ytest=testset_y_cat[[1]])
print(rf[[1]])
rf[[1]]$forest
varImpPlot(rf[[1]])
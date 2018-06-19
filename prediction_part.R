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
amount_train <- 0.65
amount_val <- 0.05
amount_test <- 0.3

ntest <- round(amount_test * n)
ntrain <- round(amount_train * n)
nval <- n - ntest - ntrain

all_ind <- 1:n
test_data <- curr_dataset[(n - ntest + 1):n,]

train_val_data <- curr_dataset[1:(n-ntest),]
train_val_ind <- 1:nrow(train_val_data)
train_ind <- sample(train_val_ind, ntrain)

train_data <- train_val_data[train_ind,]
val_data <- train_val_data[-train_ind,]

print(n)
print(ntest + ntrain + nval)
print(dim(train_val_data))
print(ntrain + nval)
print(ntest)
print(dim(test_data))
print(ntrain)
print(dim(train_data))
print(nval)
print(dim(val_data))

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
rm(list=ls())

###########################################
# Reading Data
###########################################
filenames <- c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "XOM", "GE", "GS", "HD", "INTC", "IBM", "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UNH", "UTX", "VZ", "V", "WMT", "DIS")
setwd("/home/ramela/Documents/Master/MVA/ML_project")
dataset  <- read.csv('all_stocks_7.csv',sep = ';',row.names = 1)
source("usefulFunctions.R")

###########################################
# Partitioning 
###########################################

positions <- c()
for (i in filenames){
  positions <- c(positions,which(colnames(dataset)==paste0('IncrementDayCategorical',i) | colnames(dataset)==paste0('IncrementPreCategorical',i) | colnames(dataset)==paste0('MaxDay',i) | colnames(dataset)==paste0('MinDay',i) | colnames(dataset)==paste0('IncrementDay',i) | colnames(dataset)==paste0('IncrementPre',i) ))
}
X <- dataset[,-positions]
Y <- dataset[,positions]

n <- nrow(X)   

set.seed(250)
amount_train <- 0.8
amount_test <- 0.2

ntest <- round(amount_test * n)
ntrain <- round(amount_train * n)

X.test <- data.frame(X[(n - ntest + 1):n,])
Y.test <- data.frame(Y[(n - ntest + 1):n,])

# in case CV
X.train.val <- data.frame(X[1:(n-ntest),])
Y.train.val <- data.frame(Y[1:(n-ntest),])

# in case validation error. Val 10% of the total data, 12.5% of training data (80%)
amount_val <- 0.3
nval <- round(amount_val*n)
val_ind <- sample(1:nrow(X.train.val), nval)
X.train <- data.frame(X.train.val[-val_ind,])
Y.train <- data.frame(Y.train.val[-val_ind,])
X.val <- data.frame(X.train.val[val_ind,])
Y.val <- data.frame(Y.train.val[val_ind,])

# in case I do not need PCA, I need to partitionate also dataset
dataset.train.val <- dataset[1:(n-ntest),]
dataset.test <- dataset[(n - ntest + 1):n,]

##########################################
# PCA
##########################################

library(FactoMineR)
pca.results <- PCA(X,ncp = ncol(X))

# deciding the number of principal components we want to take
cum.sum.eig <- cumsum(pca.results$eig[,1])
cum.sum.eig.norm <- cum.sum.eig / tail(cum.sum.eig, n=1)
# we decide to retain as many eigenvalues as needed in order to have 80% of the total 
# Inertia
nd <- which(cum.sum.eig.norm>=0.95)[1] 

# redefining X in PCA space
X.test.pca <- as.data.frame(predict(pca.results, X.test)$coord[,1:nd])
X.train.val.pca <- as.data.frame(predict(pca.results, X.train.val)$coord[,1:nd])
X.train.pca <- as.data.frame(predict(pca.results, X.train)$coord[,1:nd])
X.val.pca <- as.data.frame(predict(pca.results, X.val)$coord[,1:nd])

###########################################
# LINEAR REGRESSION WITH THE WHOLE DATASET
###########################################

model.lr.train <- list()
model.test.prob_pred.lr.val <- list()
for (i in 1:length(filenames)) {
  ## Linear regression
  print(i)
  pos <- which(colnames(Y)==paste0('IncrementDay',filenames[i]))
  model.lr.train[[i]] = lm(Y.train[,pos] ~ ., data=X.train.pca)
  model.test.prob_pred.lr.val[[i]] <- predict(model.lr.train[[i]], newdata=X.val.pca)
  Y.val <- addColumn(Y.val, model.test.prob_pred.lr.val[[i]], paste0('PredLinearTrain', filenames[i]))
}

performance_stocks_lr <- list()
for (i in 1:10) {
  performance_stocks_lr[[i]] <- generateBenefitContinuousDay(Y.val, "PredLinearTrain", filenames, i)
}

amount_mean_lr <- which(performance_stocks_lr == max(unlist(performance_stocks_lr)))[1]

model.lr.train.val <- list()
model.test.prob_pred.lr.test <- list()
for (i in 1:length(filenames)) {  
  print(i)
  pos <- which(colnames(Y)==paste0('IncrementDay',filenames[i]))
  model.lr.train.val[[i]] = lm(Y.train.val[,pos] ~ ., data=X.train.val.pca)
  model.test.prob_pred.lr.test[[i]] <- predict(model.lr.train.val[[i]], newdata=X.test.pca)
  Y.test <- addColumn(Y.test, model.test.prob_pred.lr.test[[i]], paste0('PredLinearTest',filenames[i]))
}

generateBenefitContinuousDay(Y.test, "PredLinearTest", filenames, amount_mean_lr)

###########################################
# RANDOM FOREST WITH THE WHOLE DATASET
###########################################

library(randomForest)

## Random forest
model.rf.train <- list()
model.test.prob_pred.rf.val <- list()
for (i in 1:length(filenames)) {
  ## Linear regression
  print(i)
  pos <- which(colnames(Y)==paste0('IncrementDayCategorical',filenames[i]))
  curr_factor <- as.factor(Y.train[,pos])
  model.rf.train[[i]] <- randomForest(curr_factor ~ . , data = X.train.pca)
  model.test.prob_pred.rf.val[[i]] <- predict(model.rf.train[[i]], newdata=X.val.pca)
  Y.val <- addColumn(Y.val, model.test.prob_pred.rf.val[[i]], paste0('PredRandomTrain',filenames[i]))
}

performance_stocks_rf <- list()
for (i in 1:10) {
  performance_stocks_rf[[i]] <- generateBenefitContinuousDay(Y.val, "PredRandomTrain", filenames, i)
}

amount_mean_rf <- which(performance_stocks_rf == max(unlist(performance_stocks_rf)))[1]

model.rf.train.val <- list()
model.test.prob_pred.rf.test <- list()
for (i in 1:length(filenames)) {  
  print(i)
  pos <- which(colnames(Y)==paste0('IncrementDayCategorical',filenames[i]))
  curr_factor <- as.factor(Y.train.val[,pos])
  model.rf.train.val[[i]] = randomForest(curr_factor ~ ., data=X.train.val.pca)
  model.test.prob_pred.rf.test[[i]] <- predict(model.rf.train.val[[i]], newdata=X.test.pca)
  Y.test <- addColumn(Y.test, model.test.prob_pred.rf.test[[i]], paste0('PredRandomTest',filenames[i]))
}

generateBenefitContinuousDay(Y.test, "PredRandomTest", filenames, amount_mean)

###########################################
# kNN WITH THE WHOLE DATASET
###########################################

library(class)

## kNN
model.knn.train <- list()
amountCenters <- 10
for (i in 1:length(filenames)) {
  print(i)
  pos <- which(colnames(Y)==paste0('IncrementDayCategorical',filenames[i]))
  curr_factor_train <- as.factor(Y.train[,pos])
  for (centers in 1:amountCenters){
    print(centers)
    predicted_labels <- knn(train = X.train.pca, test = X.val.pca,cl = curr_factor_train, k=centers)
    
    new_prediction <- as.data.frame(predicted_labels)
    colnames(new_prediction) <- c(paste0('PredKNNTrain',centers,filenames[i]))  
    Y.val <- cbind(Y.val, new_prediction)
  }
}

benefits <- matrix(, nrow=amountCenters, ncol=10)
dim(benefits)
for (centers in 1:amountCenters){
  print(centers)
  for (choices in 1:10){
    print(choices)
    curr_col_label <- paste0("PredKNNTrain", centers)
    benefits[centers, choices] <- generateBenefitContinuousDay(Y.val, curr_col_label, filenames, choices)
  }
}

total_pos <- which(benefits == max(benefits))
prevChoices <- (total_pos %% 10) + 1
prevCenters <- ((total_pos - prevChoices) / 10) + 1
benefits[prevChoices, prevCenters]

for (i in 1:length(filenames)) {
  print(i)
  pos <- which(colnames(Y)==paste0('IncrementDayCategorical',filenames[i]))
  curr_factor_train <- as.factor(Y.train.val[,pos])

  predicted_labels <- knn(train = X.train.val.pca, test = X.test.pca,cl = curr_factor_train, k=prevCenters)
  
  new_prediction <- as.data.frame(predicted_labels)
  colnames(new_prediction) <- c(paste0('PredKNNTest',prevCenters,filenames[i]))  
  Y.test <- cbind(Y.test, new_prediction)
}

generateBenefitContinuousDay(Y.test, paste0('PredKNNTest', prevCenters), filenames, prevChoices)


###########################################
# NN WITH THE WHOLE DATASET
###########################################

library(nnet)

size <- c(2,3)
decay <- c(0.01,0.1)
for(i in 1:length(filenames)){
  pos <- which(colnames(Y.train)==paste0('IncrementDay',filenames[i]))
  for(j in 1:length(size)){
    for(z in 1:length(decay)){
      model.nnet <- nnet(Y.train[,pos]~.,data=X.train.pca,  size=size[j], maxit=5, decay=decay[z],MaxNWts=50000)
      predicted_values <- predict (model.nnet, newdata=X.val.pca)
      new_prediction <- as.data.frame(predicted_values)
      colnames(new_prediction) <- c(paste0('PredNNTrain', j, z, filenames[i]))  
      Y.val <- cbind(Y.val, new_prediction)
    }
  }
}

benefits <- array(0, c(length(size),length(decay),10))
for(j in 1:length(size)){
  print(j)
  for(z in 1:length(decay)){
    print(z)
    for(choices in 1:10){
      print(choices)
      curr_col_label <- paste0("PredNNTrain", j, z)
      benefits[j, z, choices] <- generateBenefitContinuousDay(Y.val, curr_col_label, filenames, choices)
    }
  }
}

max_value <- max(benefits)

for(j in 1:length(size)){
  for(z in 1:length(decay)){
    for(choices in 1:10){
      if(benefits[j,z,choices] == max_value){
        prevJ <- j
        prevZ <- z
        prevChoices <- choices
        break
      }
    }
  }
}

for(i in 1:length(filenames)){
  pos <- which(colnames(Y.train)==paste0('IncrementDay',filenames[i]))
  model.nnet <- nnet(Y.train.val[,pos]~.,data=X.train.val.pca,  size=size[prevJ], maxit=5, decay=decay[prevZ],MaxNWts=50000)
  predicted_values <- predict (model.nnet, newdata=X.test.pca)
  
  new_prediction <- as.data.frame(predicted_values)
  colnames(new_prediction) <- c(paste0('PredNNTest',prevChoices,filenames[i]))  
  Y.test <- cbind(Y.test, new_prediction)
}

generateBenefitContinuousDay(Y.test, paste0('PredNNTest',prevChoices), filenames, prevChoices)

###########################################
# SVM WITH THE WHOLE DATASET
###########################################

library(e1071)

C <- c(0.1,5)
alpha <- c(0.05,0.1)
for(i in 1:length(filenames)){
  pos <- which(colnames(Y.train)==paste0('IncrementDayCategorical',filenames[i]))
  for(j in 1:length(C)){
    for(z in 1:length(alpha)){
      print(j)
      print(z)
      
      model.svm<- svm(as.factor(Y.train[,pos]) ~ ., data=X.train.pca, type="C-classification", cost=j, gamma=z, kernel="radial", scale = FALSE)
      predicted_values <- predict (model.svm, newdata=X.val.pca)
      new_prediction <- as.data.frame(predicted_values)
      colnames(new_prediction) <- c(paste0('PredSVMTrain', j, z, filenames[i]))  
      Y.val <- cbind(Y.val, new_prediction)
    }
  }
}

benefits <- array(0, c(length(C),length(alpha),10))
for(j in 1:length(C)){
  print(j)
  for(z in 1:length(alpha)){
    print(z)
    for(choices in 1:10){
      print(choices)
      curr_col_label <- paste0("PredSVMTrain", j, z)
      benefits[j, z, choices] <- generateBenefitContinuousDay(Y.val, curr_col_label, filenames, choices)
    }
  }
}

max_value <- max(benefits)

for(j in 1:length(C)){
  for(z in 1:length(alpha)){
    for(choices in 1:10){
      if(benefits[j,z,choices] == max_value){
        prevJ <- j
        prevZ <- z
        prevChoices <- choices
        break
      }
    }
  }
}

for(i in 1:length(filenames)){
  pos <- which(colnames(Y.train)==paste0('IncrementDayCategorical',filenames[i]))
  model.svm<- svm(as.factor(Y.train.val[,pos]) ~ ., data=X.train.val.pca, type="C-classification", cost=prevJ, gamma=prevZ, kernel="radial", scale = FALSE)
  predicted_values <- predict (model.svm, newdata=X.test.pca)
  
  new_prediction <- as.data.frame(predicted_values)
  colnames(new_prediction) <- c(paste0('PredSVMTest',prevChoices,filenames[i]))  
  Y.test <- cbind(Y.test, new_prediction)
}

generateBenefitContinuousDay(Y.test, paste0('PredSVMTest',prevChoices), filenames, prevChoices)




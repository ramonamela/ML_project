###########################################
# Reading data 
###########################################
d1<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/AXP_7.csv", row.names = 1, sep = ";", header = TRUE)
d2<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/BA_7.csv", row.names = 1, sep = ";", header = TRUE)
d3<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/CVX_7.csv", row.names = 1, sep = ";", header = TRUE)
d4<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/KO_7.csv", row.names = 1, sep = ";", header = TRUE)
AAPL_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/AAPL_7.csv", row.names = 1, sep = ";", header = TRUE)
CAT_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/CAT_7.csv", row.names = 1, sep = ";", header = TRUE)
CSCO_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/CSCO_7.csv", row.names = 1, sep = ";", header = TRUE)
DIS_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/DIS_7.csv", row.names = 1, sep = ";", header = TRUE)
GE_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/GE_7.csv", row.names = 1, sep = ";", header = TRUE)
GS_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/GS_7.csv", row.names = 1, sep = ";", header = TRUE)
HD_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/HD_7.csv", row.names = 1, sep = ";", header = TRUE)
IBM_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/IBM_7.csv", row.names = 1, sep = ";", header = TRUE)
INTC_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/INTC_7.csv", row.names = 1, sep = ";", header = TRUE)
JNJ_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/JNJ_7.csv", row.names = 1, sep = ";", header = TRUE)
JPM_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/JPM_7.csv", row.names = 1, sep = ";", header = TRUE)
MCD_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/MCD_7.csv", row.names = 1, sep = ";", header = TRUE)
MMM_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/MMM_7.csv", row.names = 1, sep = ";", header = TRUE)
MRK_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/MRK_7.csv", row.names = 1, sep = ";", header = TRUE)
MSFT_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/MSFT_7.csv", row.names = 1, sep = ";", header = TRUE)
NKE_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/NKE_7.csv", row.names = 1, sep = ";", header = TRUE)
PRE_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/PRE_7.csv", row.names = 1, sep = ";", header = TRUE)
PG_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/PG_7.csv", row.names = 1, sep = ";", header = TRUE)
TRV_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/TRV_7.csv", row.names = 1, sep = ";", header = TRUE)
UNH_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/UNH_7.csv", row.names = 1, sep = ";", header = TRUE)
UTX_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/UTX_7.csv", row.names = 1, sep = ";", header = TRUE)
V_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/V_7.csv", row.names = 1, sep = ";", header = TRUE)
VZ_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/VZ_7.csv", row.names = 1, sep = ";", header = TRUE)
WMT_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/WMT_7.csv", row.names = 1, sep = ";", header = TRUE)
XOM_7<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/XOM_7.csv", row.names = 1, sep = ";", header = TRUE)



XOM_cotization<- read.csv("C:/Users/user/Desktop/Machine learning/Final Project/drive/XOM_cotization.csv", sep = ";", header = TRUE)

#dataset<-data.frame(cbind(AXP_7[,2],BA_7[,2],CVX_7[,2], KO_7[,2],AXP_7[,3],BA_7[,3],CVX_7[,3], KO_7[,3]))
#colnames(dataset) <- c("x1", "x2", "x3", "x4","y1", "y2","y3", "y4")

###########################################
# Partitioning 
###########################################
s<-1

#70% train  30% test
set.seed(250)
n<- nrow(d1)                                                                                              
learn <- sample(1:n, round(0.7*n))
nlearn <- length(learn)
ntest <- n - nlearn

#merging all stocks in 1 list of dataframes
d<-list(d1)
#d<-list(d1,d2,d3,d4)

#initializing the output lists:
testset_x<-list()
trainset_x<-list()
testset_y<-list()
trainset_y<-list()
testset_y_cat<-list()
trainset_y_cat<-list()
dx<-list()
dy<-list()
dy_c<-list()

#Categirical vars:

for (i in 1:s) {
  #Changing the format of resp: "0" if doesn't increase, "1" if increase, "-1" if decsease:
  d[[i]]$IncrementDayCategorical<-as.numeric(d[[i]]$IncrementDayCategorical)
  d[[i]]$IncrementDayCategorical[d[[i]]$IncrementDayCategorical<0] <- -1
  d[[i]]$IncrementDayCategorical[d[[i]]$IncrementDayCategorical==0] <- 0
  d[[i]]$IncrementDayCategorical[d[[i]]$IncrementDayCategorical>0] <- 1
  #Categorical vars:
d[[i]]$IncrementDayCategorical<-as.factor(d[[i]]$IncrementDayCategorical)
dx[[i]]<-subset(d[[i]], select=-c(IncrementPre,IncrementDay, MinDay, MaxDay, IncrementDayCategorical, IncrementPreCategorical))
dy[[i]]<-data.frame(subset(d[[i]], select="IncrementDay"))
dy_c[[i]]<-subset(d[[i]], select="IncrementDayCategorical")
testset_x[[i]]=dx[[i]][-learn,]
trainset_x[[i]]=dx[[i]][learn,]
testset_y[[i]]=dy[[i]][-learn,]
trainset_y[[i]]=dy[[i]][learn,]
testset_y_cat[[i]]=dy_c[[i]][-learn,]
trainset_y_cat[[i]]=dy_c[[i]][learn,]
}

#Check results:
dim(testset_x[[1]])
length(testset_y[[1]])
dim(trainset_x[[1]])
length(trainset_y[[1]])
length(trainset_y_cat[[1]])
length(testset_y_cat[[1]])
###########################################
# LINEAR REGRESSION
###########################################

#correlation between X and Y
cor <- list()
for (i in 1:s) {
cor[[i]]<-cor(trainset_y[[i]], trainset_x[[i]])}

#Univariate linear regression:
model <- list()
model.final<- list()
beta.FINAL<- list()
for (i in 1:s) {
model[[i]] = lm(trainset_y[[i]] ~ ., data=trainset_x[[i]])

## which we simplify using the AIC (step)
#model.final[[i]] = step( model[[i]]) 
}
#beta.FINAL[[i]] <- coef(model.final[[i]])


###########################################
#Main statistics of LRs:
###########################################
r.square<-list()
adj.r.square<-list()
AIC<-list()
BIC<-list()
for (i in 1:s) {
#R-Squared-Higher the better
  r.square[[i]]<-(summary(model[[i]])$r.squared) 
#Adj R-Squared - Higher the better
  adj.r.square[[i]]<-(summary(model[[i]])$adj.r.squared) 
#AIC - Lower the better
  AIC[[i]]<-AIC(model[[i]])
#BIC - Lower the better
  BIC[[i]]<-BIC(model[[i]])}


###########################################
#Prediction of training set and accuracy
###########################################
model.train.prob_pred<-list()
model.test.prob_pred<-list()
Min_Max_Accuracy_train<-list()
MSE_train<-list()
MAPE_train<-list()
RMSE_train<-list()
Correlation_accuracy_train<-list()
Min_Max_Accuracy_test<-list()
MSE_test<-list()
MAPE_test<-list()
RMSE_test<-list()
Correlation_accuracy_test<-list()

for (i in 1:s) {
  model.train.prob_pred[[i]] <-  predict(model[[i]], newdata = trainset_x[[i]])
  #Min_Max Accuracy-Higher the better
  Min_Max_Accuracy_train[[i]]<-mean(min(trainset_y[[i]], unlist(model.train.prob_pred[[i]]))/max(trainset_y[[i]], unlist(model.train.prob_pred[[i]])))
  #MSE (Mean squared error)-Lower the better 
  MSE_train[[i]]<-sum((unlist(model.train.prob_pred[[i]])-trainset_y[[i]])^2)/(nlearn)
  #MAPE (Mean absolute percentage error)- Lower better
  MAPE_train[[i]]<- mean(abs(unlist(model.train.prob_pred[[1]])-trainset_y[[i]])/trainset_y[[i]])
  #ROOT MEAN SQUARE ERROR:
  RMSE_train[[i]]<- sqrt(mean((unlist(model.train.prob_pred[[i]])-trainset_y[[i]])^2))
  #Correlation accuracy
  Correlation_accuracy_train[[i]]<-cor(unlist(model.train.prob_pred[[1]]),trainset_y[[i]])
  }

###########################################
#Checking:
###########################################
r<-data.frame(cbind(trainset[,5],model.train.prob_pred[[1]]))
colnames(r)<-c("true", "pred")
mse<-sum((r$pred - r$true)^2)/(nrow(r)-1)
mse
#Min_Max Accuracy-Higher the better
Min_Max_Accuracy<-mean(min(r$true, r$pred)/max(r$true, r$pred))
Min_Max_Accuracy
#MAPE (Mean absolute percentage error)- Lower better
MAPE = mean(abs((r$pred-r$true))/r$true)
#ROOT MEAN SQUARE ERROR:
RMSE<- sqrt(mean((r$pred-r$true)^2))
RMSE
correlation_accuracy <- cor(r)

#Prediction of test set
for (i in 1:s) {
  model.test.prob_pred[[i]] <-  predict.lm(model[[i]], newdata=testset_x[[i]])
  #Min_Max Accuracy-Higher the better
  Min_Max_Accuracy_test[[i]]<-mean(min(testset_y[[i]], unlist(model.test.prob_pred[[i]]))/max(testset_y[[i]], unlist(model.test.prob_pred[[i]])))
  #MSE (Mean squared error)-Lower the better 
  MSE_test[[i]]<-sum((unlist(model.test.prob_pred[[i]])-testset_y[[i]])^2)/(ntest)
  #MAPE (Mean absolute percentage error)- Lower better
  MAPE_test[[i]]<- mean(abs(unlist(model.test.prob_pred[[1]])-testset_y[[i]])/testset_y[[i]])
  #ROOT MEAN SQUARE ERROR:
  RMSE_test[[i]]<- sqrt(mean((unlist(model.test.prob_pred[[1]])-testset_y[[i]])^2))
  #Correlation accuracy
  Correlation_accuracy_test[[i]]<-cor(unlist(model.test.prob_pred[[1]]),testset_y[[i]])
}



###########################################
#   RANDOM FOREST
###########################################

# Levels of the training and testing set are not the same, it is not possible to predict the exact change in... 

library(randomForest)

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

  ###############################################################
  # Example 4: k-Nearest Neighbor analysis in an artificial problem
  ###############################################################
  
  ## k-NN can be found in several places in R packages; one of them is in the "class" package
  
  library(class)
  
  ## CAREFUL! use it correctly! train and test cannot intersect!
  
  ## Usage:
  
  # knn (train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
  
  # This function predicts 'test' data; for each observation in 'test', looks for the k-nearest neighbors in 'train' (using plain Euclidean distance).
  
  # The classification is decided by majority vote, with ties broken at random (but if there are ties for the k-th nearest vector, all candidates are included in the vote)
  
  # 'cl' is the vector of class labels for the observations in 'train'
  # If data is large, and k is not small, I would recommend to use prob = TRUE to get some estimations of posterior probabilities
  
  s <- sqrt(1/4)
  set.seed(1234)
  
  generate <- function (M, n=100, Sigma=diag(2)*s) 
  {
    z <- sample(1:nrow(M), n, replace=TRUE)
    t(apply(M[z,],1, function(mu) mvrnorm(1,mu,Sigma)))
  }
  
  # generate 10 means in two dimensions
  M0 <- mvrnorm(10, c(1,0), diag(2))
  
  # generate data out of M0
  x0 <- generate(M0)
  
  # repeat with M1
  M1 <- mvrnorm(10, c(0,1), diag(2))
  x1 <- generate(M1)
  
  # Bind them together (by rows)
  train <- rbind(x0, x1)
  (N <- dim(train)[1])
  
  # generate class labels in {0,1}
  t <- c(rep(0,100), rep(1,100))
  
  # Now generate a huge test data using a grid in the correct range
  
  grid.size <- 100
  XLIM <- range(train[,1])
  grid.x <- seq(XLIM[1], XLIM[2], len=grid.size)
  
  YLIM <- range(train[,2])
  grid.y <- seq(YLIM[1], YLIM[2], len=grid.size)
  
  test <- expand.grid(grid.x,grid.y)
  dim(test)
  
  # Let's visualize 1-NN (only 1 neighbor) in action
  
  nicecolors <- c('black','red')
  
  visualize.1NN <- function ()
  {
    par(mfrow=c(1,1))
    
    predicted <- knn (train, test, t, k=1)
    
    # These are the predictions
    plot(train, xlab="X1", ylab="X2", xlim=XLIM, ylim=YLIM, type="n")
    points(test, col=nicecolors[as.numeric(predicted)], pch=".")
    contour(grid.x, grid.y, matrix(as.numeric(predicted),grid.size,grid.size), 
            levels=c(1,2), add=TRUE, drawlabels=FALSE)
    
    # Add training points, for reference
    points(train, col=nicecolors[t+1], pch=16)
    title("1-NN classification")
  }
  
  visualize.1NN ()
  
  ## In order to see the effect of a different number of neighbors, let's do something nice:
  
  par(mfrow=c(2,3))
  
  for (myk in c(1,3,5,7,10,round(sqrt(N))))
  {
    predicted <- knn(train, test, t, k=myk)
    
    plot(train, xlab="X1", ylab="X2", xlim=XLIM, ylim=YLIM, type="n")
    points(test, col=nicecolors[as.numeric(predicted)], pch=".")
    contour(grid.x, grid.y, matrix(as.numeric(predicted),grid.size,grid.size), 
            levels=c(1,2), add=TRUE, drawlabels=FALSE)
    
    # add training points, for reference
    points(train, col=nicecolors[t+1], pch=16)
    title(paste(myk,"-NN classification",sep=""))
  }
  
  # Possibly you have to "Zoom" the plot to see it properly
  
  # This maximum value (sqrt of training set size) for the number of nearest neighbors is a popular rule of thumb ... but I do not have an explanation for it
  # Folklore also says that it should be a prime number ...
  
  # Now we can illustrate the method on a real problem: we use the previous wine data for comparison
  
  ## First setup a k-NN model with 3 neighbours
  ## Notice there is no "learning" ... the data is the model (just test!)
  
  wine.data <- subset (wine, select = -Wine.type)
  
  # Very crude way of performing LOOCV for k-NN :-)
  
  knn.preds <- rep(NA, nrow(wine.data))
  
  for (i in 1:nrow(wine.data))
  {
    knn.preds[i] <- knn (wine.data[-i,], wine.data[i,], wine$Wine.type[-i], k = 3) 
  }
  
  (tab <- table(Truth=wine$Wine.type, Preds=knn.preds))
  1 - sum(tab[row(tab)==col(tab)])/sum(tab)
  
  ## As usual, rows are true targets, columns are predictions (may I suggest that you adhere to this convention too)
  
  ## One could also use the function 'knn1()' when k=1 (just one neighbour)
  
  ## How do we optimize k? One way is by using the LOOCV
  
  # Actually there is an implementation of LOOCV for k-NN:
  
  myknn.cv <- knn.cv (wine.data, wine$Wine.type, k = 3)
  
  (tab <- table(Truth=wine$Wine.type, Preds=myknn.cv))
  1 - sum(tab[row(tab)==col(tab)])/sum(tab)
  
  # The results may not fully coincide by the random tie-breaking mechanism
  
  ## Let's loop over k using a function
  set.seed (6046)
  
  # Since sqrt(178) is approx 13.34, we take 13 as the max number of neighbours
  
  N <- nrow(wine)
  neighbours <- 1:sqrt(N)
  
  loop.k <- function (mydata, mytargets, myneighbours)
  {
    errors <- matrix (nrow=length(myneighbours), ncol=2)
    colnames(errors) <- c("k","LOOCV error")
    
    for (k in myneighbours)
    {
      print(k)
      myknn.cv <- knn.cv (mydata, mytargets, k = myneighbours[k])
      
      # fill in number of neighbours and LOOCV error
      errors[k, "k"] <- myneighbours[k]
      
      tab <- table(Truth=mytargets, Preds=myknn.cv)
      errors[k, "LOOCV error"] <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab)
    }
    errors
  }
  
  
  par(mfrow=c(1,1))
  
  plot(loop.k (wine.data, wine$Wine.type, neighbours), type="l", xaxt = "n")
  axis(1, neighbours)
  
  ## It seems that kNN does a pretty bad job here; 1-NN is the best choice but the model is terrible, compared to that of LDA/QDA
  
  ## k-NN normally benefits from standardizing the variables:
  
  plot(loop.k (scale(wine.data), wine$Wine.type, neighbours), type="l", xaxt = "n")
  axis(1, neighbours)
  
  # ... which is very true in this case
  
  ## An alternative idea would be to use the previously computed LDs
  
  lda.model <- lda (Wine.type ~ ., data = wine)
  loadings <- as.matrix(wine.data) %*% as.matrix(lda.model$scaling)
  
  ## Let's repeat the loop over k
  set.seed (6046)
  
  plot(loop.k (loadings, wine$Wine.type, neighbours), type="l", xaxt = "n")
  axis(1, neighbours)
  
  # So we would keep 6 neighours
  
  # Notice that the tested values for k need not be consecutive; in a large dataset, this would be very time-consuming; also we would not use LOOCV for the same reason, but rather 10-CV


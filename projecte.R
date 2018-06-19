rm(list=ls())
setwd("/home/ramela/Documents/Master/MVA/ML_project/")
source("createCompaniesAsRwsDataset.R")
# ----------------------------------------------------------------------------------------------------------------------------- #
# PCA + Clusterring

filenames <- c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "XOM", "GE", "GS", "HD", "INTC", "IBM", "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UNH", "UTX", "VZ", "V", "WMT", "DIS")
dataset <- createCompaniesAsRowsDataset(filenames)
dataset_center <- scale(dataset, scale = FALSE)

library(FactoMineR)
library(psych)

get_outliers <- function(new_individuals, per_outliers = 0.2) {
  amount_outliers <- round(nrow(new_individuals) * per_outliers)
  #print(amount_outliers)
  m_dist <- mahalanobis(new_individuals, colMeans(new_individuals), cov(new_individuals))
  m_dist <- sort(m_dist)
  print(m_dist)
  outliers <- m_dist[(nrow(new_individuals) - amount_outliers + 1):nrow(new_individuals)]
  outlier.index <- which(rownames(new_individuals) %in% names(outliers))
  #print(outlier.index)
  return(outlier.index)
}

get_nd <- function(pca.results, percentage = 0.95) {
  cum.sum.eig <- cumsum(pca.results$eig[,1])
  cum.sum.eig.norm <- cum.sum.eig / tail(cum.sum.eig, n=1)
  return(which(cum.sum.eig.norm>=0.95)[1])
}

generate_outliers <- function(dataset_center, perc_outliers = 0.2, limit = 100){
#perc_outliers <- 0.2
#limit <- 10

  pca.results <- PCA(dataset_center, ncp = ncol(dataset_center))
  
  # deciding the number of principal components we want to take
  cum.sum.eig <- cumsum(pca.results$eig[,1])
  cum.sum.eig.norm <- cum.sum.eig / tail(cum.sum.eig, n=1)
  # we decide to retain as many eigenvalues as needed in order to have 80% of the total 
  # Inertia

  amount_outliers <- round(nrow(dataset_center) * perc_outliers)
  nd <- get_nd(pca.results, percentage = 0.95)
  counter <- rep(0, length(filenames))
  
  new_individuals <- pca.results$ind$coord[,1:nd]
  iter <- 0
  
  pos.outliers <- get_outliers(new_individuals[,1:nd], per_outliers = perc_outliers)

  while(iter < limit){
    
    weights <- rep(1,nrow(dataset_center))
    for(i in pos.outliers){
      counter[i] <- counter[i] + 1
      weights[i] <- 0.0001 
    }
    
    pca.results <- PCA(dataset_center,ncp = ncol(dataset_center),row.w=weights)
    nd <- get_nd(pca.results, percentage = 0.95)
    print(nd)
    new_individuals <- pca.results$ind$coord[,1:nd]
    
    pre.outliers <- pos.outliers
    pos.outliers <- get_outliers(new_individuals[,1:nd], per_outliers = perc_outliers)
    print(pos.outliers)
    
    if(all.equal(pre.outliers, pos.outliers) == TRUE){
      break
    }
    print(iter)
    print(counter)
    iter <- iter + 1
  }
  
  if(iter == limit){
    return(counter)
  }
  
  return(pos.outliers)

}

count.0.1 <- generate_outliers(dataset_center, perc_outliers = 0.1)

count.0.2 <- generate_outliers(dataset_center, perc_outliers = 0.2)

count.0.3 <- generate_outliers(dataset_center, perc_outliers = 0.3)

count.0.4 <- generate_outliers(dataset_center, perc_outliers = 0.4)

count.0.5 <- generate_outliers(dataset_center, perc_outliers = 0.5)


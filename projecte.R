rm(list=ls())
setwd("/home/ramela/Documents/Master/MVA/ML_project/")
source("createCompaniesAsRwsDataset.R")
# ----------------------------------------------------------------------------------------------------------------------------- #
# PCA + Clusterring

filenames <- c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "XOM", "GE", "GS", "HD", "INTC", "IBM", "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UNH", "UTX", "VZ", "V", "WMT", "DIS")
dataset <- createCompaniesAsRowsDataset(filenames)

# centering PCA
library(FactoMineR)

#dataset_centered = scale(dataset)
#pca.results <- PCA(dataset_centered,ncp = ncol(dataset))

dataset_center <- scale(dataset, scale = FALSE)
pca.results <- PCA(dataset_center, ncp = ncol(dataset))

# deciding the number of principal components we want to take
cum.sum.eig <- cumsum(pca.results$eig[,1])
cum.sum.eig.norm <- cum.sum.eig / tail(cum.sum.eig, n=1)
# we decide to retain as many eigenvalues as needed in order to have 80% of the total 
# Inertia
nd <- which(cum.sum.eig.norm>=0.95)[1] 
new_individuals <- dataset_center %*% pca.results$svd$V[,1:nd]

new_individuals[1,]
pca.results$ind$coord[1,]

# outliers detection
library(chemometrics)
outliers.searcher <- Moutlier(new_individuals[,1:nd], quantile = 0.975, plot=FALSE)

plot(outliers.searcher$md,outliers.searcher$rd,xlab='Classical Mahalanobis distance',ylab='Robust Mahalanobis distance', xlim = c(1,7),ylim = c(0,400))
names.vector <- names(outliers.searcher$md) 
text(outliers.searcher$md,outliers.searcher$rd, labels=names.vector, cex= 0.7,pos=2)  
abline(h=outliers.searcher$cutoff,col=2)
abline(v=outliers.searcher$cutoff,col=2)
pos.outliers <- which(outliers.searcher$rd>outliers.searcher$cutoff)

# doing PCA again without taking into consideration those rows
dataset.2 = dataset[-c(pos.outliers),]
pca.results.2 <- PCA(dataset.2,ncp = ncol(dataset.2))
new_individuals.2 <- dataset_center %*% pca.results.2$svd$V[,1:nd]

# outliers detection again  
outliers.searcher.2 <- Moutlier(new_individuals.2, quantile = 0.975,plot=FALSE)

plot(outliers.searcher.2$md,outliers.searcher.2$rd,xlab='Classical Mahalanobis distance',ylab='Robust Mahalanobis distance', xlim = c(1,7),ylim = c(0,400))
names.vector.2 <- names(outliers.searcher.2$md) 
text(outliers.searcher.2$md,outliers.searcher.2$rd, labels=names.vector, cex= 0.7,pos=2)  
abline(h=outliers.searcher.2$cutoff,col=2)
abline(v=outliers.searcher.2$cutoff,col=2)
pos.outliers.2 <- which(outliers.searcher.2$rd>outliers.searcher.2$cutoff)


# doing PCA again without taking into consideration those rows
dataset.2 = dataset[-c(pos.outliers.2),]
pca.results.2 <- PCA(dataset.2,ncp = ncol(dataset.2))
new_individuals.2 <- dataset_center %*% pca.results.2$svd$V[,1:nd]

# outliers detection again  
outliers.searcher.2 <- Moutlier(new_individuals.2, quantile = 0.975,plot=FALSE)

plot(outliers.searcher.2$md,outliers.searcher.2$rd,xlab='Classical Mahalanobis distance',ylab='Robust Mahalanobis distance', xlim = c(1,7),ylim = c(0,400))
names.vector.2 <- names(outliers.searcher.2$md) 
text(outliers.searcher.2$md,outliers.searcher.2$rd, labels=names.vector, cex= 0.7,pos=2)  
abline(h=outliers.searcher.2$cutoff,col=2)
abline(v=outliers.searcher.2$cutoff,col=2)
pos.outliers.3 <- which(outliers.searcher.2$rd>outliers.searcher.2$cutoff)




# doing PCA again without taking into consideration those rows
dataset.2 = dataset[-c(pos.outliers.3),]
pca.results.2 <- PCA(dataset.2,ncp = ncol(dataset.2))
new_individuals.2 <- dataset_center %*% pca.results.2$svd$V[,1:nd]

# outliers detection again  
outliers.searcher.2 <- Moutlier(new_individuals.2, quantile = 0.975,plot=FALSE)

plot(outliers.searcher.2$md,outliers.searcher.2$rd,xlab='Classical Mahalanobis distance',ylab='Robust Mahalanobis distance', xlim = c(1,7),ylim = c(0,400))
names.vector.2 <- names(outliers.searcher.2$md) 
text(outliers.searcher.2$md,outliers.searcher.2$rd, labels=names.vector, cex= 0.7,pos=2)  
abline(h=outliers.searcher.2$cutoff,col=2)
abline(v=outliers.searcher.2$cutoff,col=2)
pos.outliers.2 <- which(outliers.searcher.2$rd>outliers.searcher.2$cutoff)








# continuing with PCA
p <- ncol(dataset)

Psi <- pca.results$ind$coord[,1:nd]
Phi <- pca.results$var$coord[,1:nd]

# # rotation
# p <- ncol(dataset)
# 
# Psi <- pca.results$ind$coord[,1:nd]
# Phi <- pca.results$var$coord[,1:nd]
# pc.rot <-varimax(Phi)
# Phi.rot <- pc.rot$loadings[1:p,]
# lmb.rot <- diag(t(Phi.rot) %*% Phi.rot)
# Psi_stan.rot <- dataset %*% solve(cor(dataset)) %*% Phi.rot
# Psi.rot <- Psi_stan.rot %*% diag(sqrt(lmb.rot))
# 
# # plotting pca results with rotated axes
# library(calibrate)
# ze = rep(0,p)
# plot(Phi.rot,type="n",xlim=c(-1,1),ylim=c(-1,1)) 
# text(Phi.rot,labels=colnames(dataset), col="blue")
# arrows(ze, ze, Phi.rot[,1], Phi.rot[,2], length = 0.07,col="blue") 
# abline(h=0,v=0,col="gray")
# circle(1)
# plot(Psi.rot,type="n") 
# text(Psi.rot,labels=rownames(dataset)) 
# abline(h=0,v=0,col="gray")
# 
# 
# pca.results$ind$coord[,1:nd] <- Psi.rot
# dimdesc(pca.results, axes=1:nd)

  # summary of the pca
summary(pca.results)


# CLUSTERING

library(flashClust)
library(cluster)
library(factoextra)
# Perform two initial kmeans clustering
n1 = 14
k1 <- kmeans(Psi,n1) # no se si hauria de posar Psi.rot perque Psi l'he actualitzada pero aqui agafo el valor anterior
k2 <- kmeans(Psi,n1)
table(k2$cluster,k1$cluster)
clas <- (k2$cluster-1)*n1+k1$cluster
freq <- table(clas)   
cdclas <- aggregate(as.data.frame(Psi),list(clas),mean)[,2:(nd+1)]

# Perform a hierarchical clustering from the centroids of the clusters obtained with kmeans
d2 <- dist(cdclas)
h2 <- stats::hclust(d2,method="ward.D2",members=freq)  # COMPARE THE COST
plot(h2)
barplot(h2$height[(nrow(cdclas)-16):(nrow(cdclas)-1)])  # PLOT OF THE LAST 16 AGGREGATIONS

# no m'acaben de sortir be els resultats. segons el barplot, 6 clusters pot estar be, pero amb els seguents metodes em diuen que 5,6 son dels pitjors
gap_stat <- clusGap(Psi, FUN = kmeans, nstart = 25, K.max = 16)
fviz_gap_stat(gap_stat)
optimal <- fviz_nbclust(Psi, FUNcluster = kmeans)
optimal
optimal_clusters <- which(max(optimal$data$y) == optimal$data$y)

nc = 5  
c2 <- cutree(h2,nc)
cdg <- aggregate((diag(freq/sum(freq)) %*% as.matrix(cdclas)),list(c2),sum)[,2:(nd+1)]



# consolidation
km.x <- kmeans(Psi,centers=cdg)

# plot of the first factorial plane with the clusters
aux <- data.frame(cbind(Psi[,1:2],clusters=km.x$cluster))

subset <- aux[aux$clusters==1,]
plot(subset[,1],subset[,2],xlab = 'Dim1',ylab = 'Dim2',xlim = c(-100,100),ylim = c(-100,100),col=1)

for(i in unique(km.x$cluster)){
  subset <- aux[aux$clusters==i,]
  points(subset[,1],subset[,2],xlab = 'Dim1',ylab = 'Dim2',xlim = c(-100,100),ylim = c(-100,100),col=i)
}

# clusters
km.x$cluster

# ----------------------------------------------------------------------------------------------------------------------------- #  
# applying hclustering only and check if the results are consistent
dist.matrix <- dist(Psi)
hc<- stats::hclust(dist.matrix,method="ward.D2")
plot(hc)
barplot(h2$height)
nc = 5
c1 <- cutree(hc,nc)
# LETS SEE THE PARTITION VISUALLY
plot(Psi[,1],Psi[,2],type="n",main="Clustering of expenses in 4 classes")
text(Psi[,1],Psi[,2],col=c1,labels=names(c1),cex = 0.6) 
abline(h=0,v=0,col="gray") 
legend("topleft",c("c1","c2","c3","c4"),pch=20,col=c(1:4))

# ----------------------------------------------------------------------------------------------------------------------------- #  
# PROFILING
# prova <- data.frame(cbind(as.factor(km.x$cluster),dataset))
# catdes(prova, num.var=1)  



# }










dataset_centered = scale(dataset, scale = FALSE)
pca.results <- PCA(dataset_centered,ncp = ncol(dataset))

# deciding the number of principal components we want to take
cum.sum.eig <- cumsum(pca.results$eig[,1])
cum.sum.eig.norm <- cum.sum.eig / tail(cum.sum.eig, n=1)
# we decide to retain as many eigenvalues as needed in order to have 80% of the total 
# Inertia
nd <- which(cum.sum.eig.norm>=0.95)[1] 

# outliers detection
library(chemometrics)
outliers.searcher <- Moutlier(pca.results$ind$coord[,1:nd], quantile = 0.975,plot=FALSE)

plot(outliers.searcher$md,outliers.searcher$rd,xlab='Classical Mahalanobis distance',ylab='Robust Mahalanobis distance')
names.vector <- names(outliers.searcher$md) 
text(outliers.searcher$md,outliers.searcher$rd, labels=names.vector, cex= 0.7,pos=2)  
abline(h=outliers.searcher$cutoff,col=2)
abline(v=outliers.searcher$cutoff,col=2)
pos.outliers <- which(outliers.searcher$rd>outliers.searcher$cutoff)




# doing PCA again without taking into consideration those rows
weights <- rep(1,nrow(dataset))
for(i in pos.outliers){
  weights[i] <- 0.0001 # I cannot set them to 0
}
pca.results <- PCA(dataset_centered,ncp = ncol(dataset),row.w=weights)

# outliers detection again  
outliers.searcher <- Moutlier(pca.results$ind$coord[,1:nd], quantile = 0.975,plot=FALSE)

plot(outliers.searcher$md,outliers.searcher$rd,xlab='Classical Mahalanobis distance',ylab='Robust Mahalanobis distance')
names.vector <- names(outliers.searcher$md) 
text(outliers.searcher$md,outliers.searcher$rd, labels=names.vector, cex= 0.7,pos=2)  
abline(h=outliers.searcher$cutoff,col=2)
abline(v=outliers.searcher$cutoff,col=2)
pos.outliers <- which(outliers.searcher$rd>outliers.searcher$cutoff)

# doing PCA again without taking into consideration those rows
weights <- rep(1,nrow(dataset))
for(i in pos.outliers){
  weights[i] <- 0.0001 # I cannot set them to 0
}
pca.results <- PCA(dataset_centered,ncp = ncol(dataset),row.w=weights)

# outliers detection again  
outliers.searcher <- Moutlier(pca.results$ind$coord[,1:nd], quantile = 0.975,plot=FALSE)

plot(outliers.searcher$md,outliers.searcher$rd,xlab='Classical Mahalanobis distance',ylab='Robust Mahalanobis distance')
names.vector <- names(outliers.searcher$md) 
text(outliers.searcher$md,outliers.searcher$rd, labels=names.vector, cex= 0.7,pos=2)  
abline(h=outliers.searcher$cutoff,col=2)
abline(v=outliers.searcher$cutoff,col=2)
pos.outliers <- which(outliers.searcher$rd>outliers.searcher$cutoff)


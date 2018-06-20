createCompaniesAsRowsDataset <- function(filenames){
  
  # initializing global.dataset with the first name of the list filenames
  dataset  <- read.csv(paste0('/home/ramela/Documents/Master/MVA/ML_project/',filenames[1],'_cotization.csv'),sep = ';',row.names = 1)
  global.dataset <-t(dataset$IncrementDay)
  colnames(global.dataset) <- row.names(dataset)
  
  # reading the rest of the files, transposing them, keeping the interesting row and adding to the main dataset
  for(i in filenames[2:length(filenames)]){
    dir.file <- paste0('/home/ramela/Documents/Master/MVA/ML_project/',i,'_cotization.csv')
    dataset <- read.csv(dir.file,sep=';',row.names=1)
    dataset.transposed <- t(dataset$IncrementDay)
    colnames(dataset.transposed) <- row.names(dataset)
    global.dataset <- rbind(global.dataset,dataset.transposed)
    
  }
  row.names(global.dataset) <- filenames
  
  return(global.dataset)
}

generateBenefitContinuousDay <- function(Y, prefix, filenames, amount, com = 0.25) {
  
  values <- as.data.frame(Y[, which(colnames(Y)==paste0('IncrementDay',filenames[1]))])
  colnames(values) <- filenames[1]
  
  predictions <- as.data.frame(Y[, which(colnames(Y)==paste0(prefix,filenames[1]))])
  colnames(predictions) <- filenames[1]
  
  for(name in filenames[-c(1)]){
    new_column <- as.data.frame(Y[, which(colnames(Y)==paste0('IncrementDay',name))])
    colnames(new_column) <- name
    values <- cbind(values, new_column)
    
    new_column <- as.data.frame(Y[, which(colnames(Y)==paste0(prefix,name))])
    
    colnames(new_column) <- name
    predictions <- cbind(predictions, new_column)
  }
  
  cum_increment <- 1
  for(row in 1:nrow(values)){
    choice <- sort(predictions[row,], decreasing = TRUE)[1:amount]
    choiced_names <- colnames(choice)
    real_values <- values[row,][choiced_names]
    real_increment <- rowMeans(real_values)
    cum_increment <- cum_increment * (1 + (real_increment / 100))
  }
  
  return(cum_increment)
}

addColumn <- function(Y, newCol, colName) {
  if(colName %in% colnames(Y)) {
    Y[,colName] <- NULL
  }
  newColDat <- as.data.frame(newCol)
  colnames(newColDat) <- colName
  Y <- cbind(Y, newColDat)
  return(Y)
}



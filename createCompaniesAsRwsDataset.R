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

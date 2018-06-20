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

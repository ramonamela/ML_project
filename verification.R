Y.test
names(Y.test)

#cum.1 <- generateBenefitContinuousDay(Y.test, "PrevLinear", filenames, 1)
#cum.2 <- generateBenefitContinuousDay(Y.test, "PrevLinear", filenames, 2)
#cum.3 <- generateBenefitContinuousDay(Y.test, "PrevLinear", filenames, 3)
#cum.4 <- generateBenefitContinuousDay(Y.test, "PrevLinear", filenames, 4)
#cum.5 <- generateBenefitContinuousDay(Y.test, "PrevLinear", filenames, 5)
#cum.6 <- generateBenefitContinuousDay(Y.test, "PrevLinear", filenames, 6)

source("createCompaniesAsRwsDataset.R")

generateBenefitContinuousDay <- function(Y, prefix, filenames, amount, com = 0.25) {
  #prefix <- "PrevLinear"
  #amount <- 3

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
    #print("Mean of increments:")
    #print(real_increment)
    #print("Multiplier")
    #print(1 + (real_increment / 100))
    cum_increment <- cum_increment * (1 + (real_increment / 100))
    #print("Cummulated increment")
    #print(cum_increment)
  }
  
  return(cum_increment)
}

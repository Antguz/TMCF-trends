################################################################################
### Function to compare the mean trend from 0 using Bayes
################################################################################

###Select libraries-------------------------------------------------------------
library(data.table)
library(BayesianFirstAid)
library(MASS)

### Load frame------------------------------------------------------------------
frame <- fread("data/TMCF_slope.csv")
frame <- subset(frame, remove != "yes") #Remove duplicates
frame <- frame[, c(9:12, 15:16, 18, 20:22)]

### Bayes application ----------------------------------------------------------
bayes_apply <- function(frame) {
  
  frame <- as.data.frame(frame)
  col_names <- colnames(frame)
  
  complete <- data.table()
  
  for(i in 1:length(col_names)) {
    
    variable <- na.exclude(frame[, col_names[i]])*1000
    
    bayes <- bayes.t.test(variable)
    
    results <- bayes$stats
    parameter <- row.names(results)
    variable <- rep(col_names[i], 5)
    results <- cbind(variable, parameter, as.data.table(results))
    
    complete <- rbind(complete, results)
    
  }
  
  return(complete)
  
}


#Run
bayes_results <- bayes_apply(frame)
fwrite(bayes_results, "data/bayes_results.csv")

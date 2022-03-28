################################################################################
### Function to compare the mean trend from 0 using Bayes
################################################################################

###Select libraries-------------------------------------------------------------
library(data.table)
library(BayesianFirstAid)
library(MASS)
library(raster)

### Load frame------------------------------------------------------------------
frame <- fread("data/TMCF_slope.csv")
frame <- subset(frame, remove != "yes") #Remove duplicates
frame <- frame[, c(9:12, 15:16, 18, 20:22)]

################################################################################
### Bayes application
bayes_apply <- function(frame) {
  
  frame <- as.data.frame(frame)
  col_names <- colnames(frame)
  
  complete <- data.table()
  
  for(i in 1:length(col_names)) {
    
    variable <- na.exclude(frame[, col_names[i]])*10000
    
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

################################################################################
### Bayes t.test per realms
frame <- fread("data/TMCF_slope.csv")
frame <- subset(frame, remove != "yes") #Remove duplicates
frame <- frame[, c(7, 9:12, 15:16, 18, 20:22)]

#Function
bayes_apply_realm <- function(frame) {
  
  frame <- as.data.frame(frame)
  col_names <- colnames(frame)[-1]
  col_realms <- unique(frame$realm)[1:4] #remove Oceania
  
  complete <- data.table()
  
  for(i in 1:length(col_realms)) {
    
    sub_realm <- subset(frame, realm == col_realms[i])
    
    for(ii in 1:length(col_names)) {
      
      variable <- na.exclude(sub_realm[, col_names[ii]])*10000
      
      bayes <- bayes.t.test(variable)
      
      results <- bayes$stats
      realm <- col_realms[i]
      parameter <- row.names(results)
      variable <- rep(col_names[ii], 5)
      results <- cbind(realm, variable, parameter, as.data.table(results))
      
      complete <- rbind(complete, results)
      
    }
  }
  return(complete)
}

#Run
bayes_realm_results <- bayes_apply_realm(frame)
fwrite(bayes_realm_results, "data/bayes_realm_results.csv")

#
1.448567

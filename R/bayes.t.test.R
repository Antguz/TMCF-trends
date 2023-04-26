################################################################################
#' @title Function to compare the mean trends from 0 using Bayes
################################################################################

#' @description It computes the mean estimate and the credible intervals of the 
#' climatic trends and evaluate if such means differ from zero.

###Select libraries-------------------------------------------------------------
library(data.table)
library(BayesianFirstAid)
library(MASS)
library(raster)

################################################################################
### Step 1 - Global mean estimated of trends
################################################################################

# This step is focused on compute the mean estimate of the climatic datasets on
# the TMCF, and then test the probability if that estimate is higher or lower than 
# zero.

### Load frame------------------------------------------------------------------
frame <- fread("data/TMCF_slope.csv")
frame <- subset(frame, remove != "yes") #Remove duplicates
weights <- frame$weight
frame <- frame[, c(10:19)] #Select ECVs
frame <- frame*10000 #Scale to x10-4

################################################################################
### Bayes application
bayes_apply <- function(frame) {
  
  # Create a copy to fill it
  frame <- as.data.frame(frame)
  col_names <- colnames(frame)
  complete <- data.table()
  
  # Loop over ECVs
  for(i in 1:length(col_names)) {
    
    # ECV name
    variable <- na.exclude(frame[, col_names[i]])
    
    # Apply bayes
    bayes <- bayes.t.test(variable, weights = weights)
    
    # Get results
    results <- bayes$stats
    parameter <- row.names(results)
    variable <- rep(col_names[i], 5)
    results <- cbind(variable, parameter, as.data.table(results))
    
    # Merge results
    complete <- rbind(complete, results)
    
  }
  
  # Export complete table
  return(complete)
}

# Apply function
bayes_results <- bayes_apply(frame)
bayes_results[, 3:18] <- round(bayes_results[, 3:18], 4)

# Export results
fwrite(bayes_results, "data/bayes_all_results.csv")

################################################################################
### Step 2 - Mean estimated of trends per realm
################################################################################

# This step is similar to the previous step except that it is evaluated at the 
# realm level.

################################################################################
### Bayes t.test per realms
frame <- fread("data/TMCF_slope.csv")
frame <- subset(frame, remove != "yes") #Remove duplicates
frame <- frame[, c(4, 8, 10:19)] #Select ECVs
frame[, 3:12] <- frame[, 3:12]*10000 #Scale to x10-4

#Function
bayes_apply_realm <- function(frame) {
  
  # Make a copy to fill
  frame <- as.data.frame(frame)
  col_names <- colnames(frame)[3:12]
  col_realms <- unique(frame$realm)[1:4] #remove Oceania
  complete <- data.table()
  
  # Loop over realms
  for(i in 1:length(col_realms)) {
    
    # Subset data
    sub_realm <- subset(frame, realm == col_realms[i])
    weight <- sub_realm$weight
    
    # Loop over ECV
    for(ii in 1:length(col_names)) {
      
      variable <- sub_realm[, col_names[ii]]
      
      # Remove missing values
      na_values <- is.na(sub_realm[, col_names[ii]])
      variable <- variable[!na_values]
      sub_weigth <- weight[!na_values]
      
      # Apply bayes
      bayes <- bayes.t.test(variable, weights = sub_weigth)
      
      # Get results
      results <- bayes$stats
      realm <- col_realms[i]
      parameter <- row.names(results)
      variable <- rep(col_names[ii], 5)
      
      # Compile results
      results <- cbind(realm, variable, parameter, as.data.table(results))
      
      # Merge results
      complete <- rbind(complete, results)
      
    }
  }
  
  #Export filled table
  return(complete)
}

# Apply function
bayes_realm_results <- bayes_apply_realm(frame)
bayes_realm_results[, 4:19] <- round(bayes_realm_results[, 4:19], 4)

# Export results
fwrite(bayes_realm_results, "data/bayes_realm_results.csv")

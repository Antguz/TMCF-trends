################################################################################
### Function to compare the mean trend from 0 using Bayes
################################################################################

###Select libraries-------------------------------------------------------------
library(data.table)
library(BayesianFirstAid)
library(MASS)
library(raster)

################################################################################
### step 1 - Global mean estimated of trends
################################################################################

# This step is focused on compute the mean estimate of the climatic datasets on
# the TMCF, and then test the probability if that estimate is higher or lower than 
# zero.

### Load frame------------------------------------------------------------------
frame <- fread("data/TMCF_slope.csv")
frame <- subset(frame, remove != "yes") #Remove duplicates
frame <- frame[, c(9:18)] #Select ECVs
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
    bayes <- bayes.t.test(variable)
    
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

# Export results
fwrite(bayes_results, "data/bayes_results.csv")

################################################################################
### Step 2 - Mean estimated of trends per realm
################################################################################

# This step is similar to the previous step except that it is evaluated at the 
# realm level.

################################################################################
### Bayes t.test per realms
frame <- fread("data/TMCF_slope.csv")
frame <- subset(frame, remove != "yes") #Remove duplicates
frame <- frame[, c(7, 9:18)]
frame[, c(2:11)] <- frame[, c(2:11)]*10000 #Scale

#Function
bayes_apply_realm <- function(frame) {
  
  # Make a copy to fill
  frame <- as.data.frame(frame)
  col_names <- colnames(frame)[-1]
  col_realms <- unique(frame$realm)[1:4] #remove Oceania
  complete <- data.table()
  
  # Loop over realms
  for(i in 1:length(col_realms)) {
    
    # Subset data
    sub_realm <- subset(frame, realm == col_realms[i])
    
    # Loop over ECV
    for(ii in 1:length(col_names)) {
      
      # Remove missing values
      variable <- na.exclude(sub_realm[, col_names[ii]])
      
      # Apply bayes
      bayes <- bayes.t.test(variable)
      
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

# Export results
fwrite(bayes_realm_results, "data/bayes_realm_results.csv")

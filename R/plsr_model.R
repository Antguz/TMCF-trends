################################################################################
### Function to predict the importance and prediction of low cloud fraction
################################################################################

# The aim of these codes is to evaluate if trends of ECVs are able to predict 
# trends of low-cloud fraction, and then see which ECV is the most important
# for doing so. 

### Select libraries------------------------------------------------------------
library(data.table)
library(pls)
library(plsVarSel)

### Load frame------------------------------------------------------------------
frame <- fread("data/TMCF_slope.csv")
frame <- subset(frame, remove != "yes") #Remove duplicates
frame <- frame[, c(9:18)]
frame <- na.exclude(frame)

################################################################################
### step 1 - Estimate the optimal number of components
################################################################################

# As part of the PLSR requirements, this function is focused on estimating the 
# optimal number of components to predict low-cloud fraction.

### Create model----------------------------------------------------------------
# Cross validation
model_comp <- function(frame, iterations = 1000) {
  
  # Empty frame
  complete_frame <- data.table()
  
  # Progress
  pb <- txtProgressBar(min = 1, max = iterations, style = 3)
  
  # Number of iterations
  for(i in 1:iterations) {
    
    setTxtProgressBar(pb, i)
    
    # Set model with cross validation
    model <- plsr(cloud ~ ., 
                  data = frame,
                  scale = TRUE,
                  center = TRUE,
                  ncomp= 9,
                  validation = "CV",
                  trace= FALSE, 
                  method = "oscorespls",
                  segments = 10)
    
    # Export PRESS values
    df <- data.table(t(model$validation$PRESS))
    colnames(df) <- as.character(i)
    
    # Fill frame
    complete_frame <- cbind(complete_frame, df)
    
  }
  
  # Provide number of iteration
  component <- paste0("comp_", 1:9)
  
  # Add components
  complete_frame <- cbind(component = component, complete_frame)
  
  # Export
  return(complete_frame)
  
}

# Apply function
results_components <- model_comp(frame, iterations = 1000) 

# Export the optimal number of components
fwrite(results_components, "data/PLSR_components.csv")

################################################################################
### step 2 - Develop of a PLSR model using 9 components
################################################################################

# The following functions are focused on developing the final model knowing 
# that nine components are required to reduce the RMSE of it. 

# Performance function
performance <- function(observed, predicted) {
  
  # Make a copy
  obser <- observed
  pred <- predicted
  
  # Get performance
  rss <- sum((obser - pred) ^ 2)  ## residual sum of squares
  tss <- sum((obser - mean(obser))^2)  ## total sum of squares
  
  # Parameters of performance
  n <- length(predicted)
  Rsq <- 1 - (rss/tss)
  Bias <- mean(((obser - pred)/obser))
  RMSE <- sqrt(mean((obser - pred)^2))
  RMSE_P <- (RMSE/(max(obser)-min(obser)))*100
  
  # Return values
  return(round(c(N = n, Rsq = Rsq, Bias = Bias, RMSE = RMSE, RMSE_P = RMSE_P), 6))
  
}

# Predicted model
predicted_model <- function(frame, ncomp = 9, fraction = 0.5, iterations = 10000) {
  
  # Empty frames to fill
  predicted_frame <- data.table()
  performance_frame <- data.table()
  VIP_frame <- data.table()
  coefficients_frame <- data.table()
  
  # Progress
  pb <- txtProgressBar(min = 1, max = iterations, style = 3)
  
  # Loop over iterations
  for(i in 1:iterations) {
    
    setTxtProgressBar(pb, i)
    
    # Split data
    sub_sample <- frame[sample(nrow(frame), floor(nrow(frame)*fraction), 
                               replace = FALSE), ] 
    
    # Develop model
    model <- plsr(cloud ~ ., 
                  data = sub_sample,
                  scale = TRUE,
                  center = TRUE,
                  ncomp= ncomp,
                  validation = "none",
                  trace= FALSE, 
                  method = "oscorespls")
    
    # Return predicted
    value <- predict(model, newdata = frame, ncomp = ncomp, type= "response")[,,1]
    df <- as.data.table(matrix(value, ncol = 1))
    colnames(df) <- as.character(i)
    predicted_frame <- cbind(predicted_frame, df)
    
    # Return performance
    perf <- as.data.table(matrix(c(i,performance(frame$cloud, value)), nrow = 1))
    performance_frame <- rbind(performance_frame, perf)
    
    # Return VIP_frame
    vip <- as.data.table(matrix(c(i, VIP(model, opt.comp = ncomp)), nrow = 1))
    VIP_frame <- rbind(VIP_frame, vip)
    
    # Coefficients
    coeff <- as.data.table(matrix(c(i, coef(model, ncomp = ncomp, intercept=TRUE)), nrow = 1))
    coefficients_frame <- rbind(coefficients_frame, coeff)
    
  }
  
  # Provide names to performance
  colnames(performance_frame) <- c("iteration", "N", "Rsq", 
                                   "Bias", "RMSE", "RMSE_P")
  
  # Provide names to VIP
  colnames(VIP_frame) <- c("iteration", "temperature", "min_temperature",
                           "max_temperature", "dewpoint", "pressure", 
                           "precipitation", "vswc", "ET", "PET")
  
  # Provide names coefficients
  colnames(coefficients_frame) <- c("iteration", "intercept", "temperature", 
                                    "min_temperature", "max_temperature", 
                                    "dewpoint", "pressure", "precipitation", 
                                    "vswc", "ET", "PET")
  
  # Return compiled results
  results <- list(predicted = predicted_frame,
                  performance = performance_frame,
                  VIP = VIP_frame,
                  Coefficients = coefficients_frame)
  
  return(results)
  
}

# Apply function
results_model <- predicted_model(frame, ncomp = 9, fraction = 0.5, iterations = 5000)

# Export results
fwrite(results_model$predicted, "data/PLSR_predicted.csv")
fwrite(results_model$performance, "data/PLSR_performance.csv")
fwrite(results_model$VIP, "data/PLSR_VIP.csv")
fwrite(results_model$Coefficients, "data/PLSR_coefficients.csv")
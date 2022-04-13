################################################################################
### Function to predict the importance and prediction of low cloud fraction
################################################################################

###Select libraries-------------------------------------------------------------
library(data.table)
library(pls)
library(plsVarSel)

###Load frame-------------------------------------------------------------------
frame <- fread("data/TMCF_slope.csv")
frame <- subset(frame, remove != "yes") #Remove duplicates
frame <- frame[, c(9:12, 15, 16, 18, 20, 21, 22)]
frame <- na.exclude(frame)

###Create model-----------------------------------------------------------------
#Cross validation
model_comp <- function(frame, iterations = 1000) {
  
  complete_frame <- data.table()
  
  pb <- txtProgressBar(min = 1, max = iterations, style = 3)
  
  for(i in 1:iterations) {
    
    setTxtProgressBar(pb, i)
    
    model <- plsr(cloud ~ ., 
                  data = frame,
                  scale = TRUE,
                  center = TRUE,
                  ncomp= 9,
                  validation = "CV",
                  trace= FALSE, 
                  method = "oscorespls",
                  segments = 10)
    
    df <- data.table(t(model$validation$PRESS))
    colnames(df) <- as.character(i)
    
    complete_frame <- cbind(complete_frame, df)
    
  }
  
  component <- paste0("comp_", 1:9)
  
  complete_frame <- cbind(component = component, complete_frame)
  
  return(complete_frame)
  
}

results_components <- model_comp(frame, iterations = 1000) 
fwrite(results_components, "data/PLSR_components.csv")

#Performance function
performance <- function(observed, predicted) {
  
  obser <- observed
  pred <- predicted
  
  rss <- sum((obser - pred) ^ 2)  ## residual sum of squares
  tss <- sum((obser - mean(obser))^2)  ## total sum of squares
  
  n <- length(predicted)
  Rsq <- 1 - (rss/tss)
  Bias <- mean(((obser - pred)/obser))
  RMSE <- sqrt(mean((obser - pred)^2))
  RMSE_P <- (RMSE/(max(obser)-min(obser)))*100
  
  return(round(c(N = n, Rsq = Rsq, Bias = Bias, RMSE = RMSE, RMSE_P = RMSE_P), 6))
  
}

#Predicted model
predicted_model <- function(frame, ncomp = 9, fraction = 0.5, iterations = 10000) {
  
  predicted_frame <- data.table()
  performance_frame <- data.table()
  VIP_frame <- data.table()
  coefficients_frame <- data.table()
  
  pb <- txtProgressBar(min = 1, max = iterations, style = 3)
  
  for(i in 1:iterations) {
    
    setTxtProgressBar(pb, i)
    
    sub_sample <- frame[sample(nrow(frame), floor(nrow(frame)*fraction), 
                               replace = FALSE), ] 
    
    model <- plsr(cloud ~ ., 
                  data = sub_sample,
                  scale = TRUE,
                  center = TRUE,
                  ncomp= ncomp,
                  validation = "none",
                  trace= FALSE, 
                  method = "oscorespls")
    
    #Return predicted
    value <- predict(model, newdata = frame, ncomp = ncomp, type= "response")[,,1]
    df <- as.data.table(matrix(value, ncol = 1))
    colnames(df) <- as.character(i)
    predicted_frame <- cbind(predicted_frame, df)
    
    #Return performance
    perf <- as.data.table(matrix(c(i,performance(frame$cloud, value)), nrow = 1))
    performance_frame <- rbind(performance_frame, perf)
    
    #Retrun VIP_frame
    vip <- as.data.table(matrix(c(i, VIP(model, opt.comp = ncomp)), nrow = 1))
    VIP_frame <- rbind(VIP_frame, vip)
    
    #Coefficients
    coeff <- as.data.table(matrix(c(i, coef(model, ncomp = ncomp, intercept=TRUE)), nrow = 1))
    coefficients_frame <- rbind(coefficients_frame, coeff)
    
  }
  
  colnames(performance_frame) <- c("iteration", "N", "Rsq", 
                                   "Bias", "RMSE", "RMSE_P")
  
  
  colnames(VIP_frame) <- c("iteration", "temperature", "min_temperature",
                           "max_temperature", "dewpoint", "pressure", 
                           "precipitation", "vswc", "ET", "PET")
  
  colnames(coefficients_frame) <- c("iteration", "intercept", "temperature", 
                                    "min_temperature", "max_temperature", 
                                    "dewpoint", "pressure", "precipitation", 
                                    "vswc", "ET", "PET")
  
  results <- list(predicted = predicted_frame,
                  performance = performance_frame,
                  VIP = VIP_frame,
                  Coefficients = coefficients_frame)
  
  return(results)
  
}

results_model <- predicted_model(frame, ncomp = 9, fraction = 0.5, iterations = 5000)
a <- results_model$predicted
results_model$performance
results_model$VIP
results_model$Coefficients
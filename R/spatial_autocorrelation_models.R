################################################################################
##### Statistical assessment of the relationships with spatial autocorrelation
################################################################################

#Load library-------------------------------------------------------------------
library(data.table)
library(nlme)
library(MuMIn)
library(effects)
library(stats)
library(AICcmodavg)

#Load data----------------------------------------------------------------------
data <- fread("data/TMCF_slope.csv")
data <- subset(data, remove != "yes") #Remove duplicates
data <- subset(data, realm != "Oceania") #Remove Hawaii

#Function to test best autocorrelation model between ECV
auto <- function(data, column = 18) {
  
  frame <- as.data.frame(data)
  frame <- as.data.table(frame[, c(1, 2, 3, 7, 9, column)])
  names(frame)[6] <- "ECV"
  frame <- na.exclude(frame)
  
  #Control function
  ctrl <- lmeControl(opt='optim')
  
  #Models
  cloud.model <- lme(fixed = cloud ~ ECV, data = frame, random = ~ 1 + ECV|realm, 
                     control=ctrl, method = "ML") 
  
  #Gaussian
  cloud.gau <- update(cloud.model, 
                      correlation = corGaus(1, form = ~ longitude + latitude), 
                      control=ctrl, method = "ML")
  
  #Exponential
  cloud.exp <- update(cloud.model, 
                      correlation = corExp(1, form = ~ longitude + latitude), 
                      control=ctrl, method = "ML")
  
  #Linear
  cloud.lin <- update(cloud.model, 
                      correlation = corLin(1, form = ~ longitude + latitude), 
                      control=ctrl, method = "ML")
  
  #Spherical
  cloud.spher <- update(cloud.model, 
                        correlation = corSpher(1, form = ~ longitude + latitude), 
                        control=ctrl, method = "ML")
  
  models <- list(cloud.model, cloud.gau, cloud.exp, cloud.gau, cloud.lin, cloud.spher)
  
  model.names <- c("cloud.model", "cloud.gau", "cloud.exp", "cloud.gau", "cloud.lin", "cloud.spher")
  
  result <- aictab(cand.set = models, modnames = model.names)
  
  return(result)
  
}
  
#Temperature
temp <- auto(data, column = 10)

#Min_temperature
min_temp <- auto(data, column = 11)

#Max_temeprature
max_temp <- auto(data, column = 12)

#tr
tr <- auto(data, column = 13)

#strd
strd <- auto(data, column = 14)

#dewpoint
dewpoint <- auto(data, column = 15)

#pressure
pressure <- auto(data, column = 16)

#ssrd
ssrd <- auto(data, column = 17)

#precipitation
precp <- auto(data, column = 18)

#rwca
rwca <- auto(data, column = 19)

#vswc
vswc <- auto(data, column = 20)

##### Conclusion----------------------------------------------------------------
#Based on the AIC, all the models benefit from having an exponential spatial
#autocorrelation with them.


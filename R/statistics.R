################################################################################
##### Statistical assessment of the relationships with exponential autocorrelation
################################################################################

#Load library-------------------------------------------------------------------
library(data.table)
library(nlme)
library(MuMIn)
library(effects)
library(stats)
library(ggplot2)
library(ggpubr)
library(sjPlot)

#Load data----------------------------------------------------------------------
data <- fread("data/TMCF_slope.csv")
data <- subset(data, remove != "yes") #Remove duplicates
data <- subset(data, realm != "Oceania") #Remove Hawaii

#Note------------------------------------------------------------------------
#Knowing that the exponential autocorrelation leads to models with lower ACI, the
#resulting comparisons will be focused on extracting the slopes and intercept
#using linear mixed effects by fighting a model with a covariance between 
#intercept and slope.

#Models-------------------------------------------------------------------------
#Function
model_lme <- function(data, column = 18) {
  
  frame <- as.data.frame(data)
  frame <- as.data.table(frame[, c(1, 2, 3, 7, 9, column)])
  cat(paste0("cloud ~ ", names(frame)[6]))
  names(frame)[6] <- "ECV"
  frame <- na.exclude(frame)
  
  #Control function
  ctrl <- lmeControl(maxIter = 1000,
                     msMaxIter = 1000,
                     niterEM = 1000,
                     msMaxEval = 1000,
                     opt='nlminb',
                     msVerbose = TRUE)
  
  #Models
  cloud.model <- lme(fixed = cloud ~ ECV, 
                     data = frame, 
                     random = ~ 1 + ECV|realm,
                     correlation = corExp(1, form = ~ longitude + latitude),
                     control=ctrl, 
                     method = "ML") 
  
  return(cloud.model)
  
}

#Run models---------------------------------------------------------------------
#Temperature
temp <- model_lme(data, column = 10)

#Min_temperature
min_temp <- model_lme(data, column = 11)

#Max_temeprature
max_temp <- model_lme(data, column = 12)

#dewpoint
dewpoint <- model_lme(data, column = 15)

#precipitation
precipitation <- model_lme(data, column = 18)

#vswc
vswc <- model_lme(data, column = 20)

#Create list of models
models <- list(temperature = temp,
               min_temperature = min_temp,
               max_temperature = max_temp,
               dewpoint = dewpoint,
               precipitation = precipitation,
               vswc = vswc)

#Evaluate model statistics------------------------------------------------------
#QQplot function
qqplot_data <- function(model) {
  
  vec <- resid(model)  
  
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  ggplot(d, aes(sample = resids)) + stat_qq(colour = I("grey90"), shape = I(21), fill = I("#0072B2"), size = I(1.5)) + geom_abline(slope = slope, intercept = int) +
    xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_y_continuous(limits = c(-0.0067, 0.0067), expand = c(0,0))
  
}

#Histogram standardized residuals distribution
hist_distribution <- function(model) {
  
  vec <- resid(model)
  
  x <- (vec - mean(vec)) / sd(vec)
  d <- data.frame(std_resids = x )
  
  ggplot(d, aes(x = std_resids)) + 
    geom_histogram(aes(y = ..density..), alpha = 0.4, colour="black", fill="grey75", bins = 30) +
    stat_function(fun=dnorm, args=list(mean=mean(d$std_resids), sd=sd(d$std_resids)), colour="#0072B2") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(limits = c(-5.1, 5.1), expand = c(0,0), n.breaks = 3) +
    scale_y_continuous(limits = c(0, 0.6), expand = c(0,0)) +
    ylab("Density") + xlab("Standarized residuals")

}

#Standardized residuals and predicted plot
stand_resid_predict <- function(model) {
  
  vec <- resid(model)
  x <- (vec - mean(vec, na.rm = TRUE)) / sd(vec, na.rm = TRUE)
  
  predicted <- round(predict(model)*100, 6) 
  realm <- names(predicted)
  
  d <- data.frame(std_resids = x, predicted = predicted, realm = realm)
  
  ggplot(d) + 
    geom_hline(yintercept= 0, linetype="dashed", color = "grey") +
    geom_vline(xintercept= 0, linetype="dashed", color = "grey") +
    geom_point(aes(x = predicted, y = std_resids, shape = realm, colour = realm)) +
    scale_shape_manual("Realms", values=c(21, 24, 25, 22), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia')) +
    scale_colour_manual("Realms", values=c('#228b22', "#a65628", '#e6ab02', '#984ea3'), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia')) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ylab("Standarized residuals") + 
    xlab(expression(paste(Delta, "CF (x10"^-2, " CF year"^-1, ")", sep = ""))) +
    scale_x_continuous(limits = c(-0.6, 0.3), expand = c(0,0), n.breaks = 5) +
    scale_y_continuous(limits = c(-5.02, 5.02), expand = c(0,0), n.breaks = 5) +
    theme(legend.position = "none")
  
}

#Plots to export
widths_value <- c(2, 1.25, 1.25)
heights_value <- c(1)

#Temperature
temp_plot <- ggarrange(stand_resid_predict(models[[1]]),
                       hist_distribution(models[[1]]),
                       qqplot_data(models[[1]]),
                       nrow = 1,
                       ncol = 3,
                       common.legend = FALSE,
                       widths = widths_value, heights = heights_value)

#Min_temperature
min_temp_plot <- ggarrange(stand_resid_predict(models[[2]]),
                           hist_distribution(models[[2]]),
                           qqplot_data(models[[2]]),
                           nrow = 1,
                           ncol = 3,
                           common.legend = FALSE,
                           widths = widths_value, heights = heights_value)

#Max_temeprature
max_temp_plot <- ggarrange(stand_resid_predict(models[[3]]),
                           hist_distribution(models[[3]]),
                           qqplot_data(models[[3]]),
                           nrow = 1,
                           ncol = 3,
                           common.legend = FALSE,
                           widths = widths_value, heights = heights_value)

#dewpoint
dewpoint_plot <- ggarrange(stand_resid_predict(models[[4]]),
                           hist_distribution(models[[4]]),
                           qqplot_data(models[[4]]),
                           nrow = 1,
                           ncol = 3,
                           common.legend = FALSE,
                           widths = widths_value, heights = heights_value)

#precipitation
precipitation_plot <- ggarrange(stand_resid_predict(models[[5]]),
                                hist_distribution(models[[5]]),
                                qqplot_data(models[[5]]),
                                nrow = 1,
                                ncol = 3,
                                common.legend = FALSE,
                                widths = widths_value, heights = heights_value)

#vswc
vswc_plot <- ggarrange(stand_resid_predict(models[[6]]),
                       hist_distribution(models[[6]]),
                       qqplot_data(models[[6]]),
                       nrow = 1,
                       ncol = 3,
                       common.legend = FALSE,
                       widths = widths_value, heights = heights_value)

###Figure S2--------------------------------------------------------------------
FS3_plot <- ggarrange(temp_plot,
                      min_temp_plot,
                      max_temp_plot,
                      dewpoint_plot,
                      precipitation_plot,
                      vswc_plot,
                      nrow = 6,
                      ncol = 1,
                      common.legend = TRUE,
                      widths = sum(widths_value), 
                      heights = rep(heights_value, length(models)))

jpeg("Figure_S3.jpg", width = 183 , height = 300, units = "mm", pointsize = 12, quality = 100, res = 600)

FS3_plot

dev.off()

###Extraction of statistics for reporting-----------------------------
#Select the model

model <- models[[6]]  
round(as.data.table(intervals(model, level = 0.95, which = "fixed")[1]$fixed)*1000, 2)
tab_model(model, digits = 7, digits.p = 7, digits.re = 7)


  frame <- data.table(Argument = c("Intercept", "ECV"))
  
  coefficients <- as.data.table(model$coefficientsfixed$interval$fixed)
  
  vcov <- as.data.table(vcov(model))
  colnames(vcov) <- c("vcov_intercept", "vcov_ECV")
  
  vc <- VarCorr(model)
  
  varests <- as.numeric(VarCorr(model)[1:2])
  ICC <- varests[1]/sum(varests)
  
  test <- as.data.table(anova(model, type = "marginal"))
  
  r2 <- as.data.table(t(r.squaredGLMM(model)))
  colnames(r2) <- c("r2-m/c")
  
  frame <- cbind(frame,
                 vcov,
                 test,
                 r2)
  
  export(frame)

}

stats_random <- function(model) {
  
  realms <- data.table(realms = c("Australasia", "Indomalayan", "Neotropic", "Paleartic"))
  coefficients <- coef(model)
  colnames(coefficients) <- c("Intercept", "ECV")
  random_effects <- random.effects(model)
  colnames(random_effects) <- c("ranef_Intercept", "ranef_ECV")
}

plot(model)
qqnorm(model, ~ranef(., level=2))
qqnorm(resid(model))
qqline(resid(model))
plot(ranef(model))
intervals(temp, which = "fixed") #Confidence Intervals on lme Parameters 
r.squaredGLMM(models[[1]]) #Pseudo-R-squared for Generalized Mixed-Effect models 
ACF(model)
coef(model) #Coefficients
vcov(temp)
anova(model, type = "marginal")

intervals(model)








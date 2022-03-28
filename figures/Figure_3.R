################################################################################
### Function for plotting results from PLSR model
################################################################################

###Select libraries-------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggpubr)

###Plot features----------------------------------------------------------------
#Theme
th <- theme(
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "transparent", color = NA),
  legend.background = element_rect(fill = "transparent"),
  legend.box.background = element_rect(fill = "transparent"),
  panel.spacing = unit(0,"null"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())

################################################################################
#Plot of PRESS values

#Melt
press <- melt(results_components, 
              id.vars = c("component"),
              measure.vars = c(.SD), 
              variable.name = "iteration",
              value.name = "PRESS")
press$PRESS <- press$PRESS*10000 #Scale press

#Summary
press_summary <- press[, list(mean = mean(PRESS), 
                              quantile05 = quantile(PRESS, 0.05),
                              quantile95 = quantile(PRESS, 0.95),
                              sd = sd(PRESS)),
                       by = "component"]
press_summary$component <- 1:nrow(press_summary)

#Plot
press_plot <- ggplot(press_summary) +
  geom_ribbon(aes(x = component, ymin = quantile05, ymax = quantile95), 
              fill = "#659ca2ff", alpha = 0.5) +
  geom_errorbar(aes(x = component, ymin=mean-sd, ymax=mean+sd), width=.01) +
  geom_line(aes(x = component, y = mean), 
            colour = "#659ca2ff", linetype = "dashed") +
  geom_point(aes(x = component, y = mean), 
             shape = 21, size = 2, fill = "white") +
  xlab("Components") + 
  ylab(expression(paste("RMSEP (x10"^-4, " CF year"^-1, ")", sep = ""))) + 
  scale_x_continuous(limits = c(0.95, 9.05), expand = c(0,0), breaks = 1:9) +
  scale_y_continuous(limits = c(5, 7), expand = c(0,0), breaks = c(5, 6, 7)) +
  theme_bw() + th

################################################################################
#Plot VIP
vip <- fread("data/PLSR_VIP.csv")
vip <- results_model$VIP

#Melt
vip_melt <- melt(vip, 
                 id.vars = c("iteration"),
                 measure.vars = c(.SD), 
                 variable.name = "ECV",
                 value.name = "VIP")

#Summary
vip_summary <- vip_melt[, list(mean = mean(VIP), 
                               sd = sd(VIP)),
                       by = "ECV"]

labels <- c(expression(paste(Delta, "VSWC", sep = "")),
            expression(paste(Delta, "Temperature"['max'], sep = "")),
            expression(paste(Delta, "Temperature"['min'], sep = "")),
            expression(paste(Delta, "Dewpoint", sep = "")),
            expression(paste(Delta, "Temperature", sep = "")),
            expression(paste(Delta, "Pressure", sep = "")),
            expression(paste(Delta, "ET", sep = "")),
            expression(paste(Delta, "PET", sep = "")),
            expression(paste(Delta, "Precipitation", sep = "")))
            
              

vip_summary <- vip_summary[order(mean)]
ECV <- rev(as.character(vip_summary$ECV))
vip_summary$ECV <- factor(vip_summary$ECV, level = ECV,
                          labels = labels)

#Plot VIP
vip_plot <- ggplot(vip_summary, aes(x=ECV, y=mean)) + 
  geom_bar(stat="identity", color="gray", fill = "#659ca2ff", alpha = 0.75) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width= 0.01) +
  ylab("VIP") + xlab("ECV") +
  scale_x_discrete(labels = labels) + 
  scale_y_continuous(limits = c(0, 1.75), expand = c(0,0)) + 
  theme_bw() + th + coord_flip()


################################################################################
#Plot predicted values
predicted <- fread("data/PLSR_predicted.csv")
predicted <- predicted*10000
predicted$value <- 1:nrow(predicted)

#Melt
predicted_melt <- melt(predicted, 
                 id.vars = c("value"),
                 measure.vars = c(.SD), 
                 variable.name = "iteration",
                 value.name = "predicted")

predicted_melt <- predicted_melt[iteration != "id"]

#Summary
predicted_summary <- predicted_melt[, list(mean = mean(predicted), 
                                           sd = sd(predicted)),
                                           by = "value"]
frame <- fread("data/TMCF_slope.csv")
frame <- subset(frame, remove != "yes")
predicted_summary$observed <- frame$cloud*10000

#Plot predicted
predicted_plot <- ggplot(predicted_summary, aes(x=mean, y=observed)) + 
  geom_abline(intercept = 0, slope = 1, colour = "grey", linetype = "dotted") +
  geom_errorbar(aes(xmin=mean-sd, xmax=mean+sd), width= 0.01, colour = "grey", alpha = 0.75) +
  geom_point(shape = 21, size = 2.5, colour = "white", fill ="#659ca2ff", alpha = 0.75) +
  scale_y_continuous(limits = c(-66, 66), expand = c(0,0)) + 
  scale_x_continuous(limits = c(-66, 66), expand = c(0,0)) + 
  xlab(expression(paste("Predicted (x10"^-4, " CF year"^-1, ")", sep = ""))) +
  ylab(expression(paste("Observed (x10"^-4, " CF year"^-1, ")", sep = ""))) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, colour = "black", size = 0.4) +
  theme_bw() + th

################################################################################
#Plot performance values
performance <- fread("data/PLSR_performance.csv")
performance <- results_model$performance
performance$RMSE <- performance$RMSE*10000
mean_value <- quantile(performance$RMSE, 0.5)

#Plot
performance_plot <- ggplot(performance, aes(x = RMSE)) +
  geom_histogram(bins = 30, color="gray", fill = "#659ca2ff", alpha = 0.75) +
  scale_y_continuous(limits = c(0, 1000), expand = c(0, 0.5), n.breaks = 4) +   
  xlab(expression(paste("RMSE (x10"^-4, " CF year"^-1, ")", sep = ""))) + 
  geom_vline(xintercept = mean_value, linetype= "dashed") +
  ylab("Frequency") +
  theme_bw() + theme(legend.position = "none") + th

###Arrange plot-----------------------------------------------------------------
yo <- ggarrange(press_plot,
                vip_plot,
                predicted_plot,
                performance_plot,
                nrow = 2,
                ncol = 2,
                labels = c("a", "b", "c", "d"), 
                common.legend = TRUE,
                widths = c(3, 3), heights = c(3, 3))

###Export-----------------------------------------------------------------------
tiff("Figure_3.tiff", width = 183, height = 150, units = "mm", pointsize = 12, res = 600)

yo

dev.off()

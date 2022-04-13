################################################################################
### Function for plotting distributions of ECV on TMCF
################################################################################

###Select libraries-------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggpubr)

###Load data and layers---------------------------------------------------------
#Load ASCI of trends
frame <- as.data.frame(fread("data/TMCF_slope.csv"))
frame <- subset(frame, remove != "yes") #Remove duplicates

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

#Color selection
colores <- rev(c("#67001f", "#b2182b", "#d6604d", "#f4a582", 
                 "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", 
                 "#4393c3", "#2166ac", "#053061"))

####Histograms------------------------------------------------------------------
hist_function <- function(variable, name, y_limits, mean, LCI, HCI) {
  
  variable_limits <- c(-max(abs(range(frame[, variable], na.rm = TRUE))), 
                       max(abs(range(frame[, variable], na.rm = TRUE))))
  variable_quantile <- quantile(frame[, variable], 0.5, na.rm = TRUE)
  
  plot_export <- ggplot(frame, aes(x = frame[, variable])) +
    geom_histogram(aes(fill = ..x.., color = ..x..), bins = 30, colour = "grey75") +
    scale_y_continuous(limits = y_limits, expand = c(0, 0.5), n.breaks = 4) +   
    scale_x_continuous(limits = variable_limits, expand = c(0, 0)) +
    scale_fill_gradientn(colours = colores, limits = variable_limits) +
    geom_vline(xintercept = mean, linetype= "longdash", colour = "grey20") +
    geom_vline(xintercept = LCI, linetype= "dotted", colour = "grey20") +
    geom_vline(xintercept = HCI, linetype= "dotted", colour = "grey20") +
    xlab(name) + 
    ylab("Frequency") +
    theme_classic() + theme(legend.position = "none") + th +
    theme(
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA))
  
}



#Temperature
temperature <- hist_function("temperature",
                             expression(paste(Delta, "Temperature"['avg']," (K year"^-1, ")", sep = "")),
                             c(0, 120),
                             0.03103071,
                             0.02971575,
                             0.03228193)

#Temperature min
min_temperature <- hist_function("min_temperature",
                             expression(paste(Delta, "Temperature"['min']," (K year"^-1, ")", sep = "")),
                             c(0, 120),
                             0.01928038,
                             0.01863102,
                             0.0199509)

#Temperature max
max_temperature <- hist_function("max_temperature",
                             expression(paste(Delta, "Temperature"['max']," (K year"^-1, ")", sep = "")),
                             c(0, 120),
                             0.0187762,
                             0.0180836,
                             0.01950891)

#Precipitation
precipitation <- hist_function("precipitation",
                               expression(paste(Delta, "Precipitation (mm day"^-1, "year"^-1, ")", sep = "")),
                               c(0, 120),
                               -0.001942869,
                               -0.004725074,
                               0.0007131467)

#Dewpoint
dewpoint <- hist_function("dewpoint",
                          expression(paste(Delta, "Dewpoint (K year"^-1, ")", sep = "")),
                          c(0, 120),
                          0.03053587,
                          0.0291526,
                          0.03192858)

#Pressure
pressure <- hist_function("pressure",
                          expression(paste(Delta, "Pressure (Pa year"^-1, ")", sep = "")),
                          c(0, 120),
                          1.518385,
                          1.44925,
                          1.587649)
#VSWC
vswc <- hist_function("vswc",
                      expression(paste(Delta, "VSWC (m"^3,"m"^-3, " year"^-1, ")", sep = "")),
                      c(0, 160),
                      0.000055236397,
                      0.000011446997853383,
                      0.000096916527331151)

#ET
ET <- hist_function("ET",
                    expression(paste(Delta, "ET (mm month"^-1, " year"^-1, ")", sep = "")),
                    c(0, 160),
                    0.000025566240612677,
                    -0.000003829778800594,
                    0.000055163888778894)

#PET
PET <- hist_function("PET",
                     expression(paste(Delta, "PET (mm month"^-1, " year"^-1, ")", sep = "")),
                    c(0, 160),
                    0.331283388621889,
                    0.313842646321805,
                    0.349115655259898)

###Arrange plot-----------------------------------------------------------------
yo <- ggarrange(temperature, min_temperature, max_temperature,
                precipitation, dewpoint, pressure,
                vswc, ET, PET,
                nrow = 3,
                ncol = 3,
                labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i"), 
                common.legend = FALSE,
                widths = c(3, 3, 3), heights = c(3, 3, 3))

###Export-----------------------------------------------------------------------
tiff("Figure_2.tiff", width = 183, height = 150, units = "mm", pointsize = 12, res = 600)

yo

dev.off()

################################################################################
### Function for plotting distributions of ECV on TMCF
################################################################################

###Select libraries-------------------------------------------------------------
library(data.table)
library(ggplot2)

###Load data and layers---------------------------------------------------------
#Load ASCI of trends
frame <- fread("data/TMCF_slope.csv")
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
hist_function <- function(variable, name, y_limits) {
  
  variable_limits <- c(-max(abs(range(frame[, variable], na.rm = TRUE))), 
                       max(abs(range(frame[, variable], na.rm = TRUE))))
  variable_quantile <- quantile(frame[, variable], 0.5, na.rm = TRUE)
  
  plot_export <- ggplot(frame, aes(x = frame[, variable])) +
    geom_histogram(aes(fill = ..x.., color = ..x..), bins = 30, colour = "grey75") +
    scale_y_continuous(limits = y_limits, expand = c(0, 0.5), n.breaks = 4) +   
    scale_x_continuous(limits = variable_limits, expand = c(0, 0)) +
    scale_fill_gradientn(colours = colores, limits = variable_limits) +
    geom_vline(xintercept = variable_quantile, linetype= "dashed", colour = "grey20") +
    xlab(name) + 
    ylab("Frequency") +
    theme_classic() + theme(legend.position = "none") + th +
    theme(
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA))
  
}



#Temperature
temperature <- hist_function("temperature",
                             expression(paste(Delta, "Temperature (K year"^-1, ")", sep = "")),
                             c(0, 120))

#Temperature min
min_temperature <- hist_function("min_temperature",
                             expression(paste(Delta, "Temperature"['min']," (K year"^-1, ")", sep = "")),
                             c(0, 120))

#Temperature max
max_temperature <- hist_function("max_temperature",
                             expression(paste(Delta, "Temperature"['max']," (K year"^-1, ")", sep = "")),
                             c(0, 120))

#Precipitation
precipitation <- hist_function("precipitation",
                               expression(paste(Delta, "Precipitation (mm day"^-1, "year"^-1, ")", sep = "")),
                               c(0, 120))

#Dewpoint
dewpoint <- hist_function("dewpoint",
                          expression(paste(Delta, "Dewpoint (K year"^-1, ")", sep = "")),
                          c(0, 120))

#Pressure
pressure <- hist_function("pressure",
                          expression(paste(Delta, "Pressure (Pa year"^-1, ")", sep = "")),
                          c(0, 120))
#VSWC
vswc <- hist_function("vswc",
                      expression(paste(Delta, "VSWC (m"^3,"m"^-3, " year"^-1, ")", sep = "")),
                      c(0, 160))

#ET
ET <- hist_function("ET",
                    expression(paste(Delta, "ET (mm month"^-1, " year"^-1, ")", sep = "")),
                    c(0, 160))

#PET
PET <- hist_function("PET",
                     expression(paste(Delta, "PET (mm month"^-1, " year"^-1, ")", sep = "")),
                    c(0, 160))

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
jpeg("Figure_2.jpg", width = 183, height = 150, units = "mm", pointsize = 12, quality = 100, res = 600)

yo

dev.off()

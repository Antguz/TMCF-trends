################################################################################
#' @title Function for plotting distributions of ECV on TMCF
################################################################################

#' @description Figure 3 of the manuscript

###Select libraries-------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggpubr)

### Load data and layers--------------------------------------------------------
# Load ASCI of trends
frame <- as.data.frame(fread("data/TMCF_slope.csv"))
frame <- subset(frame, remove != "yes") #Remove duplicates

###Plot features ---------------------------------------------------------------
# Theme
th <- theme(
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "transparent", color = NA),
  legend.background = element_rect(fill = "transparent"),
  legend.box.background = element_rect(fill = "transparent"),
  panel.spacing = unit(0,"null"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())

# Color selection
colores <- rev(c("#67001f", "#b2182b", "#d6604d", "#f4a582", 
                 "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", 
                 "#4393c3", "#2166ac", "#053061"))

###Histograms function ----------------------------------------------------------
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
                             310.2092/10000,
                             297.6825/10000,
                             322.8063/10000)

#Temperature min
min_temperature <- hist_function("min_temperature",
                             expression(paste(Delta, "Temperature"['min']," (K year"^-1, ")", sep = "")),
                             c(0, 120),
                             192.7926/10000,
                             186.1727/10000,
                             199.3886/10000)

#Temperature max
max_temperature <- hist_function("max_temperature",
                             expression(paste(Delta, "Temperature"['max']," (K year"^-1, ")", sep = "")),
                             c(0, 120),
                             187.865/10000,
                             180.7394/10000,
                             195.0721/10000)

#Precipitation
precipitation <- hist_function("precipitation",
                               expression(paste(Delta, "Precipitation (mm day"^-1, "year"^-1, ")", sep = "")),
                               c(0, 120),
                               -19.5259/10000,
                               -46.6009/10000,
                               7.6699/10000)

#Dewpoint
dewpoint <- hist_function("dewpoint",
                          expression(paste(Delta, "Dewpoint (K year"^-1, ")", sep = "")),
                          c(0, 120),
                          305.2824/10000,
                          291.4405/10000,
                          319.3143/10000)

#Pressure
pressure <- hist_function("pressure",
                          expression(paste(Delta, "Pressure (Pa year"^-1, ")", sep = "")),
                          c(0, 120),
                          15183.9223/10000,
                          14495.0049/10000,
                          15867.1632/10000)
#VSWC
vswc <- hist_function("vswc",
                      expression(paste(Delta, "VSWC (m"^3,"m"^-3, " year"^-1, ")", sep = "")),
                      c(0, 160),
                      0.5564/10000,
                      0.1298/10000,
                      0.986/10000)

#ET
PET <- hist_function("PET",
                     expression(paste(Delta, "PET (mm month"^-1, " year"^-1, ")", sep = "")),
                     c(0, 160),
                     0.2549/10000,
                     -0.0341/10000,
                     0.5646/10000)


###Arrange plot-----------------------------------------------------------------
main <- ggarrange(temperature, min_temperature, max_temperature,
                  precipitation, dewpoint, pressure,
                  vswc, PET,
                  nrow = 3,
                  ncol = 3,
                  labels = c("a", "b", "c", "d", "e", "f", "g", "h"), 
                  common.legend = FALSE,
                  widths = c(3, 3, 3), heights = c(3, 3, 3))

###Export-----------------------------------------------------------------------
tiff("Figure_3.tiff", width = 183, height = 150, units = "mm", pointsize = 12, res = 600)

main

dev.off()

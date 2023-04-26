################################################################################
#' @title Function for plotting trends in low clouds
################################################################################

#' @description Figure 2 of the manuscript.

###Select libraries-------------------------------------------------------------
library(data.table)
library(ggplot2)
library(raster)
library(rnaturalearth)
library(sf)
library(e1071)

library(scales)
library(spatstat)
library(graticule)
library(ggridges)
library(dplyr)


###Source code -----------------------------------------------------------------
source("R/area_weigth.R")

### TMCF cloud fraction --------------------------------------------------------
# Load ASCI of trends
frame <- fread("data/TMCF_slope.csv")
frame <- subset(frame, remove != "yes") #Remove duplicates
frame$cloud <- frame$cloud*10000

### Global cloud fraction ------------------------------------------------------
# Load cloud trends base layer
cloud <- raster("data/cloud_trends.tiff")

# Load world layers
world <- ne_countries(scale='large',returnclass = 'sf')

#Mask landmasses
cloud_mask <- mask(cloud, world)

#Create frame
cloud_frame <- as.data.frame(cloud_mask, xy = TRUE)
cloud_frame <- as.data.table(na.exclude(cloud_frame))

#Global metrics ---------------------------------------------
#Range 
round(range(cloud_frame$cloud_trends*10000), 1)

#Mean
w <- area_weigth(cloud_frame$y, cloud_frame$x, res = c(0.25, 0.25))
mean_value <- weighted.mean(x = cloud_frame$cloud_trends*10000, w = w)
round(mean_value, 1)

#Tropical landmasses ----------------------------------------
#Subset by Tropics of Cancer and Capriciousness
cloud_frame <- cloud_frame[y >= -23.43629]
cloud_frame <- cloud_frame[y <= 23.43629]

#Range 
round(range(cloud_frame$cloud_trends*10000), 1)

#Skewness
skewness(cloud_frame$cloud_trends, type = 3)

# Histogram range scale
cloud_frame$cloud_trends <- cloud_frame$cloud_trends*10000

### Load statistics ------------------------------------------------------------

#Statistics
cloud_stats <- fread("data/bayes_all_results.csv")
cloud_stats <- cloud_stats[variable == "cloud"]
cloud_stats$realm <- "TMCF"
realm_stats <- fread("data/bayes_realm_results.csv")
realm_stats <- realm_stats[variable == "cloud"]

cloud_stats <- cloud_stats[, c("realm", "parameter", "mean", "sd")]
cloud_stats <- cloud_stats[parameter == "mu"]
realm_stats <- realm_stats[, c("realm", "parameter", "mean", "sd")]
realm_stats <- realm_stats[parameter == "mu"]

stats <- rbind(cloud_stats, realm_stats)

#Add
stats$realm <- as.factor(stats$realm)
stats$realm <- factor(stats$realm, levels = c("Australasia",
                                            "Indomalayan",
                                            "Paleartic", 
                                            "Neotropic",
                                            "TMCF",
                                            "Tropical landmasses"))

#Merge TMCF and global
clouds <- frame[, c("realm", "cloud")]
clouds <- subset(clouds, realm != "Oceania")
tmcf <- clouds
tmcf$realm <- "TMCF"
global <- data.table(realm = "Tropical landmasses", cloud = cloud_frame$cloud_trends)

data <- rbind(clouds, global, tmcf)
data$realm <- as.factor(data$realm)
data$realm <- factor(data$realm, levels = c("Australasia",
                                            "Indomalayan",
                                            "Paleartic", 
                                            "Neotropic",
                                            "TMCF",
                                            "Tropical landmasses"))

### Plot basic features---------------------------------------------------------

# Theme
th <- theme(
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "transparent", color = NA),
  legend.background = element_rect(fill = "transparent"),
  legend.box.background = element_rect(fill = "transparent"),
  panel.spacing = unit(0,"null"))

# Color selection
colores <- rev(c("#67001f", "#b2182b", "#d6604d", "#f4a582", 
                          "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", 
                          "#4393c3", "#2166ac", "#053061"))
                          
gradient_range <- c(-70.0, 70.0)

### Plot -----------------------------------------------------------------------

#Recycle plot for top density lines (remove geom_segment)
ingredients <- ggplot_build(plot) %>% purrr::pluck("data", 1)
density_lines <- ingredients %>%
  group_by(group) %>% filter(density == max(density)) %>% ungroup()

plot <- ggplot() + 
  geom_density_ridges_gradient(
    data = data, 
    aes(x = cloud, y = realm, fill = stat(x)),
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|',
    point_size = 2,
    point_alpha = 1,
    scale = 0.9) +
  geom_segment(data = density_lines, 
               aes(x = x, y = ymin, xend = x, 
                   yend = ymin+density*scale*iscale),
               color = "black", inherit.aes = F, linetype= "solid") +
  geom_segment(data = stats, 
               aes(x = mean, 
                   xend = mean, 
                   y = as.numeric(realm), 
                   yend = as.numeric(realm) + c(0.4)),
               color = "grey40", inherit.aes = F, linetype= "solid", linewidth = 1) +
 geom_segment(data = stats, 
               aes(x = (mean - sd), xend = (mean - sd), 
                   y = as.numeric(realm), yend = as.numeric(realm) + c(0.4)),
               color = "grey40", linetype = "dotted", inherit.aes = F) +
  geom_segment(data = stats, 
               aes(x = (mean + sd), xend = (mean + sd), 
                   y = as.numeric(realm), yend = as.numeric(realm) + c(0.4)),
               color = "grey40", linetype = "dotted", inherit.aes = F) +
  geom_vline(xintercept = 0, 
             linetype= "solid", 
             colour = "grey75", alpha = 0.5) +
  scale_point_size_continuous(breaks = c(0, 1, 1, 1, 1)) +
  scale_fill_gradientn(colours = colores, 
                       limits = gradient_range) +
  coord_cartesian(xlim=c(-60, 60)) +
  scale_y_discrete(expand = c(0.02, 0)) +
  theme_ridges(grid = FALSE, 
               center_axis_labels = TRUE) +
  xlab(expression(paste(Delta, "CF (x10"^-4, " CF year"^-1, ")", sep = ""))) + 
  ylab("Realms") +
  theme(legend.position = "none") + th +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA))

###Export-----------------------------------------------------------------------
tiff("Figure_2.tiff", width = 140, height = 150, units = "mm", pointsize = 12, res = 600)

plot

dev.off()
  
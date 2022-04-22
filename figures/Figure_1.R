################################################################################
### Function for plotting trends in low clouds and distribution of TMCF
################################################################################

# Figure 1 in the manuscript.

###Select libraries-------------------------------------------------------------
library(data.table)
library(ggplot2)
library(raster)
library(sf)
library(rnaturalearth)
library(ggpubr)
library(grid)
library(gridExtra)
library(scales)

###Load data and layers---------------------------------------------------------
# Load ASCI of trends
frame <- fread("data/TMCF_slope.csv")
frame <- subset(frame, remove != "yes") #Remove duplicates

# Load world layers
world <- ne_countries(scale='large',returnclass = 'sf')

# Load cloud trends base layer
cloud <- raster("data/cloud_trends.tiff")

### Areas of interest and reproject for visualization---------------------------
PROJ <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84"

# Project TMCF
frame <- frame[is.na(frame$latitude) == FALSE] #TMCF
frame_WGS84 <- st_as_sf(x = frame,                         
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
frame_proj <- st_transform(frame_WGS84, crs = PROJ)

# Mask cloud trends tif
cloud_mask <- mask(cloud, world)

# Reproject
world_proj <- st_transform(world, crs = PROJ) #Countries
cloud_proj <- projectRaster(cloud_mask, crs = PROJ) #Cloud raster

# Raster to data.frame
cloud_frame <- as.data.frame(cloud_proj, xy = TRUE)
cloud_frame <- na.exclude(cloud_frame)

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

# Histogram range scale
cloud_frame$cloud_trends <- cloud_frame$cloud_trends*10000
frame$cloud <- frame$cloud*10000
range(cloud_frame$cloud_trends)
gradient_range <- c(-70, 70)
gradient_hist <- c(-70, 70)

# Limits
limits_breaks = c(-60, 0, 60)
limits_labels = c(-60, 0, 60)

### Create Plot-----------------------------------------------------------------
## Histograms --------------------------------
# Histograms - all landmasses
hist_all <- ggplot(cloud_frame, aes(x = cloud_trends)) +
  geom_histogram(aes(fill = ..x.., color = ..x..), bins = 30, colour = "grey75") +
  scale_y_continuous(limits = c(0, 200000), breaks = c(0, 50000, 100000, 150000, 200000), 
                     label= c("0.0", "0.5", "1.0", "1.5", "2.0"), expand = c(0, 0)) +
  scale_x_continuous(limits = gradient_hist, expand = c(0, 0), breaks = limits_breaks) +
  scale_fill_gradientn(colours = colores, limits = gradient_range, oob = scales::squish) +
  geom_vline(xintercept = -4.357497, linetype= "longdash", colour = "grey20") +
  xlab(expression(paste(Delta, "CF (x10"^-4, " CF year"^-1, ")", sep = ""))) + 
  ylab(expression(paste("Frequency (x10"^5, ")", sep = ""))) +
  theme_classic() + theme(legend.position = "none") + th +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA))

# Histograms - all TMCFs
hist_TMCF <- ggplot(frame, aes(x = cloud)) +
  geom_histogram(aes(fill = ..x.., color = ..x..), bins = 30, colour = "grey75") +
  scale_y_continuous(limits = c(0, 110), expand = c(0, 0), n.breaks = 4) +   
  scale_x_continuous(limits = gradient_range, expand = c(0, 0), breaks = limits_breaks) +
  scale_fill_gradientn(colours = colores, limits = gradient_range) +
  geom_vline(xintercept = -6.50059137594565, linetype= "longdash", colour = "grey20") +
  geom_vline(xintercept = -7.6809082150379, linetype= "dotted", colour = "grey20") +
  geom_vline(xintercept = -5.30631419137255, linetype= "dotted", colour = "grey20") +
  xlab(expression(paste(Delta, "CF (x10"^-4, " CF year"^-1, ")", sep = ""))) + 
  ylab("Frequency") +
  theme_classic() + theme(legend.position = "none") + th +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA))

# Histograms - Neotropic
neotropic <- subset(frame, realm == "Neotropic")

hist_neotropic <- ggplot(neotropic, aes(x = cloud)) +
  geom_histogram(aes(fill = ..x.., color = ..x..), bins = 30, colour = "grey75") +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0), breaks = c(0, 25, 50), 
                     labels = c(0, 25, 50)) +   
  scale_x_continuous(limits = gradient_range, expand = c(0, 0), breaks = limits_breaks) +
  scale_fill_gradientn(colours = colores, limits = gradient_range) +
  geom_vline(xintercept = -11.3212025889328, linetype= "longdash", colour = "#228b22") +
  geom_vline(xintercept = -13.2408840348968, linetype= "dotted", colour = "#228b22") +
  geom_vline(xintercept = -9.47735480808135, linetype= "dotted", colour = "#228b22") +
  xlab(expression(paste(Delta, "CF (x10"^-4, " CF year"^-1, ")", sep = ""))) + 
  ylab("Frequency") +
  theme_classic() + theme(legend.position = "none") + th +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA))

# Histograms - Indomalayan
indomalayan <- subset(frame, realm == "Indomalayan")

hist_indomalayan <- ggplot(indomalayan, aes(x = cloud)) +
  geom_histogram(aes(fill = ..x.., color = ..x..), bins = 30, colour = "grey75") +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0), breaks = c(0, 25, 50), 
                     labels = c(0, 25, 50)) +   
  scale_x_continuous(limits = gradient_range, expand = c(0, 0), breaks = limits_breaks) +
  scale_fill_gradientn(colours = colores, limits = gradient_range) +
  geom_vline(xintercept = -4.92055510014286, linetype= "longdash", colour = "#e6ab02") +
  geom_vline(xintercept = -7.06878233798084, linetype= "dotted", colour = "#e6ab02") +
  geom_vline(xintercept = -2.7072538761192, linetype= "dotted", colour = "#e6ab02") +
  xlab(expression(paste(Delta, "CF (x10"^-4, " CF year"^-1, ")", sep = ""))) + 
  ylab("Frequency") +
  theme_classic() + theme(legend.position = "none") + th +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA))

# Histograms - Paleartic
paleartic <- subset(frame, realm == "Paleartic")

hist_paleartic <- ggplot(paleartic, aes(x = cloud)) +
  geom_histogram(aes(fill = ..x.., color = ..x..), bins = 30, colour = "grey75") +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0), breaks = c(0, 25, 50), labels = c(0, 25, 50)) +   
  scale_x_continuous(limits = gradient_range, expand = c(0, 0), breaks = limits_breaks) +
  scale_fill_gradientn(colours = colores, limits = gradient_range) +
  geom_vline(xintercept = -1.49336002592469, linetype= "longdash", colour = "#a65628") +
  geom_vline(xintercept = -3.31157748660705, linetype= "dotted", colour = "#a65628") +
  geom_vline(xintercept = 0.349691142071848, linetype= "dotted", colour = "#a65628") +
  xlab(expression(paste(Delta, "CF (x10"^-4, " CF year"^-1, ")", sep = ""))) + 
  ylab("Frequency") +
  theme_classic() + theme(legend.position = "none") + th +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA))

# Histograms - Australasia
australasia <- subset(frame, realm == "Australasia")

hist_australasia <- ggplot(australasia, aes(x = cloud)) +
  geom_histogram(aes(fill = ..x.., color = ..x..), bins = 30, colour = "grey75") +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0), breaks = c(0, 25, 50), 
                     labels = c(0, 25, 50)) +   
  scale_x_continuous(limits = gradient_range, expand = c(0, 0), breaks = limits_breaks) +
  scale_fill_gradientn(colours = colores, limits = gradient_range) +
  geom_vline(xintercept = 2.56194530731001, linetype= "longdash", colour = "#984ea3") +
  geom_vline(xintercept = -1.91805628164917, linetype= "dotted", colour = "#984ea3") +
  geom_vline(xintercept = 7.30997760358128, linetype= "dotted", colour = "#984ea3") +
  xlab(expression(paste(Delta, "CF (x10"^-4, " CF year"^-1, ")", sep = ""))) + 
  ylab("Frequency") +
  theme_classic() + theme(legend.position = "none") + th +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA))

## Map of low cloud fraction trends --------------------------
panel_main <- ggplot() +
  geom_tile(data = cloud_frame, aes(x, y, fill= cloud_trends)) + 
  scale_fill_gradientn(name = expression(paste(Delta, "CF (x10"^-4, " CF year"^-1, ")", sep = "")), 
                       colours = colores, limits = gradient_range, 
                       breaks = c(-60, 0, 60), oob = scales::squish) +
  geom_sf(data = world_proj, color = "grey60", fill = NA, size = 0.2) +
  geom_sf(data= frame_proj, aes(shape = realm, colour = realm), 
          size = 1.2, alpha = 0.7) +
  scale_shape_manual("Realms", values=c(21, 24, 25, 22, 23), 
                     breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+ #21, 22, 23, 24, 25
  scale_colour_manual("Realms", 
                      values=c('#228b22', "#a65628", '#e6ab02', '#984ea3', "black"), 
                      breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+
  th + 
  xlab("") + 
  ylab("") +
  theme(legend.position= c("top"), legend.direction = "horizontal", 
        legend.background = element_rect(fill = "transparent"), 
        legend.box.background = element_blank()) +
  guides(colour = guide_legend(title.position = "bottom", ncol = 4, nrow = 4),
         fill = guide_colourbar(barwidth = 15, barheight = 0.7, title.position = "bottom"))

# Auxiliary map to manually improve the visualization
panel_a <- ggplot() +
  geom_tile(data = cloud_frame, aes(x, y, fill= cloud_trends)) + 
  scale_fill_gradientn(name = expression(paste(Delta, "CF (x10"^-4, " CF year"^-1, ")", sep = "")), 
                       colours = colores, limits = gradient_range, breaks = c(-60, 0, 60), oob = scales::squish) +
  geom_sf(data = world_proj, color = "grey60", fill = NA, size = 0.2) +
  th + xlab("") + ylab("") +
  theme(legend.position= c("top"), legend.direction = "horizontal", 
        legend.background = element_rect(fill = "transparent"), legend.box.background = element_blank()) +
  guides(colour = guide_legend(title.position = "bottom", ncol = 4, nrow = 4),
         fill = guide_colourbar(barwidth = 15, barheight = 0.7, title.position = "bottom"))

panel_b <- ggplot() +
  geom_tile(data = cloud_frame, aes(x, y)) + 
  scale_fill_gradientn(name = expression(paste(Delta, "CF (x10"^-4, " CF year"^-1, ")", sep = "")), 
                       colours = colores, limits = gradient_range, breaks = c(-60, 0, 60), oob = scales::squish) +
  geom_sf(data = world_proj, color = "grey60", fill = NA, size = 0.2) +
  geom_sf(data= frame_proj, aes(shape = realm, colour = realm), size = 1.2, alpha = 0.7) +
  scale_shape_manual("Realms", values=c(21, 24, 25, 22, 23), 
                     breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+ #21, 22, 23, 24, 25
  scale_colour_manual("Realms", values=c('#228b22', "#a65628", '#e6ab02', '#984ea3', "black"), 
                      breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+
  th + xlab("") + ylab("") +
  theme(legend.position= c("top"), legend.direction = "horizontal", 
        legend.background = element_rect(fill = "transparent"), legend.box.background = element_blank()) +
  guides(colour = guide_legend(title.position = "bottom", ncol = 4, nrow = 4),
         fill = guide_colourbar(barwidth = 15, barheight = 0.7, title.position = "bottom"))

###Arrange plot-----------------------------------------------------------------
main <- ggarrange(panel_main,
                  ggarrange(hist_all, hist_TMCF, ncol = 2, labels = c("", "c"), 
                          common.legend = FALSE, widths = c(4, 4), heights = 1),
                  ggarrange(hist_neotropic, hist_paleartic, hist_indomalayan, hist_australasia, 
                          ncol = 4, labels = c("d", "f", "g", "h"), common.legend = FALSE, 
                          widths = c(2, 2, 2, 2), heights = 1), # Second row with box and dot plots
                  nrow = 3,
                  ncol = 1,
                  labels = "a", 
                  common.legend = TRUE,
                  widths = 8, heights = c(3, 1, 1))

###Export-----------------------------------------------------------------------
tiff("Figure_1.tiff", width = 183 , height = 247, units = "mm", pointsize = 12, res = 600)

main

dev.off()





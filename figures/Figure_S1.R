################################################################################
### Function for plotting trends in low clouds and distribution of TMCF in detail
################################################################################


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
#Load ASCI of trends
frame <- fread("data/TMCF_slope.csv")
frame <- subset(frame, remove != "yes") #Remove duplicates
frame[, list(min(longitude), max(longitude), min(latitude), max(latitude)), by = realm]

#Load world layers
world <- ne_countries(scale='large',returnclass = 'sf')

#Load cloud trends base layer
cloud <- raster("data/cloud_trends.tiff")


###Areas of interest and reproject for visualization----------------------------
PROJ <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#Project TMCF
frame <- frame[is.na(frame$latitude) == FALSE] #TMCF
frame_WGS84 <- st_as_sf(x = frame,                         
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
frame_proj <- st_transform(frame_WGS84, crs = PROJ)

#Mask cloud trends tif
cloud_mask <- mask(cloud, world)

#Reproject
world_proj <- st_transform(world, crs = PROJ) #Countries
cloud_proj <- projectRaster(cloud_mask, crs = PROJ) #Cloud raster

#Raster to data.frame
cloud_frame <- as.data.frame(cloud_proj, xy = TRUE)
cloud_frame <- na.exclude(cloud_frame)

###Plot features----------------------------------------------------------------
#Theme
th <- theme(
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "transparent", color = NA),
  legend.background = element_rect(fill = "transparent"),
  legend.box.background = element_rect(fill = "transparent"),
  panel.spacing = unit(0,"null"))

#Color selection
colores <- rev(c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", 
                 "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"))

#Histogram range scale
cloud_frame$cloud_trends <- cloud_frame$cloud_trends*100
frame$cloud <- frame$cloud*100
range(cloud_frame$cloud_trends)
gradient_range <- c(-0.7, 0.7)
gradient_hist <- c(-1.0402, 1.0402)

#Limits
limits_breaks = c(-0.6, 0, 0.6)
limits_labels = c(-0.6, 0, 0.6)

#Main plot
all <- ggplot() +
  geom_tile(data = cloud_frame, aes(x, y, fill= cloud_trends)) +
  scale_fill_gradientn(name = expression(paste(Delta, "CF (x10"^-2, " CF year"^-1, ")", sep = "")), colours = colores, limits = gradient_range, breaks = c(-0.6, 0, 0.6), oob = scales::squish) +
  geom_sf(data = world_proj, color = "grey60", fill = NA, size = 0.2) +
  geom_sf(data= frame_proj, aes(shape = realm, colour = realm), size = 1.5) +
  scale_shape_manual("Realms", values=c(21, 24, 25, 22, 23), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+ #21, 22, 23, 24, 25
  scale_colour_manual("Realms", values=c('#228b22', "#a65628", '#e6ab02', '#984ea3', "black"), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+
  xlab("") + ylab("") +
  theme_bw() + theme(legend.position= c("top"), legend.direction = "horizontal", legend.background = element_rect(fill = "transparent"), legend.box.background = element_blank()) +
  guides(colour = guide_legend(title.position = "bottom", ncol = 4, nrow = 4),
         fill = guide_colourbar(barwidth = 15, barheight = 0.7, title.position = "bottom"))

#Neotropics Central America
neotropics_rangea <- c(-106, -80, 5, 23.6)
neotropics_a <- ggplot() +
  geom_tile(data = cloud_frame, aes(x, y, fill= cloud_trends)) +
  scale_fill_gradientn(name = expression(paste(Delta, "CF (x10"^-2, " CF year"^-1, ")", sep = "")), colours = colores, limits = gradient_range, breaks = c(-0.6, 0, 0.6), oob = scales::squish) +
  geom_sf(data = world_proj, color = "grey60", fill = NA, size = 0.2) +
  geom_sf(data= frame_proj, aes(shape = realm, colour = realm), size = 1.5) +
  scale_shape_manual("Realms", values=c(21, 24, 25, 22, 23), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+ #21, 22, 23, 24, 25
  scale_colour_manual("Realms", values=c('#228b22', "#a65628", '#e6ab02', '#984ea3', "black"), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+
  xlab("") + ylab("") +
  coord_sf(xlim = c(neotropics_rangea[1], neotropics_rangea[2]), ylim = c(neotropics_rangea[3], neotropics_rangea[4]), expand = FALSE) +
  theme_bw() + theme(legend.position= c("top"), legend.direction = "horizontal", legend.background = element_rect(fill = "transparent"), legend.box.background = element_blank()) +
  guides(colour = guide_legend(title.position = "bottom", ncol = 4, nrow = 4),
         fill = guide_colourbar(barwidth = 15, barheight = 0.7, title.position = "bottom"))

#Neotropics South America
neotropics_rangeb <- c(-82,  -42, -29, 24)
neotropics_b <- ggplot() +
  geom_tile(data = cloud_frame, aes(x, y, fill= cloud_trends)) +
  scale_fill_gradientn(name = expression(paste(Delta, "CF (x10"^-2, " CF year"^-1, ")", sep = "")), colours = colores, limits = gradient_range, breaks = c(-0.6, 0, 0.6), oob = scales::squish) +
  geom_sf(data = world_proj, color = "grey60", fill = NA, size = 0.2) +
  geom_sf(data= frame_proj, aes(shape = realm, colour = realm), size = 1.5) +
  scale_shape_manual("Realms", values=c(21, 24, 25, 22, 23), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+ #21, 22, 23, 24, 25
  scale_colour_manual("Realms", values=c('#228b22', "#a65628", '#e6ab02', '#984ea3', "black"), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+
  xlab("") + ylab("") +
  coord_sf(xlim = c(neotropics_rangeb[1], neotropics_rangeb[2]), ylim = c(neotropics_rangeb[3], neotropics_rangeb[4]), expand = FALSE) +
  theme_bw() + theme(legend.position= c("top"), legend.direction = "horizontal", legend.background = element_rect(fill = "transparent"), legend.box.background = element_blank()) +
  guides(colour = guide_legend(title.position = "bottom", ncol = 4, nrow = 4),
         fill = guide_colourbar(barwidth = 15, barheight = 0.7, title.position = "bottom"))

#Paleartic
paleartic_range <- c(-12, 56, -23, 14)
paleartic <- ggplot() +
  geom_tile(data = cloud_frame, aes(x, y, fill= cloud_trends)) +
  scale_fill_gradientn(name = expression(paste(Delta, "CF (x10"^-2, " CF year"^-1, ")", sep = "")), colours = colores, limits = gradient_range, breaks = c(-0.6, 0, 0.6), oob = scales::squish) +
  geom_sf(data = world_proj, color = "grey60", fill = NA, size = 0.2) +
  geom_sf(data= frame_proj, aes(shape = realm, colour = realm), size = 1.5) +
  scale_shape_manual("Realms", values=c(21, 24, 25, 22, 23), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+ #21, 22, 23, 24, 25
  scale_colour_manual("Realms", values=c('#228b22', "#a65628", '#e6ab02', '#984ea3', "black"), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+
  xlab("") + ylab("") +
  coord_sf(xlim = c(paleartic_range[1], paleartic_range[2]), ylim = c(paleartic_range[3], paleartic_range[4]), expand = FALSE) +
  theme_bw() + theme(legend.position= c("top"), legend.direction = "horizontal", legend.background = element_rect(fill = "transparent"), legend.box.background = element_blank()) +
  guides(colour = guide_legend(title.position = "bottom", ncol = 4, nrow = 4),
         fill = guide_colourbar(barwidth = 15, barheight = 0.7, title.position = "bottom"))

#Indomalayan
indomalayan_range <- c(76, 127, -9, 22)
indomalayan <- ggplot() +
  geom_tile(data = cloud_frame, aes(x, y, fill= cloud_trends)) +
  scale_fill_gradientn(name = expression(paste(Delta, "CF (x10"^-2, " CF year"^-1, ")", sep = "")), colours = colores, limits = gradient_range, breaks = c(-0.6, 0, 0.6), oob = scales::squish) +
  geom_sf(data = world_proj, color = "grey60", fill = NA, size = 0.2) +
  geom_sf(data= frame_proj, aes(shape = realm, colour = realm), size = 1.5) +
  scale_shape_manual("Realms", values=c(21, 24, 25, 22, 23), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+ #21, 22, 23, 24, 25
  scale_colour_manual("Realms", values=c('#228b22', "#a65628", '#e6ab02', '#984ea3', "black"), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+
  xlab("") + ylab("") +
  coord_sf(xlim = c(indomalayan_range[1], indomalayan_range[2]), ylim = c(indomalayan_range[3], indomalayan_range[4]), expand = FALSE) +
  theme_bw() + theme(legend.position= c("top"), legend.direction = "horizontal", legend.background = element_rect(fill = "transparent"), legend.box.background = element_blank()) +
  guides(colour = guide_legend(title.position = "bottom", ncol = 4, nrow = 4),
         fill = guide_colourbar(barwidth = 15, barheight = 0.7, title.position = "bottom"))

#Australasia
australasia_range <- c(115, 154, -18, 2.1)
australasia <- ggplot() +
  geom_tile(data = cloud_frame, aes(x, y, fill= cloud_trends)) +
  scale_fill_gradientn(name = expression(paste(Delta, "CF (x10"^-2, " CF year"^-1, ")", sep = "")), colours = colores, limits = gradient_range, breaks = c(-0.6, 0, 0.6), oob = scales::squish) +
  geom_sf(data = world_proj, color = "grey60", fill = NA, size = 0.2) +
  geom_sf(data= frame_proj, aes(shape = realm, colour = realm), size = 1.5) +
  scale_shape_manual("Realms", values=c(21, 24, 25, 22, 23), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+ #21, 22, 23, 24, 25
  scale_colour_manual("Realms", values=c('#228b22', "#a65628", '#e6ab02', '#984ea3', "black"), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+
  xlab("") + ylab("") +
  coord_sf(xlim = c(australasia_range[1], australasia_range[2]), ylim = c(australasia_range[3], australasia_range[4]), expand = FALSE) +
  theme_bw() + theme(legend.position= c("top"), legend.direction = "horizontal", legend.background = element_rect(fill = "transparent"), legend.box.background = element_blank()) +
  guides(colour = guide_legend(title.position = "bottom", ncol = 4, nrow = 4),
         fill = guide_colourbar(barwidth = 15, barheight = 0.7, title.position = "bottom"))

###Arrange plot-----------------------------------------------------------------
yo <- ggarrange(neotropics_a,
                neotropics_b,
                paleartic,
                indomalayan,
                australasia,
                nrow = 3,
                ncol = 2,
                common.legend = TRUE,
                widths = c(2, 2), heights = c(2, 2, 2))

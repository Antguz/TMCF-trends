################################################################################
###### Figure of cloud patterns and PARRIS information
################################################################################

#Library
library(ggplot2)
library(data.table)
library(viridis)
library(ggExtra)
library(ggpubr)
library(ggpointdensity)
library(ggpmisc)

###Load data--------------------------------------------------------------------
data_slope <- as.data.frame(fread("data/TMCF_slope.csv")) #Trends of change
data_error <- as.data.frame(fread("data/TMCF_error.csv")) #Trends of change

#Function to get % among panels-------------------------------------------------
panels <- function(X, Y) {
  frame <- data.table(X= X, Y= Y)
  lines <- nrow(frame)
  
  p1 <- nrow(frame[X <= 0 & Y > 0])
  p2 <- nrow(frame[X > 0 & Y > 0])
  p3 <- nrow(frame[X > 0 & Y <= 0])
  p4 <- nrow(frame[X <= 0 & Y <= 0])
  
  p1 <- round(p1/lines, 4) * 100
  p2 <- round(p2/lines, 4) * 100
  p3 <- round(p3/lines, 4) * 100
  p4 <- round(p4/lines, 4) * 100
  
  vec <- paste0(round(c(p1, p2, p3, p4), 1), "%")
  
  return(vec)
  
}

#Functions to get range of points to put the %----------------------------------
range_points_x <- function(limits = c(-0.33, 0.33), per =  0.09) {
  
  ran <- diff(range(limits, na.rm = TRUE))
  p1 <- limits[1] + ran*per
  p2 <- limits[2] - ran*per
  p3 <- limits[2] - ran*per
  p4 <- limits[1] + ran*per
  
  return(c(p1, p2, p3, p4))
}

range_points_y <- function(limits = c(-0.33, 0.33), per =  0.09) {
  
  ran <- diff(range(limits, na.rm = TRUE))
  p1 <- limits[2] - ran*per
  p2 <- limits[2] - ran*per
  p3 <- limits[1] + ran*per
  p4 <- limits[1] + ran*per
  
  return(c(p1, p2, p3, p4))
}

###Plot features----------------------------------------------------------------
th <- theme_bw() + theme(plot.background = element_rect(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           plot.margin = margin(4, 6, 0, 0, "pt"),
                                           axis.text.x = element_text(color = "black"),
                                           axis.text.y = element_text(color = "black"))

size_point <- 1
size_text <- 2

formula <- y ~ x

###Ranges for axis--------------------------------------------------------------
#Trends of low cloud 
data_slope$cloud <- data_slope$cloud*100 #Scale
data_error$cloud <- data_error$cloud*100 #Scale
range(data_slope$cloud, na.rm = TRUE)
range_clouds <- c(-0.648, 0.648)

###Plot function----------------------------------------------------------------
plot_function <- function(x_ECV, y_cloud, name_x, name_y) {
  
  panels_x <- panels(data_slope[, x_ECV], data_slope[, y_cloud])
  x_extreme <- max(abs(range(data_slope[, x_ECV], na.rm = TRUE)))
  x_annotate <- range_points_x(c(-x_extreme, x_extreme), 0.09)
  y_annotate <- range_points_y(range_clouds, 0.05)
  
  plot_export <- ggplot(data_slope, aes(x = data_slope[, x_ECV], y = data_slope[, y_cloud])) +
    geom_vline(xintercept = 0, col = "grey80") + geom_hline(yintercept= 0, col = "grey80") +
    scale_x_continuous(limits = c(-x_extreme, x_extreme), expand = c(0,0)) +
    scale_y_continuous(limits = range_clouds, expand = c(0,0)) +
    xlab(name_x) +
    ylab(expression(paste(Delta, "CF (x10"^-2, " CF year"^-1, ")", sep = ""))) +
    stat_poly_eq(formula = formula) +
    geom_linerange(aes(xmin = data_error[, x_ECV], xmax = data_error[, x_ECV], ymin = data_error[, y_cloud], ymax = data_error[, y_cloud]), colour = "black") +
    geom_point(aes(shape = realm, fill = realm), size = size_point, alpha = 0.7, colour = "grey") +
    geom_smooth(method='lm', formula= formula, se=T, colour = "black", size = 0.4) +
    scale_shape_manual("Realms", values=c(21, 24, 25, 22, 23), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania')) +
    scale_fill_manual("Realms", values=c('#228b22', "#a65628", '#e6ab02', '#984ea3', "black"), breaks = c('Neotropic','Paleartic','Indomalayan', 'Australasia', 'Oceania'))+
    annotate("text", x= x_annotate[1], y= y_annotate[1], label= panels_x[1], col = "grey25") + 
    annotate("text", x= x_annotate[2], y= y_annotate[2], label= panels_x[2], col = "grey25") + 
    annotate("text", x= x_annotate[3], y= y_annotate[3], label= panels_x[3], col = "grey25") + 
    annotate("text", x= x_annotate[4], y= y_annotate[4], label= panels_x[4], col = "grey25") + 
    th + theme(legend.position = "none")
  
}

#Trends of temperature
temperature <- plot_function(x_ECV = "temperature", 
                             y_cloud = "cloud", 
                             name_x = expression(paste(Delta, "Temperature (K year"^-1, ")", sep = "")), 
                             name_y = expression(paste(Delta, "CF (x10"^-2, " CF year"^-1, ")", sep = "")))

p1 <- ggMarginal(temperature, 
                 type= "histogram", 
                 groupColour = FALSE, 
                 xparams = list(fill = "grey80", colour = "grey40"),
                 yparams = list(fill = "grey80", colour = "grey40"))

#Precipitation
precipitation <- plot_function("precipitation", 
                             "cloud", 
                             name_x = expression(paste(Delta, "Precipitation (mm day"^-1, "year"^-1, ")", sep = "")), 
                             name_y = expression(paste(Delta, "CF (x10"^-2, " CF year"^-1, ")", sep = "")))

p2 <- ggMarginal(precipitation, 
                 type= "histogram", 
                 groupColour = FALSE, 
                 xparams = list(fill = "grey80", colour = "grey40"),
                 yparams = list(fill = "grey80", colour = "grey40"))

#dewpoint
dewpoint <- plot_function("dewpoint", 
                          "cloud", 
                          name_x = expression(paste(Delta, "Dewpoint (K year"^-1, ")", sep = "")), 
                          name_y = expression(paste(Delta, "CF (x10"^-2, " CF year"^-1, ")", sep = "")))

p3 <- ggMarginal(dewpoint, 
                 type= "histogram", 
                 groupColour = FALSE, 
                 xparams = list(fill = "grey80", colour = "grey40"),
                 yparams = list(fill = "grey80", colour = "grey40"))

#Volumetric soild water
vswc <- plot_function("vswc", 
                      "cloud", 
                      name_x = expression(paste(Delta, "VSWC (m"^3,"m"^-3, " year"^-1, ")", sep = "")), 
                      name_y = expression(paste(Delta, "CF (x10"^-2, " CF year"^-1, ")", sep = "")))

p4 <- ggMarginal(vswc, 
                 type= "histogram", 
                 groupColour = FALSE, 
                 xparams = list(fill = "grey80", colour = "grey40"),
                 yparams = list(fill = "grey80", colour = "grey40"))


#ET
et <- plot_function("ET", 
                    "cloud", 
                    name_x = expression(paste(Delta, "ET (mm month"^-1, " year"^-1, ")", sep = "")), 
                    name_y = expression(paste(Delta, "CF (x10"^-2, " CF year"^-1, ")", sep = "")))

p5 <- ggMarginal(et, 
                 type= "histogram", 
                 groupColour = FALSE, 
                 xparams = list(fill = "grey80", colour = "grey40"),
                 yparams = list(fill = "grey80", colour = "grey40"))

#PET
pet <- plot_function("PET", 
                      "cloud", 
                      name_x = expression(paste(Delta, "PET (mm month"^-1, " year"^-1, ")", sep = "")), 
                      name_y = expression(paste(Delta, "CF (x10"^-2, " CF year"^-1, ")", sep = "")))

p6 <- ggMarginal(pet, 
                 type= "histogram", 
                 groupColour = FALSE, 
                 xparams = list(fill = "grey80", colour = "grey40"),
                 yparams = list(fill = "grey80", colour = "grey40"))



###Arrange plot-----------------------------------------------------------------
prow <- cowplot::plot_grid(
  p1, 
  p2, 
  p3, 
  p4, 
  p5,
  p6,
  nrow = 3, ncol = 2, 
  align = 'hv',
  labels = c("a", "b", "c", "d", "e", "f"),
  hjust = -1
)

#Export

jpeg('Figure_2.jpg', width = 183 , height = 247, units = "mm", pointsize = 12, quality = 100, res = 600)

prow

dev.off()

#END


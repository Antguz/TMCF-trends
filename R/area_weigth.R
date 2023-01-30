earth_radius <- function(lat) {
  
  ##calculate radius of Earth assuming oblate spheroid defined by WGS84
  
  # define oblate spheroid from WGS84
  a <- 6378137
  b <- 6356752.3142
  e2 <- 1 - (b^2/a^2)
  
  # convert from geodecic to geocentric
  # see equation 3-110 in WGS84
  lat <-deg2rad(lat)
  lat_gc <- atan((1-e2)*tan(lat))
  
  # radius equation
  # see equation 3-107 in WGS84
  r <- ((a * (1 - e2)^0.5) / (1 - (e2 * cos(lat_gc)^2))^0.5)
  
  return(r)
    
}

deg2rad <- function(deg) {
  stopifnot(is.numeric(deg))
  ( rad <- (pi/180)*deg )
}

area_weigth <- function(lat, lon, res){

  R <- earth_radius(lat)/1000

  dlat <- deg2rad(res[1])
  dlon <- deg2rad(res[2])

  dy <- dlat * R
  dx <- dlon * R * cos(deg2rad(lat))

  area <- dy * dx
  
  return(area)

}

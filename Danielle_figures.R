#plots for danielle 5/20/24
#load packages
library(pacman)
p_load(tidyverse, sp, mapview, tmap, stars,
       lubridate, gganimate, animation, patchwork, ggrepel, whitebox,
       terra,  tidyterra, RColorBrewer)

#read in temperature analysis from Danielle
stic <- read_csv("TDAS_metrics_STIC.csv") 

#read in STIC locations
locations <- read_csv("~/Documents/VT Research/HBTopModel/DataForMary/HB_stic.csv")
locations$site_id <- paste0(locations$wshed, "_", locations$ID)
only_locs <- locations %>% 
  select(site_id, lat, long, wshed) %>% 
  unique()

#combine datasets containing the location data and the temperature analysis
comb <- left_join(stic, only_locs, by = "site_id") %>% 
  select(site_id, wshed, date, lat, long, AmpRatio)

#read in DEM to use as a template for crs
dem_location <- "./rasters/1m_hydroenforced.tif"
dem <- rast(dem_location)

#convert dataframe to spatial data for map making
comb_vect <- vect(comb, geom=c("long", "lat"), crs="epsg:4326", keepgeom=FALSE)
comb_vect_proj <- project(comb_vect, crs(dem))

#creating hillshade for background of plots
#writeRaster(mask_dem, "./rasters/1m_hydroenforced.tif")

hill_location <- "./rasters/hillshade.tif"
wbt_hillshade(
  dem_location,
  hill_location,
  azimuth = 315,
  altitude = 30
)
w3_stream <- vect("~/Documents/VT Research/HBTopModel/HB/hbstream/hb42_master_startend.shp")
w3_regime <- vect("~/Documents/VT Research/HB_data/headwater_streams_byflowregime/WS3_Streams_Types.shp")
plot(w3_regime)

hill <- rast(hill_location)
pal <- colorRampPalette(c("white","black"))

#plot formatted nicely
plot(hill, col = pal(7), legend = FALSE, axes=FALSE)
plot(dem, alpha = 0.6, add = TRUE, legend = FALSE)
lines(filter(w3_regime, StrType == "E"), lty = 3, col = "blue")
lines(filter(w3_regime, StrType == "I"), lty = 2, col = "blue")
lines(filter(w3_regime, StrType == "P"), lty = 1, col = "blue")
legend("topright", inset=c(-0.05,0), legend=c("Perennial", "Intermittent", "Ephemeral"),
       col="blue", lty = c(1,2,3), cex=0.6, xpd = TRUE, bty = 'n')
points(filter(comb_vect_proj, wshed == "W3",  date == "2023-08-05"), cex = 0.1+ comb_vect_proj$AmpRatio/2)
title(main = "2023-07-05", adj = 0, line =2.5)
legend("bottomright", 
       title = "AR", inset=c(-0.05,0), legend=c("0", "2", "4"),
       col="black", pt.cex = c(0.1,1,2), pch = 19, xpd = TRUE, bty = "n", cex = 0.6)

#turn plot into an animation!
#first make function to display map from specific day:
arPlot <- function(d){
  png(filename=paste0("./ar_images/",d,".png"))
  plot(hill, col = pal(7), legend = FALSE, axes=FALSE)
  plot(dem, alpha = 0.6, add = TRUE, legend = FALSE)
  lines(filter(w3_regime, StrType == "E"), lty = 3, col = "blue")
  lines(filter(w3_regime, StrType == "I"), lty = 2, col = "blue")
  lines(filter(w3_regime, StrType == "P"), lty = 1, col = "blue")
  legend("topright", inset=c(-0.05,0), legend=c("Perennial", "Intermittent", "Ephemeral"),
       col="blue", lty = c(1,2,3), cex=1, xpd = TRUE, bty = 'n')
  legend("bottomright", 
       title = "AR", inset=c(-0.05,0), legend=c("0", "1"),
       col="black", pt.cex = c(0.1,3), pch = 19, xpd = TRUE, bty = "n", cex = 1)
  points(filter(comb_vect_proj, wshed == "W3", date == d, AmpRatio < 1.25), cex = 0.1+ comb_vect_proj$AmpRatio*3)
  title(main = d, adj = 0, line =2.5)
  dev.off()
  
}

arPlot(d = "2023-09-05")
arPlot(d = "2023-10-06")

for(i in 1:length(unique(comb_vect_proj$date))){
  d <- unique(comb_vect_proj$date)[i]
  arPlot(d)
}
filter(comb_vect_proj, wshed == "W3",  date == "2023-07-05")


#display precip
https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-hbr.13.21&entityid=0387dde36ad18772f53a551fa3a2083d

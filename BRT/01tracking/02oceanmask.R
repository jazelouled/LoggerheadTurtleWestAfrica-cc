#------------------------------------------------------------------------------------
# oceanmask.R           Create ocean mask to create simulations
#------------------------------------------------------------------------------------
#
# Description:
# Simulations of animal tracks requires setting a map layer to constrain their movement.
# We consider three main constrains of the movement of marine animals:
# 1.- A land mask, to avoid moving into land.
# 2.- A study area, in this case defined by the minimum convex polygon of all tracks.
#
# Returns:
# 1) A minimum convex polygon (shapefile format)
# 2) A raster map (resistance layer) with the following characteristics:
# - CRS is epsg:3031 (simulations seem to perform better using a projected CRS)
# - Resolution is 5 km
# - values are: 0 = ocean, 1 = land
library(raster)

#--------------------------------
# Import data
#--------------------------------

# Import bathymetry
# bathy <- raster("data/gebco/derived_bathy.nc")

bathymetry <-"~/Dropbox/2022_SouthHemisphere_SDM/bathymetry/"
bathy <- raster(paste0(bathymetry, "/GEBCO_2014_2D.nc"))  # bathymetry: This we have to check how to put it online so it is downloaded with the package


bathy_nc <- paste0(output_data, "/terrain/derived_bathy.nc")
bathy <- raster(bathy_nc)

## Import L2 product of simulations (i.e. simulations with environmental data)
indir <- paste0(output_data, "tracking/", "L2_locations")
loc_files <- list.files(indir, full.names = TRUE, pattern = "L2_locations.csv")
ssm <- readTrack(loc_files)

# sexseason <- read.csv(paste0(input_data, "/tracking/SexSeasonMF.csv"))
# 
# ssm <- merge(ssm, sexseason, by=c("id"))


# 
# #-----------------------------------------------------------------
# # mapL1       Map locations using L1 product
# #-----------------------------------------------------------------
# mapL1 <- function (data){
#   # data: locations L1 data.frame
#   #
#   # Required columns for L1
#   # date: POSIXct
#   # argosfilter:
#   # onland:
#   # lon:
#   # lat:
#   #
#   # Function return a plot
#   
#   
#   # Load libraries and dependencies
#   library(ggplot2)
#   library(RColorBrewer)
#   #source("R/config.R")  # set your Google Drive data folder here
#   #source("R/database_tools.R")
#   
#   # Import world map
#   data(countriesHigh, package = "rworldxtra", envir = environment())
#   wm <- suppressMessages(fortify(countriesHigh))
#   
#   ### Parse date format
#   ssm$time <- as.integer(ssm$date)
#   
#   data <- ssm
#   ### Filter location data
#   #data <- filter(data, argosfilter == "not" & onland == "FALSE")
#   
#   ### Get metadata
#   sdate <- min(data$date)
#   edate <- max(data$date)
#   days <- round(as.numeric(difftime(edate, sdate, units="days")))
#   
#   ### Define extension for plot
#   xl <- extendrange(data$lon, f = 0.5)
#   yl <- extendrange(data$lat, f = 0.5)
#   b <- c(data$time[1], data$time[nrow(data)])
#   blab <- c(as.Date(data$date[1]), as.Date(data$date[nrow(data)]))
#   
#   polyFW <- st_as_sf(cp.buf_femalesW)
#   polyFS <- st_as_sf(cp.buf_femalesS)
#   polyMW <- st_as_sf(cp.buf_malesW)
#   cp.buf_malesW
#   
#   
#   
#   coordinates(polyMW)
#   ### Plot
#   p <- ggplot() +
#     geom_sf(data = polyFW, fill = "lightcyan", alpha = 0.6)+
#     geom_sf(data = polyMW, fill = "slategray2", alpha = 0.6)+
#     geom_sf(data = polyFS, fill = "slategrey", alpha = 0.6)+
#     
#     coord_sf(xlim = c(-180,16.884), ylim = c(-83.606,-29.243), expand = F)+
#     geom_polygon(data = wm, aes_string(x = "long", y = "lat", group = "group"),
#                  fill = grey(0.3)) +
#     xlab("Longitude") +
#     ylab("Latitude") +
#     geom_point(data = data,
#                aes_string(x = "lon", y = "lat",  colour ="sex", fill = "sex"),
#                size = 0.4) +
#     # coord_quickmap(xlim = xl, ylim = yl, expand = TRUE) +
#     
#     #scale_colour_gradientn(colours = terrain.colors(10)) + 
#     # scale_colour_gradientn(colours=rev(brewer.pal(10,"RdYlGn")),
#     #                        breaks=b,labels=format(blab)) +
#     scale_colour_fish_d("Acanthurus_olivaceus") +
#     geom_path(data = data,
#               aes_string(x = "lon", y = "lat", group = "id"), size  = 0.2, alpha = 0.4) +
#     labs(title = paste(data$sp_code[1],
#          subtitle = paste("Start:", sdate, "End:", edate, "(", days, "days)"))) + theme2Review
#   
#   return(p)
#   
# }
# 




library(rgeos)
library(sf)
library(adehabitatHR)


#--------------------------------
# Minimum convex polygon
#--------------------------------

# Create a minimum convex polygon using all tracks to delimit
ssm$sID <- 1  # create a single ID for all tracks
ssm <- dplyr::select(ssm, sID, lon, lat)


bbox <-ssm %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  st_bbox()

??st_bbox
# 
# # females winter
# ssm_femalesW <- subset(ssm, ssm$sex == "females" & ssm$season == "winter")
# coordinates(ssm_femalesW) <- ~ lon + lat
# proj4string(ssm_femalesW) <- "+proj=longlat +ellps=WGS84"
# cp_femalesW <- mcp(ssm_femalesW, percent = 100) # MCP use all the positions
# 
# # Extend the MCP with a buffer of 1 degree
# cp.buf_femalesW <- gBuffer(cp_femalesW, width = mcp_expand)
# 
# 
# 
# # females summer
# ssm_femalesS <- subset(ssm, ssm$sex == "females" & ssm$season == "summer")
# coordinates(ssm_femalesS) <- ~ lon + lat
# proj4string(ssm_femalesS) <- "+proj=longlat +ellps=WGS84"
# cp_femalesS <- mcp(ssm_femalesS, percent = 100) # MCP use all the positions
# 
# # Extend the MCP with a buffer of 1 degree
# cp.buf_femalesS <- gBuffer(cp_femalesS, width = mcp_expand)
# 
# plot(cp.buf_femalesS)
??mcp
# males winter
# ssm_malesW <- subset(ssm, ssm$sex == "males" & ssm$season == "winter")
coordinates(ssm) <- ~ lon + lat
proj4string(ssm) <- "+proj=longlat +ellps=WGS84"
cp_lhgt <- mcp(ssm, percent = 100) # MCP use all the positions

# Extend the MCP with a buffer of 1 degree
cp.buf_lhgt<- gBuffer(cp_lhgt, width = mcp_expand)

plot(cp_femalesS) 


cp.buf <- cp.buf_malesW

plot(cp.buf)
#--------------------------------
# Minimum convex polygon
#--------------------------------

# Create a minimum convex polygon using all tracks to delimit
ssm$sID <- 1  # create a single ID for all tracks
ssm <- dplyr::select(ssm, sID, lon, lat)
coordinates(ssm) <- ~ lon + lat
proj4string(ssm) <- "+proj=longlat +ellps=WGS84"
cp <- mcp(ssm, percent = 100) # MCP use all the positions

# Extend the MCP with a buffer of 1 degree
cp.buf <- gBuffer(cp, width = mcp_expand)

# Export MCP as shapefile
# cp.df <- data.frame(ID=1:length(cp.buf)) 
# row.names(cp.df) <- "buffer"
# p <- SpatialPolygonsDataFrame(cp.buf, cp.df) 
# writeOGR(p, paste(input_data, "mcp.gpkg", sep="/"), "mcp", driver="GPKG", overwrite_layer=TRUE)


#--------------------------------
# Combine MCP and landmask
#--------------------------------

# Reclassify bathymetry to create ocean mask
# Define resistance values (0 = ocean, 1 = land)
bathy[!is.na(bathy)] <- 0
bathy[is.na(bathy)] <- 1

# Mask with MCP extent
#oceanmask <- mask(bathy, cp.buf)
oceanmask <- bathy

# downsize
oceanmask <- aggregate(oceanmask, fact = 5, fun = median)

# Export resistance
writeRaster(oceanmask, paste0(output_data, "/terrain/oceanmask.nc"), format="CDF", overwrite=TRUE)

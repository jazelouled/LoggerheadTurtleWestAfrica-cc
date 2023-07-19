library(ncdf4)
library(data.table)
library(lubridate)
library(raster)
library(stringr)
nc_var <- "tos"
nc_var <- "so-surf"
nc_var <- "intpp"

# CMIP6 dataset errata:

# All IPSL Chlorophyll-a data are 3 orders of magnitude (1e3) too high because related variables are in g/m3 
# instead of kg/m3 as expected in CMIP6 data request. Users are invited to apply a factor of 10^-3 while a new 
# version of the affected dataset is published. Old dataset version will be unpublished from the ESGF. 
# https://errata.es-doc.org/static/view.html?uid=1496cb33-b5ba-2583-b9f6-961e6ad94347
ff


# Code to create monthly layers of tos, chl, o2, siconc, so-surf, thetao, uo & vo. Also added code in the 
# end of the loop to calculate tos and so-surf gradients and distance to ice (> 15% sic).


# argentina <- extent(-95.449219, -21.796875, -70.140364, -12.897489)
argentina <- extent(-125.156250, 3.339844, -70.554179, 20.632784) # new, not done yet

# southAfrica <- extent(-17.578125, 72.421875,-64.774125,-13.752725)
southAfrica <- extent(-21.796875,70.488281, -50.176898, 15.453680)# new, not done yet

# australiaNZ <- extent(84.199219, 180, -52.802761, 0.351560) # old

australiaNZ <- extent(105, 180, -65, -2) # new


SOextents <- c(argentina, southAfrica, australiaNZ)





names_SOextents <- c("southAmerica", "southAfrica", "australiaNZ")


ssp <- c("histor", "ssp126", "ssp370", "ssp585")

nc_var_list <- c("tos", "so-surf", "intpp")

ISIMIP_models <- dir("D:/ISIMIP_netCDF/models/", recursive = F, full.names = T )

rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")



for (h in 1:length(SOextents)) {
  
  extent <- SOextents[[h]]
  name_SOextent <- names_SOextents[h]

for (i in 1:length(nc_var_list)){
  
  
  nc_var <- nc_var_list[i]
  
  for (z in 1:length(ISIMIP_models)){
    print(z)
    model <- gsub('.*/ ?(\\w+)', '\\1', ISIMIP_models[z])
    if((nc_var == "siconc" | nc_var == "chl"|nc_var == "o2-surf"| nc_var == "so-surf" | nc_var == "uo" | nc_var == "vo" | nc_var == "phydiat-vint" | nc_var == "mlotstmax")  & model == "MRI-ESM2-0") next
    if((nc_var == "phydiat-vint")  & (model == "MPI-ESM1-2-HR"| model == "UKESM1-ESM2-0")) next
    if((nc_var == "mlotstmax")  & (model == "GFDL-ESM4")) next
    if(model == "MRI-ESM2-0") next
    
    
    ssp_folders<- dir(ISIMIP_models[z], recursive = F, full.names = T )
    ssp <- gsub('.*/ ?(\\w+)', '\\1', ssp_folders)
    
    for (q in 1:length(ssp)){
      print(q)
      ISIMIP_files <- dir(ssp_folders[q], recursive = T, full.names = T )
      ISIMIP_files_var <- ISIMIP_files[ISIMIP_files %like% nc_var]
      filename <- gsub('.*/ ?(\\w+)', '\\1', ISIMIP_files_var)
      nc <- nc_open(ISIMIP_files_var)
      
      if (nc_var == "chl" | nc_var == "thetao" | nc_var == "uo" | nc_var == "vo"){
        var <-ncvar_get(nc, varid=nc_var, start=c(1,1,1,1), count=c(-1,-1, 1, -1))
      } else (var <-ncdf4::ncvar_get(nc, varid=nc_var))
      
      lat<-ncvar_get(nc, varid="lat")
      lon<-ncvar_get(nc, varid="lon")
      time<-ncvar_get(nc, varid="time")
      time<-floor(time)
      
      if (model == "MRI-ESM2-0"){
        date <-as.POSIXct(days(time), origin = "2015-01-01", format ="%Y-%m-%d")
      } else {date <-as.POSIXct(months(time), origin = "1601-01-01", format ="%Y-%m-%d")
      }
      
      
      var_r<-brick(var)
      var_r<-(flip(var_r, direction="x"))
      var_r<-t(flip(var_r, direction="x"))
      bb<-extent(min(lon),max(lon),min(lat),max(lat))
      var_r<-setExtent(var_r,bb,keepres = F, snap = F)
      projection(var_r)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      southernHemisphere <- extent
      var_r_southernHemisphere <- crop(var_r, southernHemisphere)
      months<-seq(1: nlayers(var_r_southernHemisphere))
      
      for (k in 1:length(date)){
        print(k)
        YYYY <- year(date)[k]
        MM <- sprintf("%02d", month(date))[k]
        DD <- sprintf("%02d", day(date))[k]
        product_folder <- paste("D:/ISIMIP_netCDF/output/SH/SouthernHemisphereSDM/", name_SOextent, "monthly_rasters_ISIMIP_tif", model, ssp, YYYY,  sep="/")
        if (!dir.exists(product_folder[q])) dir.create(paste(product_folder[q], sep = ""), recursive = TRUE)  # create output directory if does not exist
        
        r <- var_r_southernHemisphere[[k]]
        if (model == "IPSL-CM6A-LR" & nc_var =="chl"){
          r <- r*0.001      }
        
        filename <- paste0(YYYY, MM, DD, "_", model,"_", ssp,"_", nc_var, "_", name_SOextent, ".tif")
        writeRaster(r, filename=paste0(product_folder[q], sep="","/", filename[q]), overwrite = T)     
        
        
        
        
                }
             }
          }
        }
     }












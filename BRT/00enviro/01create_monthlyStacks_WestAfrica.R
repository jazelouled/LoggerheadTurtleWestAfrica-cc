library(raster)
library(data.table)
library(lubridate)
library(ncdf4)


`%notlike%` <- Negate(`%like%`)


#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------

# set number of cores
cores <- 10
ff
# Set raster resolution and extent
res <- 1

res <- 0.008333333
e <- extent(bathy)

# e <- extent(-90, -20, -80, -40) # males winter
e <- extent(-180, 16.884, -83.606, -29.243) # males winter, females winter, females summer

# Set period
# date_start <- as.Date("2019-03-01")
# date_end <- as.Date("2019-09-30")

# dynamic variables to extract. same names as catalog
# env_dyn_vars <- c("SST", "SSTg", "SAL", "SALg", "SSH", "EKE", "CHL", "SIC", "SIT", "MLD", "EDGE") 
env_dyn_vars <- c("SST", "THETAO", "SAL", "CHL", "SIC", "UO", "VO", "SSTg", "SALg", "EDGE", "EKE", "MLD", "DIAT")
# path to environmental static data
bathymetry <-"C:/Users/Jazel Ouled/Dropbox/2022_SouthHemisphere_SDM/bathymetry/"

# path to output
outdir <- "D:/ISIMIP_netCDF/output/SH/SouthernHemisphereSDM"
# outdir <- paste0(static_data, "stack_monthly")#"data/out/environment/stack_daily"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)



#---------------------------------------------------------------
# 1. Create oceanmask
#---------------------------------------------------------------

# create base raster
m <- raster(e, res = res, crs = crs("+proj=longlat +datum=WGS84"))
m[] <- 1


# bathy[bathy >= 0] <- NA

# writeRaster(bathy, paste0(outdir, "derived_bathy.nc"), format="CDF", overwrite=TRUE)

#---------------------------------------------------------------
# 2. Import environmental data
#---------------------------------------------------------------
prepareGrid <- function(r, m, method, name){
  # rc <- raster::crop(r, m)  # crop by extent
  # rs <- raster::resample(r, m, method=method)  # resample
  rm <- raster::mask(bathy, m)  # mask
  names(rm) <- name
  return(rm)
}



# import static maps
bathy <- raster(paste0(bathymetry, "/GEBCO_2014_2D.nc"))  # bathymetry

bathy <- crop(bathy, m)
bathy <- bathy+0


# mm <- raster("D:/ISIMIP_netCDF/output/SouthernHemisphereSDMderived_bathy.nc")
# 
# plot(mm)

#-------------------------------------------------------
# 3. Prepare static variables
#-------------------------------------------------------

# create stack with static variables
bat <- prepareGrid(bathy, m, method="bilinear", name="BAT")

stack_static <- stack(bat)
plot(stack_static)



# import catalogue with oceanographic products
# catalog<-read.csv("data/output/environment/catalog_antarctica_cc.csv", sep=",") # males winter
catalog<-read.csv("C:/Users/Jazel Ouled/Dropbox/2022_SouthHemisphere_SDM/GitHub/catalog_southHemisphereSDM.csv")

# catalog <- catalog[catalog$product %like% "IPSL" & catalog$service == "histor",]

# Create dates
# dates <- seq.Date(date_start, date_end, by="month")

ISIMIP_montly_files_gr <- dir("D:/ISIMIP_netCDF/output/SH/SouthernHemisphereSDM/", recursive = T, full.names = T, pattern = ".tif" )

ISIMIP_montly_files_gr_histor <- dir("D:/ISIMIP_netCDF/output/SH/SouthernHemisphereSDM/southAmerica/monthly_rasters_ISIMIP_tif/GFDL-ESM4/histor", recursive = T, full.names = T, pattern = ".tif" )

datestring_histor <- unique(substring(basename(ISIMIP_montly_files_gr_histor), 1, 8))
dates_histor <- ymd(datestring_histor)
# dates_histor <- unique(round_date(dates_histor, "month"))
datestring_histor <- gsub("-", "", dates_histor)


ISIMIP_montly_files_gr_ssp <- dir("D:/ISIMIP_netCDF/output/SH/SouthernHemisphereSDM/southAmerica/monthly_rasters_ISIMIP_tif/GFDL-ESM4/ssp126", recursive = T, full.names = T, pattern = ".tif" )

datestring_ssp <- unique(substring(basename(ISIMIP_montly_files_gr_ssp), 1, 8))
dates_ssp <- ymd(datestring_ssp)
# dates_ssp <- unique(round_date(dates_ssp, "month"))
datestring_ssp <- gsub("-", "", dates_ssp)

# argentina <- extent(-95.449219, -21.796875, -70.140364, -12.897489) old
# southAfrica <- extent(-17.578125, 72.421875,-64.774125,-13.752725) old
# australiaNZ <- extent(84.199219, 180, -52.802761, 0.351560) 

argentina <- extent(-124, 3, -70, 20) # new, not done yet
southAfrica <- extent(-21.796875,70.488281, -50.176898, 15.453680)# new, not done yet
australiaNZ <- extent(105, 180, -65, -2) # new







SOextents <- c(argentina, southAfrica, australiaNZ)

names_SOextents <- c("southAmerica", "southAfrica", "australiaNZ")


bathy_list <- list()

  
for (b in 1:length(SOextents)){
     extent <- SOextents[[b]] 
     m <- raster(extent, res = 1, crs = crs("+proj=longlat +datum=WGS84"))
     rc <- raster::crop(bathy, extent)  # crop by extent
     rs <- raster::resample(rc, m, method="bilinear")  # resample
     names(rs) <- paste0("BAT_", names_SOextents[b])
     bathy_list[[b]] <- rs
}

ssp <- c("histor", "ssp126", "ssp370", "ssp585")


system.time(
for (h in 1:length(SOextents)) {
  extent <- SOextents[[h]]
  name_SOextent <- names_SOextents[h]
  
        for (i in 1:length(unique(catalog$product))){
          print(i)
          model <- unique(catalog$product)[i]
          if (model == "MRI-ESM2-0") next
  
                  for (j in 1:length(unique(catalog$service))){
                    print(j)
                    ssp <- catalog$service[j]

                    if(ssp == "histor"){
                      dates <- dates_histor
                      datestring <- datestring_histor
                    }   else if (ssp %like% "ssp") {
                      dates <- dates_ssp
                      datestring <- datestring_ssp
                    }
    
                          for (k in seq_along(datestring)){
                            print(k)
                            ISIMIP_montly_files_gr_ <- ISIMIP_montly_files_gr[which(ISIMIP_montly_files_gr %like% datestring[k] 
                                                                                    & ISIMIP_montly_files_gr %like% model 
                                                                                    & ISIMIP_montly_files_gr %like% ssp
                                                                                    &ISIMIP_montly_files_gr %like% name_SOextent)]
        stack_dynamic <- stack()
            
            for(q in 1:length(ISIMIP_montly_files_gr_)){
              ISIMIP_montly_files_gr_r<- raster(ISIMIP_montly_files_gr_[q])
              ISIMIP_montly_files_gr_r_<- setExtent(ISIMIP_montly_files_gr_r, extent)
              
              
              if (ISIMIP_montly_files_gr_r_@file@name %like% "tos"){
                names(ISIMIP_montly_files_gr_r_) <- "tos"
              } else if (ISIMIP_montly_files_gr_r_@file@name %like% "intpp"){
                names(ISIMIP_montly_files_gr_r_) <- "intpp"
              } else if (ISIMIP_montly_files_gr_r_@file@name %like% "so-surf"){
                names(ISIMIP_montly_files_gr_r_) <- "so.surf"}

              if (name_SOextent == "australiaNZ" & model == "IPSL-CM6A-LR" & ssp %notlike% "histor" & names(ISIMIP_montly_files_gr_r_) %like% "tos"){
                
                ISIMIP_montly_files_gr_r_ <- resample(ISIMIP_montly_files_gr_r, stack_dynamic[[q-1]], method='bilinear')

                              }
              
              stack_dynamic <- stack(stack_dynamic , ISIMIP_montly_files_gr_r_)
              
            }
      
      
      bathymetry <- bathy_list[[h]]
      bathymetry_resampled <- raster::resample(bathymetry, stack_dynamic[[1]],  method="bilinear")  # resample
     
     # combine with static stack
      bathymetry_resampled_ <-  raster::mask(bathymetry_resampled, stack_dynamic[[1]])
      bathymetry_resampled_[bathymetry_resampled_ >= 0] <- NA
      s <- stack(bathymetry_resampled_, stack_dynamic)
     
       names(s) <- ifelse(names(s) %like% "BAT", "BAT",
                   ifelse(names(s) %like% "so.surf", "SAL",
                   ifelse(names(s) %like% "tos", "SST",
                   ifelse(names(s) %like% "intpp", "NPP", ""))))
      
      
      s_ <- mask(s, bathymetry_resampled_)
      u <- raster(extent, res = 1, crs = crs("+proj=longlat +datum=WGS84"))
      
      
      s_resampled <- raster::resample(s_, u,  method="bilinear")
      
      
      
      
      
      
      
      # set/create folder
      YYYY <- year(dates[k])
      # product_folder <- paste(outdir, model, ssp, YYYY, sep ="/")  # Set folder
      product_folder <- paste0(outdir, "/",  name_SOextent, "/monthly_stacks_ISIMIP_tif/", model,  sep="/", ssp, sep="/", YYYY)
      if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
      
      # store file in GRD format
      outfile <- paste0(product_folder,"/", datestring[k], "_", model, "_", ssp, "_monthly_ISIMIP_stack_", name_SOextent, ".grd")
      writeRaster(s_resampled, outfile, bandorder='BIL', overwrite=TRUE)
      
    }}}}
)

 nc<-nc_open("D:/ISIMIP_netCDF/models/IPSL-CM6A-LR/ssp126/ipsl-cm6a-lr_r1i1p1f1_ssp126_chl_onedeg_global_monthly_2015_2100.nc")



var <-ncvar_get(nc, varid="chl", start=c(1,1,1,1), count=c(-1,-1, 1, -1))


plot(var)
lat<-ncvar_get(nc, varid="lat")
lon<-ncvar_get(nc, varid="lon")
time<-ncvar_get(nc, varid="time")
time<-floor(time)

ee<-raster(var[,,1])


plot(ee)

var <-ncvar_get(nc, varid=nc_var)



nc<-nc_open("D:/ISIMIP_netCDF/models/GFDL-ESM4/ssp126/gfdl-esm4_r1i1p1f1_ssp126_chl_onedeg_global_monthly_2015_2100.nc")

tt <-nc_open("D:/ISIMIP_netCDF/models/MPI-ESM1-2-HR/ssp126/mpi-esm1-2-hr_r1i1p1f1_ssp126_chl_onedeg_global_monthly_2015_2100.nc")

tt <-nc_open("D:/ISIMIP_netCDF/models/IPSL-CM6A-LR/histor/ipsl-cm6a-lr_r1i1p1f1_historical_phydiat-vint_onedeg_global_monthly_1850_2014.nc")

ncvar_get(ww)
raster(ww)




# bathymetry_pr <-raster::crop(stack_dynamic, bathymetry)
extent(stack_dynamic_pr) == extent(bathymetry)
stack_dynamic_pr <-setExtent(stack_dynamic, extent(bathymetry))
stack_dynamic_pr_ <-raster::crop(bathymetry, stack_dynamic_pr)


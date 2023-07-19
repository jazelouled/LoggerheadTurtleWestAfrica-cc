
library(data.table)
library(raster)
library(lubridate)


# create layers for predict (ensemble of each scenario for all models. e.g. averaged 126 for gfdl, ipsl, mpi and ukesm)

jj


ISIMIP_files_fit_argentina <- dir("D:/ISIMIP_netCDF/output/SH/SouthernHemisphereSDM/southAmerica//monthly_stacks_ISIMIP_tif", recursive = T, full.names = T )


ISIMIP_files_fit_soutAfrica <- dir("D:/ISIMIP_netCDF/output/SH/SouthernHemisphereSDM/southAfrica/monthly_stacks_ISIMIP_tif", recursive = T, full.names = T )

ISIMIP_files_fit_AusNZ <- dir("D:/ISIMIP_netCDF/output/SH/SouthernHemisphereSDM/australiaNZ/monthly_stacks_ISIMIP_tif/", recursive = T, full.names = T )


ISIMIP_files_fit <- list(ISIMIP_files_fit_argentina, ISIMIP_files_fit_soutAfrica, ISIMIP_files_fit_AusNZ)

names_SOextents <- c("argentina", "southAfrica", "australiaNZ")






# if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)  # create output directory if does not exist

datestring <- substr(basename(ISIMIP_files_fit_argentina), 1,8)

datestring_hist <- datestring[datestring <= 20141201]

datestring_ssp <- datestring[datestring > 20141201]

ssp <- c("histor", "ssp126", "ssp370", "ssp585")



names_SOextents <- c("southAmerica", "southAfrica", "australiaNZ")



for (k in 1:length(ISIMIP_files_fit)) {
  
  ISIMIP_files_fit_ <- ISIMIP_files_fit[[k]]
  ISIMIP_files_fit_ <- ISIMIP_files_fit_[ISIMIP_files_fit_ %like% ".grd"]
  
  outdir <- paste("D:/ISIMIP_netCDF/output/SH/SouthernHemisphereSDM", names_SOextents[k], sep = "/")
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)  # create output directory if does not exist
  

for(t in 1:length(ssp)){
  print(t)
  ISIMIP_files_fit_ssp <- ISIMIP_files_fit_[ISIMIP_files_fit_ %like% ssp[t]]
  
  ssp_ <- ssp[t]
  
  if (ssp_ == "histor"){
    datestring <- datestring_hist
    
  } else if (ssp_ %like% "ssp") {
    datestring <- datestring_ssp
    
  }
  
  for(h in 1:length(unique(substr(datestring, 1, 4)))){
    print(h)
    ISIMIP_files_fit_year <- ISIMIP_files_fit_ssp[year(ymd(substr(basename(ISIMIP_files_fit_ssp), 1,8))) == unique(substr(datestring, 1, 4))[h]]
    
    for(r in 1:length(unique(substr(datestring, 5, 8)))) {
      
      ISIMIP_files_fit_month <- ISIMIP_files_fit_year[substr(basename(ISIMIP_files_fit_year), 5, 8) %like% unique(substr(datestring, 5, 8))[r]]
      
      aa <- stack(ISIMIP_files_fit_month)
      
      # With DIAT in UKESM. [1] is just to take one of the strings. All of them should contain histor or ssp. 
      
      vars<-c("BAT", "NPP", "SAL", "SST")
      
      
      bb <- stackApply(aa, indices = vars, fun=mean, na.rm =T)
      
      bb_ <- raster::mask(bb, bb$index_BAT)
      
      names(bb_) <- ifelse(names(bb_) %like% "BAT", "BAT",
                           ifelse(names(bb_) %like% "SAL", "SAL",
                           ifelse(names(bb_) %like% "SST", "SST",
                           ifelse(names(bb_) %like% "NPP", "NPP", ""))))
                                                       
     
      
      # set/create folder
      YYYY <- unique(substr(datestring, 1, 4))[h]
      # product_folder <- paste(outdir, model, ssp, YYYY, sep ="/")  # Set folder
      
      # product_folder <- paste0(outdir, "/", ssp_, "/",  YYYY, sep = "/")
      
      
      product_folder <- paste0(outdir, "/monthly_ESMensemble_stacks/", ssp_, sep="/", YYYY)
      
      
      if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
      
      
      # store file in GRD format
      outfile <- paste0(product_folder, sep ="/", YYYY, unique(substr(datestring, 5, 8))[r], "_monthly_ESMensemble_stack_", ssp_, "_ISIMIP.grd")
      writeRaster(bb_, outfile, bandorder='BIL', overwrite=TRUE)
      
      
    } } }}








# Yearly means from ESM monthly stacks ------------------------------------



# create layers for predict (ensemble of each scenario for all models. e.g. averaged 126 for gfdl, ipsl, mpi and ukesm)




ISIMIP_files_fit_argentina <- dir("D:/ISIMIP_netCDF/output/SouthernHemisphereSDM/argentina/monthly_stacks_ISIMIP_tif", recursive = T, full.names = T )


ISIMIP_files_fit_soutAfrica <- dir("D:/ISIMIP_netCDF/output/SouthernHemisphereSDM/southAfrica/monthly_stacks_ISIMIP_tif", recursive = T, full.names = T )

ISIMIP_files_fit_AusNZ <- dir("D:/ISIMIP_netCDF/output/SouthernHemisphereSDM/australiaNZ/monthly_stacks_ISIMIP_tif/", recursive = T, full.names = T )


ISIMIP_files_fit <- list(ISIMIP_files_fit_argentina, ISIMIP_files_fit_soutAfrica, ISIMIP_files_fit_AusNZ)

names_SOextents <- c("argentina", "southAfrica", "australiaNZ")






# if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)  # create output directory if does not exist

datestring <- substr(basename(ISIMIP_files_fit_argentina), 1,8)

datestring_hist <- datestring[datestring <= 20141201]

datestring_ssp <- datestring[datestring > 20141201]

ssp <- c("histor", "ssp126", "ssp370", "ssp585")



names_SOextents <- c("argentina", "southAfrica", "australiaNZ")



for (k in 1:length(ISIMIP_files_fit)) {
  
  ISIMIP_files_fit_ <- ISIMIP_files_fit[[k]]
  ISIMIP_files_fit_ <- ISIMIP_files_fit_[ISIMIP_files_fit_ %like% ".grd"]
  
  outdir <- paste("D:/ISIMIP_netCDF/output/yearlyMean_stacks_SH_BRT", names_SOextents[k], sep = "/")
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)  # create output directory if does not exist
  
  
  for(t in 1:length(ssp)){
    print(t)
    ISIMIP_files_fit_ssp <- ISIMIP_files_fit_[ISIMIP_files_fit_ %like% ssp[t]]
    
    ssp_ <- ssp[t]
    
    if (ssp_ == "histor"){
      datestring <- datestring_hist
      
    } else if (ssp_ %like% "ssp") {
      datestring <- datestring_ssp
      
    }
    
    for(h in 1:length(unique(substr(datestring, 1, 4)))){
      print(h)
      ISIMIP_files_fit_year <- ISIMIP_files_fit_ssp[year(ymd(substr(basename(ISIMIP_files_fit_ssp), 1,8))) == unique(substr(datestring, 1, 4))[h]]
      
      for(r in 1:length(unique(substr(datestring, 5, 8)))) {
        
        ISIMIP_files_fit_month <- ISIMIP_files_fit_year[substr(basename(ISIMIP_files_fit_year), 5, 8) %like% unique(substr(datestring, 5, 8))[r]]
        
        aa <- stack(ISIMIP_files_fit_month)
        
        # With DIAT in UKESM. [1] is just to take one of the strings. All of them should contain histor or ssp. 
        
        vars<-c("BAT", "NPP", "SAL", "SST")
        
        
        bb <- stackApply(aa, indices = vars, fun=mean, na.rm =T)
        
        bb_ <- raster::mask(bb, bb$index_BAT)
        
        names(bb_) <- ifelse(names(bb_) %like% "BAT", "BAT",
                             ifelse(names(bb_) %like% "SAL", "SAL",
                                    ifelse(names(bb_) %like% "SST", "SST",
                                           ifelse(names(bb_) %like% "NPP", "NPP", ""))))
        
        
        
        # set/create folder
        YYYY <- unique(substr(datestring, 1, 4))[h]
        # product_folder <- paste(outdir, model, ssp, YYYY, sep ="/")  # Set folder
        product_folder <- paste0(outdir, "/", ssp_, "/",  YYYY, sep = "/")
        if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
        
        
        # store file in GRD format
        outfile <- paste0(product_folder, YYYY, unique(substr(datestring, 5, 8))[r], "_monthly_ESMensemble_stack_", ssp_, "_ISIMIP.grd")
        writeRaster(bb_, outfile, bandorder='BIL', overwrite=TRUE)
        
        
      } } }}








# Yearly means from ESM monthly stacks ------------------------------------



# create layers for predict (ensemble of each scenario for all models. e.g. averaged 126 for gfdl, ipsl, mpi and ukesm)



ISIMIP_files_fit_argentina <- dir("D:/ISIMIP_netCDF/output/SH/SouthernHemisphereSDM/southAmerica/monthly_ESMensemble_stacks/", recursive = T, full.names = T )


ISIMIP_files_fit_soutAfrica <- dir("D:/ISIMIP_netCDF/output/SH/SouthernHemisphereSDM/southAfrica/monthly_ESMensemble_stacks/", recursive = T, full.names = T )

ISIMIP_files_fit_AusNZ <- dir("D:/ISIMIP_netCDF/output/SH/SouthernHemisphereSDM/australiaNZ/monthly_ESMensemble_stacks/", recursive = T, full.names = T )


ISIMIP_files_fit <- list(ISIMIP_files_fit_argentina, ISIMIP_files_fit_soutAfrica, ISIMIP_files_fit_AusNZ)

names_SOextents <- c("southAmerica", "southAfrica", "australiaNZ")






# if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)  # create output directory if does not exist

datestring <- substr(basename(ISIMIP_files_fit_argentina), 1,8)

datestring_hist <- datestring[datestring <= 20141201]

datestring_ssp <- datestring[datestring > 20141201]

ssp <- c("histor", "ssp126", "ssp370", "ssp585")



names_SOextents <- c("southAmerica", "southAfrica", "australiaNZ")



for (k in 1:length(ISIMIP_files_fit)) {
  
  ISIMIP_files_fit_ <- ISIMIP_files_fit[[k]]
  ISIMIP_files_fit_ <- ISIMIP_files_fit_[ISIMIP_files_fit_ %like% ".grd"]
  
  outdir <- "D:/ISIMIP_netCDF/output/SH/SouthernHemisphereSDM"
  
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)  # create output directory if does not exist
  
  
  for(t in 1:length(ssp)){
    print(t)
    ISIMIP_files_fit_ssp <- ISIMIP_files_fit_[ISIMIP_files_fit_ %like% ssp[t]]
    
    ssp_ <- ssp[t]
    
    if (ssp_ == "histor"){
      datestring <- datestring_hist
      
    } else if (ssp_ %like% "ssp") {
      datestring <- datestring_ssp
      
    }
    
    for(h in 1:length(unique(substr(datestring, 1, 4)))){
      print(h)
      ISIMIP_files_fit_year <- ISIMIP_files_fit_ssp[year(ymd(substr(basename(ISIMIP_files_fit_ssp), 1,8))) == unique(substr(datestring, 1, 4))[h]]
      
      
      aa <- stack(ISIMIP_files_fit_year)
      
      # With DIAT in UKESM. [1] is just to take one of the strings. All of them should contain histor or ssp. 
      
      vars<-c("BAT", "NPP", "SAL", "SST")
      
      
      bb <- stackApply(aa, indices = vars, fun=mean, na.rm =T)
      
      bb_ <- raster::mask(bb, bb$index_BAT)
      
      names(bb_) <- ifelse(names(bb_) %like% "BAT", "BAT",
                           ifelse(names(bb_) %like% "SAL", "SAL",
                                  ifelse(names(bb_) %like% "SST", "SST",
                                         ifelse(names(bb_) %like% "NPP", "NPP", ""))))
      
      
      
      # set/create folder
      YYYY <- unique(substr(datestring, 1, 4))[h]
      # product_folder <- paste(outdir, model, ssp, YYYY, sep ="/")  # Set folder
      product_folder <- paste0(outdir, "/", names_SOextents[k], "/", "yearlyMean_ESM_stacks/")
      if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
      
      
      # store file in GRD format
      outfile <- paste0(product_folder, YYYY, "_", ssp_, "_", names_SOextents[k],"_ISIMIP.grd")
      writeRaster(bb_, outfile, bandorder='BIL', overwrite=TRUE)
      
      
    } } }













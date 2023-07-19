#-------------------------------------------------------------------------------------
# 03_regularize_ssm     Interpolate tracks into regular time steps using foieGras
#-------------------------------------------------------------------------------------
#
# Main steps are:
# - Regularize tracks

# devtools::install_github("ianjonsen/foieGras")  # github version fixes problem with GPS
# library("foieGras")
input_data <- "~/Dropbox/2023_LoggerheadWestAfrica_cc/00input/"
output_data <- "~/Dropbox/2023_LoggerheadWestAfrica_cc/00output/"

require(devtools)
install_version("foieGras", version = "0.7.6", repos = "http://cran.us.r-project.org")
library(foieGras)

#---------------------------------------------------------------
# Prepare cluster
#---------------------------------------------------------------
library(doParallel)
cores <- 3
cl <- makeCluster(cores)
registerDoParallel(cl)

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- paste0(input_data, "L1_locations")
outdir <- paste0(output_data, "tracking", "/L2_locations")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

#---------------------------------------------------------------
# 2. Import data
#---------------------------------------------------------------

# import all location files
loc_files <- list.files(indir, full.names = TRUE, pattern = "L1_loc.csv")
df <- readTrack(loc_files)
df$date <- parse_date_time(df$time, orders = c("%d/%m/%Y %H:%M", "%Y-%m-%d %H:%M:%S"))

#---------------------------------------------------------------
# 2. Select trips to run the SSM
#---------------------------------------------------------------

# summarize data per trip
trips <- summarizeTrips(df)

# # plot histograms
# ggplot(trips, aes(x=duration_h)) +
#   geom_histogram() +
#   geom_vline(aes(xintercept=sel_min_dur),
#              color="blue", linetype="dashed", size=1)
# 
# ggplot(trips, aes(x=n_loc)) +
#   geom_histogram() +
#   geom_vline(aes(xintercept=sel_min_loc),
#              color="blue", linetype="dashed", size=1)
# 
# ggplot(trips, aes(x=distance_km)) +
#   geom_histogram() +
#   geom_vline(aes(xintercept=sel_min_dist),
#              color="blue", linetype="dashed", size=1)

# filter trips
trips <- dplyr::filter(trips,
                duration_h >= sel_min_dur,
                n_loc >= sel_min_loc,
                distance_km >= sel_min_dist, 
                !tripID %in% sel_exclude)



tags <- unique(trips$tripID)

# tags <- unique(trips$id[94:100])


#---------------------------------------------------------------
# 3. Regularize each track using a SSM
#---------------------------------------------------------------
library(aniMotum)
library(TMB)

# foreach(i=tags, .packages=c("dplyr", "ggplot2", "aniMotum", "stringr")) %dopar% {

  
  for(i in 1:length(tags)){
 
  print(paste("Processing tag", tags[i]))

  # subset data
  # filter by id and selected trips
  
  # data <- filter(df, id == i, trip %in% trips$trip)
  data <- dplyr::filter(df, tripID == tags[i], tripID %in% trips$tripID)
  
  
  ###### State-Space Model
  
  # convert to foieGras format
  if(tag_type == "GPS") indata <- data %>% dplyr::select(tripID, date, argosLC, longitude, latitude) %>% dplyr::rename(id = tripID) 
  if(tag_type == "PTT") indata <- data %>% dplyr::select(tripID, date, argosLC, longitude, latitude, smaj, smin, eor) %>% dplyr::rename(id = tripID) 
  
  
  # filter location class data with NA values
  # very few cases, but creates an error in fit_ssm
  # if (unique(indata$sex) != "females" & unique(indata$season) != "summer") {
  # indata <- dplyr::filter(indata, !is.na(lc))
  # 
  # } else (
  #   
  #   indata$lc <- "G"
  #   
  # )
  
  # indata <- indata %>% select (-c(sex, season))
  # fit SSM
  # we turn sdafilter off because we previously filtered data
  # we run the model with multiple trips at once
  
  colnames(indata) <- c("id", "date", "lc", "lon", "lat")
  
  fit <- fit_ssm(indata,
                 vmax= filt_vmax,
                 ang = filt_ang,
                 distlim = filt_distlim,
                 model = "crw",
                 time.step = reg_time_step,
                 control = ssm_control(verbose = 0))
  

  # fit <- fit_ssm(indata, model = "crw", time.step = reg_time_step, control = ssm_control(verbose = 0), spdf = FALSE)
  
  # get fitted locations
  # segments that did not converge were not consider
  data <- data.frame(grab(fit, what = "predicted", as_sf = FALSE))
  data <- data %>% rename(trip = id) %>% arrange(date)
  data <- cbind(id = i, data)
  
  # check if points on land
  #data$onland <- point_on_land(lat = data$lat, lon = data$lon, land = land)
  
  # export track data into individual folder at output path
  out_file <- paste0(outdir, "/", tags[i], "_L2_locations.csv")
  write.csv(data, out_file, row.names = FALSE)
  
  # export convergence status
  convergence <- data.frame(trip = fit$id, converged = fit$converged)
  out_file <- paste0(outdir, "/", tags[i], "_L2_convergence.csv")
  write.csv(convergence, out_file, row.names = FALSE)
  
  # plot figures
  # plot(fit, what = "predicted", type = 1)
  # plot(fit, what = "fitted", type = 2)
  # fmap(fit, what = "predicted", obs = FALSE)
  p <- mapL1(data = data)
  out_file <- paste0(outdir, "/", tags[i], "_L2_locations.png")
  ggsave(out_file, p, width=30, height=15, units = "cm")
 }
  
stopCluster(cl) # Stop cluster

#---------------------------------------------------------------
# 4. Summarize processed data
#---------------------------------------------------------------

# import all location files
loc_files <- list.files(outdir, full.names = TRUE, pattern = "L2_locations.csv")
df <- readTrack(loc_files)

# import convergence files
loc_files <- list.files(outdir, full.names = TRUE, pattern = "L2_convergence.csv")
data_proc <- lapply(loc_files, read.csv) %>% rbindlist

# summarize data per trip
tripstats <- summarizeTrips2(df)

# combine track data summary and convergence status
comb <- merge(tripstats, data_proc, by=c("trip"))

# export table
out_file <- paste0(outdir, "summary_ssm.csv")
write.csv(comb, out_file, row.names = FALSE)


print("Regularization ready")

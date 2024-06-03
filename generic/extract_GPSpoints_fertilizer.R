


################################################################################
################################################################################
# install & load packages
packages_required <- c("terra", "tidyverse", "sf", "lubridate", "plyr")
# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

################################################################################
################################################################################

#' Title a function to extract point based advisory
#'
#' @param crop is among c(Maize, Sorghum, Tef and Wheat)
#' @param ExtractionRound reflects the batch of the GPS readings (it is send in batches)
#'
#' @return writes out a cvs file in "~/shared-data/Data/... /result/final_advisory/" ... filed by the crop
#' @examples extractAdvice(crop = "Maize", ExtractionRound = 2)
extractAdvice <- function(crop, ExtractionRound){
  
  #read raster fertilizer rate data
  pathIn <- paste("~/shared-data/Data", crop, "result/geoSpatial/final_rate_raster", sep = "/")
  rate <- list.files(path = pathIn, pattern = "n.tif|p.tif|yield.tif", full.names = T)
  rate_rast <- rast(rate)
  names(rate_rast) <- c("N", "P", "Yield")
  plot(rate_rast)
  
  #calculate average values of rate in each location to reduce variability in neighboring locations
  rate_avg_rast2 <- rate_rast |> terra::focal(w = 9, fun = "mean", 
                                              na.policy = 'omit', na.rm = TRUE) 
  
  #fill empty cell values with the dominant cell values
  rate_rast2 <- rate_avg_rast2 |> terra::focal(w = 25, fun = "modal", 
                                               na.policy = 'only', na.rm = TRUE)
  
  plot(rate_rast2)
  #read the field gps points
  path_gps <- paste("~/shared-data/Data", crop, "fieldData/rate_extraction_pts/", sep = "/")
  gps_pts <- read.csv(paste0(path_gps, "gps_coordinate_maize_", ExtractionRound, ".csv"), header = T, sep = ",") |>
    terra::vect(geom = c("long", "lat"), crs = "epsg:4326")
  plot(rate_rast2[[1]])
  plot(gps_pts, add = T)
  
  rate_extracted <- rate_rast2 |> terra::extract(gps_pts, ID = F)
  dim(rate_extracted)
  # View(rate_extracted)
  
  #bind and write the csv file
  final_advisory <- as.data.frame(gps_pts |>  cbind(rate_extracted))
  
  #round NPS & Urea to the nearest ten
  final_advisory$Urea <- as.integer(final_advisory$N/0.46) %>% round_any(5)
  final_advisory$TSP <- as.integer(final_advisory$P/0.205) %>% round_any(5)
  final_advisory$N <- round(final_advisory$N, digits = 0)
  final_advisory$P <-  round(final_advisory$P, digits = 0)
  final_advisory$Yield <-  round(final_advisory$Yield, digits = 0)
  final_advisory$P2O5 <- round((final_advisory$P*141.94)/61.94, digits = 0)
  final_advisory$NP2O5 <- round(final_advisory$N / final_advisory$P2O5, digits=1)
  
  
  pathOut <- paste("~/shared-data/Data", crop, "result/final_advisory/", sep = "/")
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)  }
  
  out_filename <- paste0(crop, "_Advice_", ExtractionRound,".csv") 
  
  write.csv(final_advisory, paste0(pathOut, out_filename), row.names = FALSE)
  
}

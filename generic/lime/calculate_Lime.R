# ------------------------------------------------------------------------------
# Calculate lime rate for different countries and different crops using soil data 
# ISRIC soil grids. The data includes - bulk density and exchangeable acidity 
# for three depths(0-5, 5-15 & 15-30cm), exchangeable potassium, exchangeable 
# calcium, exchangeable magnesium and exchangeable sodium for 20cm depth.

#' @description - this function uses three different lime rate calculation 
#' methods from the limer package.It uses predefined values of target aluminium 
#' saturation for different crops to calculate the lime rates of crops.
#' 
#' @param country country name to be used to mask the cropland raster file to be 
#' used to further mask the calculated lime rate. 
#' @param useCaseName is the Name of the usecase
#' @param crops the name of the crop for selecting the target aluminium 
#' saturation values to be used in the lime rate calculation. The crops should 
#' be maize, sorghum, teff or wheat
#' @example - calculate_limeRate("wheat")

calculate_limeRate <- function(crops){
  
  #install & load packages
  packages_required <- c("terra", "tidyverse", "geodata", "remotes")
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])}
  
  # load required packages
  invisible(lapply(packages_required, library, character.only = TRUE))
  
  # install and load limer package
  limer_pkg <- "gaiafrica/limer"
  installed_git_packages <- limer_pkg %in% rownames(installed.packages())
  if(any(installed_git_packages == FALSE)){
    remotes::install_github(limer_pkg, quiet = T)}
  require("limer", quietly = TRUE)
  
  # load the isric layers from path
  pathIn_soil <- "~/Eth_DST_Harmonization/data/national_Data/raw/geospatial/soil/soil_Grids/"
  
  # load bulk density of the first three depths and calculate weighted average
  bdod <- list.files(path = pathIn_soil, pattern = ("bdod_0-5cm|bdod_5-15cm|bdod_15-30cm"), full.names = T) |> 
          rast()
  bdod_mean <- (5 * bdod[[1]] + 10 * bdod[[3]] + 15 * bdod[[2]]) / 30
  
  # load exchangeable acidity of the first three depths and calculate weighted average
  acid_exch <- list.files(path = pathIn_soil, pattern = ("af_acid-exch_0-5cm|af_acid-exch_5-15cm|af_acid-exch_15-30cm"), full.names = T) |> 
              rast()
  acid_exch_mean <- (5*acid_exch[[1]] + 10*acid_exch[[3]] + 15*acid_exch[[2]]) / 30
  
  # load 20cm depth exchangeable K, Ca, Mg & Na 
  exch_cakmgna <- list.files(path = pathIn_soil, pattern = ("af_ca|af_k|af_mg|af_na"), full.names = T) |> 
    rast()
  
  # exchangeable acidity saturation
  ecec <- acid_exch_mean + exch_cakmgna [[1]] + exch_cakmgna [[2]] +
    exch_cakmgna [[3]] + exch_cakmgna [[4]]
  hp_sat <- 100 * acid_exch_mean / ecec
  hp_sat_acid <- terra::classify(hp_sat, rcl = cbind(-1, 10, 0))
  hp_sat_acid <- terra::ifel(hp_sat_acid != 0, 1, hp_sat_acid)
  
  # calculate calcium saturation
  ca_sat <- 100 * exch_cakmgna[[1]] / ecec
  
  # read the growing area raster for the crop specified resmaple and mask the soil rasters 
  path_CropMask <- "~/Eth_DST_Harmonization/data/national_Data/raw/geospatial/crop_Mask/"
  crop_mask <- terra::rast(paste(path_CropMask, paste0(tolower(crops), ".tif"), sep = "/")) 
  crop_mask <- crop_mask |>
            terra::resample(ca_sat)
  
  # assemble all raster
  x <-
    c(acid_exch_mean,
      exch_cakmgna[[2]],
      exch_cakmgna[[1]],
      exch_cakmgna[[3]],
      exch_cakmgna[[4]],
      bdod_mean) |> 
    terra::crop(crop_mask) |>
    terra::mask(crop_mask)
  names(x) <- c('exch_ac', 'exch_k', 'exch_Ca', 'exch_mg', 'exch_na', 'SBD')  
  
  # create a data frame for target aluminium saturation values for crops
  crop <- c("Cabbage", "Sorghum", "Carrot", "Barely", "Tomato", "Wheat", 
             "Faba bean", "Sweet potato", "Sunflower", "Haricot bean", "Pepper",  
             "Maize",  "Cotton",  "Groundnut", " Rape seed", "Potato", "Onion", 
             "Teff") |> tolower()
  value <- c(1, 10, 1, 10, 1, 10, 5, 10, 5, 20, 5, 20, 5, 20, 5, 30, 5, 40)
  tas <- data.frame(crop, value)
  
  # select tas value from crop parameter
  t <- tas |> dplyr::filter(crop == tolower(crops)) |>
    dplyr::pull(value)
  
  # calculate lime rate using different methods
  # kamprath - the same for all crops
  caco3_kamprath <-
    limer::limeRate(
      x,
      method = 'ka',
      check_Ca = FALSE,
      unit = 't/ha',
      SD = 20
    )
  caco3_kamprath_filter <-
            (caco3_kamprath * hp_sat_acid)  # for soils with exch acidity saturation > 10% only
  # cochrane
  caco3_cochrane <-
    limer::limeRate(
      x,
      method = 'co',
      check_Ca = FALSE,
      unit = 't/ha',
      SD = 20,
      TAS = t
    ) 
  
  # aramburu-merlos - (litas) method
  caco3_merlos <-
    limer::limeRate(
      x,
      method = 'my',
      check_Ca = FALSE,
      unit = 't/ha',
      SD = 20,
      TAS = t) 
  
  # write the calculated lime rate raster
  pathOut_layers <- paste("~/Eth_DST_Harmonization/data/data_Processing", 
                          crops, "geospatial/data_Driven/lime", sep = "/")
  if(!exists(pathOut_layers)){
    suppressWarnings(dir.create(pathOut_layers, recursive = T))
  }

  terra::writeRaster(caco3_kamprath_filter,
                     paste(pathOut_layers, 'lime_kamprath.tif', sep = "/"),
                     filetype = "GTiff",
                     overwrite = TRUE)
  
  terra::writeRaster(caco3_cochrane,
                     paste(pathOut_layers, 'lime_cochrane.tif', sep = "/"),
                     filetype = "GTiff",
                     overwrite = TRUE)
  
  terra::writeRaster(caco3_merlos,
                     paste(pathOut_layers, 'lime_merlos.tif', sep = "/"),
                     filetype = "GTiff",
                     overwrite = TRUE)
}
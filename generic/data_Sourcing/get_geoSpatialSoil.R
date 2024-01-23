# ------------------------------------------------------------------------------
#' Extraction of EthioSIS soil nuitrient covariate using field trial data.
#' @description - The function masks the EthioSIS soil data with the growing 
#' area of each crop. It also Extracts the soil covariates using the trial 
#' data and write the outputs for the crop specified.
#' @param crops - the crop in which the data is generated
#' example - get_geospatialTopography("wheat")
# ------------------------------------------------------------------------------ 

get_geoSpatialSoil <- function(crops){
  
  packages_required <- c("terra", "sf", "geodata", "tidyverse", "readxl")
  
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])}
  
  # load required packages
  invisible(lapply(packages_required, library, character.only = TRUE))
  
  # read the elevation data stored in a path 
  pathIn_Soil <- "~/Eth_DST_Harmonization/data/national_Data/raw/geospatial/soil/soil_Grids/"
  soil <- list.files(path = pathIn_Soil, pattern = "*.tif$", full.names = T) |> 
    rast()
  soil <- c(soil)
  
  # read the growing area raster for the crop specified 
  path_CropMask <- "~/Eth_DST_Harmonization/data/national_Data/raw/geospatial/crop_Mask/"
  crop_mask <- terra::rast(paste(path_CropMask, paste0(tolower(crops), ".tif"), sep = "/"))
  
  # mask the soil raster file by the growing area and write to the path specified
  pathOut_layers <- paste("~/Eth_DST_Harmonization/data/data_Processing", 
                          crops, "geospatial/data_Driven/soil", sep = "/")
  if(!exists(pathOut_layers)){
    suppressWarnings(dir.create(pathOut_layers, recursive = T))
  }
  crop_mask <- crop_mask |> terra::resample(soil)
  for(i in 1:nlyr(soil)){
    r <- soil[[i]] |> terra::crop(crop_mask) |> terra::mask(crop_mask)
    terra::writeRaster(r, filename = paste(pathOut_layers, paste0(names(soil)[i], ".tif"), sep = "/"), filetype = "Gtiff", overwrite = T)
  }
  
  # read the trial data and extract the topography data
  pathIn_trial <- "~/Eth_DST_Harmonization/data/national_Data/raw/field_Data"
  trial <- readxl::read_xlsx(paste(pathIn_trial, "trial_data.xlsx", sep = "/"), 
            col_names = T) |> dplyr::mutate(crop = tolower(crop)) |>
            dplyr::filter(crop == tolower(crops))
  
  points <- terra::vect(trial, geom = c("lon", "lat"), crs = "epsg:4326")
  points_soil <- soil |> terra::extract(points) |> select(-(ID))
  extracted_soil <- trial |> dplyr::select(id, lon, lat, year) |> cbind(points_soil)
  
  # export the results to intermediate path  
  pathOut_gps <- paste("~/Eth_DST_Harmonization/data/data_Processing", 
                   crops, "geospatial/data_Driven/gps", sep = "/")
  if(!exists(pathOut_gps)){
    suppressWarnings(dir.create(pathOut_gps, recursive = T))
  }
  saveRDS(extracted_soil, paste(pathOut_gps, "soil.rds", sep = "/"))
}

# ------------------------------------------------------------------------------
#' Derivation of topographic variables which includes slop, tri and tpi for SRTM 
#' DEM and extraction of these topographic covariates using field trial data
#' @description - The function derives the topographic variables from SRTM  
#' elevation data and mask with the growing area of each crop. It also Extracts   
#' the topographic covariates using the trial data and write the outputs for 
#' the crop specified.
#' @param crops - the crop in which the data is generated
#' example - get_geospatialTopography("wheat")
# ------------------------------------------------------------------------------ 

get_geospatialTopography <- function(crops){
  
  packages_required <- c("terra", "sf", "geodata", "tidyverse", "readxl")
  
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])}
  
  # load required packages
  invisible(lapply(packages_required, library, character.only = TRUE))
  
  # read the elevation data stored in a path 
  pathIn_dem <- "~/Eth_DST_Harmonization/data/national_Data/raw/geospatial/topography"
  #dem <- rast(paste(pathIn, "srtm_dem.tif", sep = "/"))
  dem <- rast(paste(pathIn_dem, "test_dem.tif", sep = "/"))
  
  # read the growing area raster for the crop specified 
  path_CropMask <- "~/Eth_DST_Harmonization/data/national_Data/raw/geospatial/crop_Mask/"
  crop_mask <- terra::rast(paste(path_CropMask, paste0(tolower(crops), ".tif"), sep = "/"))
  
  # derive slope and aspect mask the rasters with growing area of the crop 
  crop_mask <- crop_mask |> terra::resample(dem)
  dem <- dem |> terra::crop(crop_mask) |> terra::mask(crop_mask)
  slope <- terra::terrain(dem, v = 'slope') |> 
          terra::crop(crop_mask) |> terra::mask(crop_mask)
  tpi <- terra::terrain(dem, v = 'TPI') |> 
          terra::crop(crop_mask) |> terra::mask(crop_mask)
  tri <- terra::terrain(dem, v = 'TRI') |> 
          terra::crop(crop_mask) |> terra::mask(crop_mask)

  # read the trial data and extract the topography data
  pathIn_trial <- "~/Eth_DST_Harmonization/data/national_Data/raw/field_Data"
  trial <- readxl::read_xlsx(paste(pathIn_trial, "trial_data.xlsx", sep = "/"), 
            col_names = T) |> dplyr::mutate(crop = tolower(crop)) |>
            dplyr::filter(crop == tolower(crops))
  
  points <- terra::vect(trial, geom = c("lon", "lat"), crs = "epsg:4326")
  #points_dem <- dem |> terra::extract(points) |> select(srtm_dem)
  points_dem <- dem |> terra::extract(points) |> select(test_dem)
  points_slope <- slope |> terra::extract(points) |> select(slope)
  points_tpi <- tpi |> terra::extract(points) |> select(TPI)
  points_tri <- tri |> terra::extract(points) |> select(TRI)
  
  extracted_topo <- trial |> dplyr::select(id, lon, lat, year) |> cbind(points_dem, points_slope, points_tpi, points_tri)
  colnames(extracted_topo)
  # export the results to intermediate path  
  pathOut_gps <- paste("~/Eth_DST_Harmonization/data/data_Processing", 
                   crops, "geospatial/data_Driven/gps", sep = "/")
  if(!exists(pathOut_gps)){
    suppressWarnings(dir.create(pathOut_gps, recursive = T))
  }
  pathOut_layers <- paste("~/Eth_DST_Harmonization/data/data_Processing", 
                       crops, "geospatial/data_Driven/topography", sep = "/")
  if(!exists(pathOut_layers)){
    suppressWarnings(dir.create(pathOut_layers, recursive = T))
  }
  
  saveRDS(extracted_topo, paste(pathOut_gps, "topography.rds", sep = "/"))
  terra::writeRaster(
    dem,
    filename = paste(pathOut_layers,"dem.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
  terra::writeRaster(
    slope,
    filename = paste(pathOut_layers,"slope.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
  terra::writeRaster(
    tpi,
    filename = paste(pathOut_layers,"tpi.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  ) 
  writeRaster(
    tri,
    filename = paste(pathOut_layers,"tri.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
}

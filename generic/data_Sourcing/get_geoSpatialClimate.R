# ------------------------------------------------------------------------------
#' Extraction of climate monthly data using field trial data based on different 
#' crops. The climate variables in this extraction are precipitation, temp max,
#' temp min, solar radiation and relative humidity. 
#' @description - this function selects a crop from trial data, extracts climate 
#' data and finally writes the results for the crop specified. Besides it 
#' generates a raster stack for three different scenarios(above, below & normal)
#' for the ML prediction   
#' @param climate the climate variable to be extracted should be one of 
#' precipitation, tempMax, tempMin, solaRadiation & relativeHumidity.
#' @param crops the name of the crop is one of  maize, sorghum teff or wheat
#' @example - get_geoSpatialClimate("tempMax", "wheat")
# ------------------------------------------------------------------------------

get_geoSpatialClimate <- function(climate, crops){
  # install & load packages
  packages_required <- c("terra", "tidyverse", "sf", "lubridate", "plyr")
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])}
  
  # load required packages
  invisible(lapply(packages_required, library, character.only = TRUE))
  
  # read trial sample data
  pathIn_trial <- "~/Eth_DST_Harmonization/data/national_Data/raw/field_Data"
  trial <- readxl::read_xlsx(paste(pathIn_trial, "trial_data.xlsx", sep = "/"), 
                             col_names = T) |> dplyr::mutate(crop = tolower(crop)) |>
    dplyr::filter(crop == tolower(crops)) |> na.omit()
  
  # change trial data to spatial points
  points <- vect(trial, geom = c("lon", "lat"), crs = "epsg:4326")
  
  # read sowing date and growing length decade data
  pathIn_sow <- "~/Eth_DST_Harmonization/data/national_Data/raw/geospatial/sowing_Date"
  pathIn_gleng <- "~/Eth_DST_Harmonization/data/national_Data/raw/geospatial/growing_Length"
  sow_start <- rast(paste(pathIn_sow, paste0(tolower(crops), ".tif"), sep = "/")) * 10
  sow_start[sow_start <= 0 | sow_start > 365] <- NA
  grow_length <- rast(paste(pathIn_gleng, paste0(tolower(crops), ".tif"), sep = "/")) * 10
  grow_length[grow_length <= 0 | grow_length > 365] <- NA
  
  # extract the sowing start and growing length by the trial points data
  start <- sow_start |> terra::extract(points)
  grow_len <- grow_length |> terra::extract(points)
  
  grow_period <- cbind(start[, -1], grow_len[,-1]) 
  colnames(grow_period) <- c("sowing_start", "grow_length")
  
  # bind the trial data with sowing date and growing length
  trial_with_grow_len <- cbind(trial, grow_period)
  trial_with_grow_len <- trial_with_grow_len |> 
    dplyr::mutate(full_date = as.Date(paste(year, "01", "01", sep = "-"))) |>
    dplyr::mutate(pl_date = ymd(full_date) + days(sowing_start)) |>
    dplyr::mutate(hv_date = ymd(full_date) + days(sowing_start+grow_length)) |> 
    na.omit()
  
  # read the climate monthly data from different directories
  if(climate == "precipitation"){
    path <- "~/Eth_DST_Harmonization/data/national_Data/raw/geospatial/climate/rainfall/monthly"
  }else if(climate == "tempMax"){
    path <- "~/Eth_DST_Harmonization/data/national_Data/raw/geospatial/climate/temp_Max/monthly"
  }else if(climate == "tempMin"){
    path <- "~/Eth_DST_Harmonization/data/national_Data/raw/geospatial/climate/temp_Min/monthly"
  }else if(climate == "solaRadiation"){
    path <- "~/Eth_DST_Harmonization/data/national_Data/raw/geospatial/climate/solar_Radiation/monthly"
  }else if(climate == "relativeHumidity"){
    path <- "~/Eth_DST_Harmonization/data/national_Data/raw/geospatial/climate/relative_Humidity/monthly/"
  }else{
    print("Please enter a valid climate variable name")
  }
  
  unique_grow_len <- trial_with_grow_len |> dplyr::select(year, pl_date, hv_date) |>
                       unique()
  
  # loop unique planting and harvesting date to extract the climate data
  f_df <- c()
  for(i in 1:nrow(unique_grow_len)){
    print(i)
    d <- trial_with_grow_len |> dplyr::filter(pl_date == unique_grow_len$pl_date[i] & hv_date == unique_grow_len$hv_date[i])
    pl_date <- d$pl_date[1]
    hv_date <- d$hv_date[1]
    year <- d$year[1]
    d <- vect(d, geom = c("lon", "lat"), crs = "epsg:4326")
    if(pl_date < hv_date){
      r <- rast(paste(path, paste0(year,".tif"), sep = "/"))
      month_start <- as.numeric(format(pl_date, "%m"))
      month_end <- as.numeric(format(hv_date, "%m"))
      grow_len_r <- r[[month_start:month_end]]
    }else if(pl_date > hv_date){
      month_start <- as.numeric(format(pl_date, "%m"))
      month_end <- as.numeric(format(hv_date, "%m"))
      r1 <- rast(paste(path, paste0(year,".tif"), sep = "/"))
      r2 <- rast(paste(path, paste0((year)+1, ".tif"), sep = "/"))
      r <- c(r1,r2)
      grow_len_r1 <- r[[month_start:12]]
      grow_len_r2 <- r[[13:month_end + 12]]
      grow_len_r <- c(grow_len_r1, grow_len_r2)
    }
    r_df <- terra::extract(grow_len_r, d) |> select(-ID)
    colnames(r_df) <- paste0(climate, "_g_len_", seq(1,ncol(r_df),1))
    f_df <- rbind.fill(f_df, r_df) # row binds df with different column numbers
  }
  
  # convert to appropriate units
  if(climate %in% c("tempMax","tempMin")){
    f_df <- f_df - 274
  }else if (climate == "solarRadiation"){
    f_df <- f_df / 1000000
  }
  # fill na values with row means - with different growing lengths some areas have 3,
  # others have 4, others have 5. The shorter ones have NA values to match the larger
  # ones. The NA values will be filled by row means
  
  # k <- which(is.na(f_df), arr.ind = TRUE)
  # f_df[k] <- rowMeans(f_df, na.rm = TRUE)[k[, 1]]
  
  final_df <- trial_with_grow_len |> dplyr::select(id, lon, lat, year, pl_date, hv_date) |>
    cbind(f_df)
  
  # ----------------------------------------------------------------------------
  # create a prediction scenario raster
  min_pl_date <- min(format(as.Date(final_df$pl_date,format="%d/%m/%Y"),"%m")) |>
                as.numeric()
  max_hv_date <- max(format(as.Date(final_df$hv_date,format="%d/%m/%Y"),"%m")) |>
                as.numeric()
  clim_rast <- list.files(path = path, pattern = "*.tif$", full.names = T) |> 
    rast()
  
  q2_clim <- rast()
  q1_clim <- rast()
  q3_clim <- rast()
  
  for (i in 1:12){
    clim_year <- rast()
    k <- i
    for(j in 1:(nlyr(clim_rast)/12)){
      print(k)
      add(clim_year) <- clim_rast[[k]]
      k <- k + 12
    }
    q1 <- terra::quantile(clim_year, probs = 0.25)
    add(q1_clim) <- q1
    q2 <- terra::quantile(clim_year, probs = 0.5)
    add(q2_clim) <- q2
    q3 <- terra::quantile(clim_year, probs = 0.75)
    add(q3_clim) <- q3
  }
  
  clim_below <- q1_clim[[min_pl_date:max_hv_date]]
  clim_normal <- q2_clim[[min_pl_date:max_hv_date]]
  clim_above <- q3_clim[[min_pl_date:max_hv_date]]
  names(clim_above) <- names(clim_normal) <- names(clim_above) <-
    paste0(climate, "_g_len_", seq(1,nlyr(clim_below),1))
  
  # convert to appropriate units
  if(climate %in% c("tempMax","tempMin")){
    clim_above <- clim_above - 274
    clim_normal <- clim_normal - 274
    clim_below <- clim_below - 274
  }else if (climate == "solarRadiation"){
    clim_above <- clim_above / 1000000
    clim_normal <- clim_normal / 1000000
    clim_below <- clim_below / 1000000
  }
  
  # mask using the growing area of the crop
  path_CropMask <- "~/Eth_DST_Harmonization/data/national_Data/raw/geospatial/crop_Mask/"
  crop_mask <- terra::rast(paste(path_CropMask, paste0(tolower(crops), ".tif"), sep = "/"))
  crop_mask <- crop_mask |> terra::resample(clim_above)
  clim_above <- clim_above |> terra::crop(crop_mask) |> terra::mask(crop_mask)
  clim_normal <- clim_normal |> terra::crop(crop_mask) |> terra::mask(crop_mask)
  clim_below <- clim_below |> terra::crop(crop_mask) |> terra::mask(crop_mask)
  
  # ----------------------------------------------------------------------------
  # export the results to the crop path  
  pathOut_gps <- paste("~/Eth_DST_Harmonization/data/data_Processing", 
                       crops, "geospatial/data_Driven/gps", sep = "/")
  if(!exists(pathOut_gps)){
    suppressWarnings(dir.create(pathOut_gps, recursive = T))
  }
  
  pathOut_layers <- paste("~/Eth_DST_Harmonization/data/data_Processing", 
                          crops, "geospatial/data_Driven/climate", sep = "/")
  if(!exists(pathOut_layers)){
    suppressWarnings(dir.create(pathOut_layers, recursive = T))
  }
  
  final_df <- final_df |> select(-c(hv_date, pl_date))
  saveRDS(final_df, paste(pathOut_gps, paste0(climate, ".rds"), sep = "/"))
  
  terra::writeRaster(
    clim_above,
    filename = paste(pathOut_layers,paste0(climate, "_above", ".tif"), sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
  terra::writeRaster(
    clim_normal,
    filename = paste(pathOut_layers,paste0(climate, "_normal", ".tif"), sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
  terra::writeRaster(
    clim_below,
    filename = paste(pathOut_layers,paste0(climate, "_below", ".tif"), sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
}

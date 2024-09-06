

# ------------------------------------------------------------------------------
#' @description - Predicts the yield using different combination of n & p based 
#' on different climate scenario (normal, below normal and above normal).
#' @param scenario - the name of the crop 
#' @param crop - the name of the crop
#' @param nstart- the initial value for n rate
#' @param nend - the end value for n rate
#' @param ninterval - the interval for consecutive  n rates
#' @param pstart- the initial value for p rate
#' @param pend - the end value for p rate
#' @param pinterval - the interval for consecutive  p rates
#' @param model_crop - the name of the ML model trained with CoW wheat data.
#' @param zone_list zones names in the target area
#' @param pathModel the path where the trained ML model is saved. This model is used to predict at scale. 
#' @param pathOut the path to save the prediction result   
#' @example - see below the script
# ------------------------------------------------------------------------------

predict_NPKY <- function(scenario, crop, nstart, nend, ninterval,pstart, pend,
                         pinterval, model_crop, zone_list, pathModel, pathOut){
  
  
  packages_required <- c("terra","tidyverse", "pryr",  "benchmarkme", "sf",
                         "parallel", "h2o")
  
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])
  }
  
  # load required packages
  invisible(lapply(packages_required, library, character.only = TRUE))
  
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])}
  
  #load model
  h2o.init()
  
  # load the model
  saved_model <- h2o.loadModel(paste(pathModel, model_crop, sep = "/"))
  my_local_model <- h2o.download_model(saved_model, path = pathModel)
  uploaded_model <- h2o.upload_model(my_local_model)
  
  #read zone data selected for the validation (this was acquired from EIAR validation woreda list)
  eth_zone <- geodata::gadm(path = tempdir(), country = "ETH", level = 2)
  head(eth_zone)
  sort(unique(eth_zone$NAME_2), decreasing = F)
  
  
  #zone_list <- c("Misraq Gojjam", "Ilubabor", "Misraq Shewa","Mirab Welega")
  valid_zone <- eth_zone |> st_as_sf() |> dplyr::filter(NAME_2 %in% zone_list) |>
    terra::vect()
  plot(valid_zone)
  #read prediction raster
  if(scenario == "normal"){
    if(crop=="Wheat"){
      pred_stack <- rast(paste("~/shared-data/Data", crop, "geoSpatial/geo_4ML_AOI/NPrate/scenario_normal.tif", sep = "/"))|>
        terra::crop(valid_zone) |> terra::mask(valid_zone)
    }else{
      pred_stack <- rast(paste("~/shared-data/Data", crop, "geoSpatial/geo_4ML_AOI/scenario_normal.tif", sep = "/"))|>
        terra::crop(valid_zone) |> terra::mask(valid_zone)
    } 
  }else if(scenario == "above"){
    if(crop=="Wheat"){ pred_stack <- rast(paste("~/shared-data/Data", crop, "geoSpatial/geo_4ML_AOI/NPrate/scenario_above.tif", sep = "/")) |>
      terra::crop(valid_zone) |> terra::mask(valid_zone) 
    }else{
      pred_stack <- rast(paste("~/shared-data/Data", crop, "geoSpatial/geo_4ML_AOI/scenario_above.tif", sep = "/")) |>
        terra::crop(valid_zone) |> terra::mask(valid_zone) 
    }
  }else if(scenario == "below"){
    if(crop=="Wheat"){
      pred_stack_soil <- rast(paste("~/shared-data/Data", crop, "geoSpatial/geo_4ML_AOI/NPrate/scenario_below_soilDEM.tif", sep = "/")) |>
        terra::crop(valid_zone) |> terra::mask(valid_zone)  
      pred_stack_weather <- rast(paste("~/shared-data/Data", crop, "geoSpatial/geo_4ML_AOI/NPrate/scenario_below_weather.tif", sep = "/")) |>
        terra::crop(valid_zone) |> terra::mask(valid_zone) 
      pred_stack <- c(pred_stack_soil, pred_stack_weather)
    }else{
      pred_stack <- rast(paste("~/shared-data/Data", crop, "geoSpatial/geo_4ML_AOI/scenario_below.tif", sep = "/")) |>
        terra::crop(valid_zone) |> terra::mask(valid_zone)  
    }
  }else{
    print("Enter valid prediction scenario")
  }
  
  res(pred_stack)
  names(pred_stack)
  plot(pred_stack[[1]])
  
  #resample the prediction raster from 250m to 1km
  pred_stack <- pred_stack |> terra::aggregate(fact = 4, fun = "mean")
  res(pred_stack)
  
  # ------------------------------------------------------------------------------  
  # Multiple iteration
  # convert the prediction stack to data frame 
  pred_vect <- terra::as.points(pred_stack)
  plot(pred_vect, add = T)
  #include the woreda shapefile if it is trained for the model
  woreda <- geodata::gadm(path = tempdir(), country = "ETH", level = 2)
  pts_woreda <- terra::extract(woreda, pred_vect) |> as.data.frame() |>
    dplyr::select(NAME_2)
  
  pred_df <- as.data.frame(pred_vect)
  pred_df <- pred_df |> cbind(pts_woreda)
  
  pred_df$solarRad_month1 <- pred_df$solarRad_month1 / 1000000
  pred_df$solarRad_month2 <- pred_df$solarRad_month2 / 1000000
  pred_df$solarRad_month3 <- pred_df$solarRad_month3 / 1000000
  pred_df$soil_type <- as.factor(pred_df$soil_type)
  pred_df$NAME_2 <- as.factor(pred_df$NAME_2)
  pred_df <- pred_df[, c("NAME_2","totalRF","nrRainyDays","Rainfall_month1", "Tmax_month1",
                         "Rainfall_month2", "Tmax_month2","Rainfall_month3", "Tmax_month3",
                         "soil_type","TRI","TPI", "slope","Alt","P_soil","N_soil","pH_","CEC","OM" )]
  
  names(pred_df) <- c("NAME_2","totalRF","nrRainyDays","Rainfall_month1", "Tmax_month1",
                      "Rainfall_month2", "Tmax_month2","Rainfall_month3", "Tmax_month3",
                      "soil_type","TRI","TPI", "slope","Alt","P_soil","N_soil","pH","CEC","OM" )
  
  pred_df$soil_type <- as.factor(pred_df$soil_type)
  pred_coord <- data.frame(geom(pred_vect)) |> dplyr::select(x, y)
  
  N <- seq(nstart, nend, ninterval) 
  P <- seq(pstart, pend, pinterval)
  
  np <- expand.grid(N = N, P = P)
  dim(np)
  colnames(np)
  colnames(np) <- c("N_fertilizer" , "P_fertilizer")
  
  # single core 
  final_df <- c()
  for(i in 1:nrow(np)){
    print(i)
    df <- pred_df |> dplyr::mutate(N_fertilizer = np$N_fertilizer[i], P_fertilizer = np$P_fertilizer[i]) |>
      as.h2o()
    pr <- h2o::h2o.predict(uploaded_model, df) |> as.data.frame()
    colnames(pr) <- paste("yield", np$N_fertilizer[i], np$P_fertilizer[i], sep = ".")
    if(i == 1){
      final_df <- pr
    }else{
      final_df <- cbind(final_df, pr)
    }
  }
  
  
  
  if(!dir.exists(pathOut)){
    suppressWarnings(dir.create(pathOut, recursive = T))
  }
  
  final_df_sp <- cbind(pred_coord, final_df)
  saveRDS(final_df_sp, paste(pathOut, paste0("NPrate_predicted_df_", crop, "_", scenario, ".rds"), sep = "/"))
  
}

# ------------------------------------------------------------------------------
# predict the whole Ethiopian area using prediction raster
predictParallel <- function(predict = "normal", crop = "Maize"){
  
  #rm(list = ls())
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
  pathOut <- "~/shared-data/Data/Maize/Intermediate/grainwt"
  # load the model
  saved_model <- h2o.loadModel(paste(pathOut, "GBM_model_R_1713167860625_3", sep = "/"))
  my_local_model <- h2o.download_model(saved_model, path = pathOut)
  # upload the model that you just downloded above to the H2O cluster
  uploaded_model <- h2o.upload_model(my_local_model)
  
  #read zone data
  eth_zone <- geodata::gadm(path = tempdir(), country = "ETH", level = 2)
  head(eth_zone)
  sort(unique(eth_zone$NAME_2), decreasing = F)
  
  zone_list <- c("Mirab Hararghe", "Misraq Harerge", "Misraq Shewa", "Mirab Shewa",
                 "Mirab Arsi", "Horo Guduru", "Ilubabor", "Mirab Welega", 
                 "Misraq Wellega", "Jimma", "Kelem Wellega", "Semen Gondar", 
                 "Misraq Gojjam", "Mirab Gojjam", "Debub Gondar", "Agew Awi",
                 "Sidama", "Gamo Gofa", "Debub Omo", "Derashe" , "Amaro", "Gedeo",
                 "Konso","Wolayita", "Gurage", "Silti", "Hadiya", "Kembata Tembaro",
                 "Alaba", "Metekel", "Asosa", "Keffa")
  valid_zone <- eth_zone |> st_as_sf() |> dplyr::filter(NAME_2 %in% zone_list) |>
    terra::vect()
  
  #read prediction raster
  if(predict == "normal"){
    pred_stack <- rast("~/shared-data/Data/Maize/geoSpatial/geo_4ML_AOI/scenario_normal.tif") |>
      terra::crop(valid_zone) |> terra::mask(valid_zone) 
  }else if(predict == "above"){
    pred_stack <- rast("~/shared-data/Data/Maize/geoSpatial/geo_4ML_AOI/scenario_above.tif") |>
      terra::crop(valid_zone) |> terra::mask(valid_zone)  
  }else if(predict == "below"){
    pred_stack <- rast("~/shared-data/Data/Maize/geoSpatial/geo_4ML_AOI/scenario_below.tif") |>
      terra::crop(valid_zone) |> terra::mask(valid_zone)  
  }else{
    print("Enter valid prediction scenario")
  }
  
  res(pred_stack)
  plot(pred_stack[[1]])
  
  #resample the prediction raster from 250m to 1km
  pred_stack <- pred_stack |> terra::aggregate(fact = 4, fun = "mean")
  res(pred_stack)
  
  # ------------------------------------------------------------------------------  
  # Multiple iteration
  # convert the prediction stack to data frame 
  # convert the prediction raster from 250m to 1km and extract the woreda name data
  pred_vect <- terra::as.points(pred_stack)
  woreda <- geodata::gadm(path = tempdir(), country = "ETH", level = 3)
  pts_woreda <- terra::extract(woreda, pred_vect) |> as.data.frame() |>
    dplyr::select(NAME_3)
  pred_df <- as.data.frame(pred_vect)
  pred_df <- pred_df |> cbind(pts_woreda)
  dim(pred_df)
  colnames(pred_df)
  pred_coord <- data.frame(geom(pred_vect)) |> dplyr::select(x, y)
  
  
  # N <- seq(40, 200, 20) #this will be optimized
  # P <- seq(0, 50, 10)
  
  N <- seq(0, 240, 10) 
  P <- seq(0, 50, 5)
  
  
  npk <- expand.grid(N = N, P = P)
  dim(npk)
  colnames(npk)
  colnames(npk) <- c("n_rate2", "p_rate2")
  
  # single core
  final_df <- c()
  for(i in 1:nrow(npk)){
    print(i)
    df <- pred_df |> dplyr::mutate(n_rate2 = npk$n_rate2[i], p_rate2 = npk$p_rate2[i]) |>
      as.h2o()
    pr <- h2o::h2o.predict(uploaded_model, df) |> as.data.frame()
    colnames(pr) <- paste("yield", npk$n_rate2[i], npk$p_rate2[i], sep = ".")
    if(i == 1){
      final_df <- pr
    }else{
      final_df <- cbind(final_df, pr)
    }
  }
  
  head(final_df)
  dim(final_df)
  
  final_df_sp <- cbind(pred_coord, final_df)
  dim(final_df_sp)
  head(final_df_sp)
  #max_col_sp <- cbind(pred_coord, max_col)
  
  #select output location
  pathOut <- paste("~/shared-data/Data", crop, "result/geoSpatial", sep = "/")
  
  if(!dir.exists(pathOut)){
    suppressWarnings(dir.create(pathOut, recursive = T))
  }
  saveRDS(final_df_sp, paste(pathOut, paste0("final_df_275_sp_", predict, ".rds"), sep = "/"))
  ####
}

predictParallel(predict = "normal", crop = "Maize")
predictParallel(predict = "above", crop = "Maize")
predictParallel(predict = "below", crop = "Maize")
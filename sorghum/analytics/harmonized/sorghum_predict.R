# predict the whole Ethiopian area using prediction raster
predictParallel <- function(predict = "above", crop = "Sorghum"){
  
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
  pathOut <- "~/shared-data/Data/Sorghum/Intermediate/Secondrun"
  saved_model <- h2o.loadModel("/home/jovyan//shared-data/Data/Sorghum/Intermediate/Secondrun/GBM_model_R_1716525803741_38")
  my_local_model <- h2o.download_model(saved_model, path = pathOut)
  uploaded_model <- h2o.upload_model(my_local_model)
  
  #read zone data
  eth_zone <- geodata::gadm(path = tempdir(), country = "ETH", level = 2)
  head(eth_zone)
  sort(unique(eth_zone$NAME_2), decreasing = F)
  
  zone_list <- c("Mirab Hararghe", "Misraq Harerge", "North Shewa", "Semen Gondar",
                 "Debub Gondar", "Semen Wello" ,"Debub Wollo", "Oromia", "Wag Himra", 
                 "Derashe", "Konso", "Debub Omo", "Sheka", "Asosa")
  
  valid_zone <- eth_zone |> st_as_sf() |> dplyr::filter(NAME_2 %in% zone_list) |>
    terra::vect()
  
  #read prediction raster
  if(predict == "normal"){
    pred_stack <- rast("~/shared-data/Data/Sorghum/geoSpatial/geo_4ML_AOI/scenario_normal.tif") |>
      terra::crop(valid_zone) |> terra::mask(valid_zone) 
  }else if(predict == "above"){
    pred_stack <- rast("~/shared-data/Data/Sorghum/geoSpatial/geo_4ML_AOI/scenario_above.tif") |>
      terra::crop(valid_zone) |> terra::mask(valid_zone)  
  }else if(predict == "below"){
    pred_stack <- rast("~/shared-data/Data/Sorghum/geoSpatial/geo_4ML_AOI/scenario_below.tif") |>
      terra::crop(valid_zone) |> terra::mask(valid_zone)  
  }else{
    print("Enter valid prediction scenario")
  }
  
  res(pred_stack)
  plot(pred_stack[[7]])
  
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

  N <- seq(0, 150, 10) #this will be optimized
  P <- seq(0, 60, 5)
  
  
  npk <- expand.grid(N = N, P = P)
  dim(npk)
  colnames(npk)
  colnames(npk) <- c("N_rate", "P_rate")
  
  # single core
  final_df <- c()
  for(i in 1:nrow(npk)){
    print(i)
    df <- pred_df |> dplyr::mutate(N_rate = npk$N_rate[i], P_rate = npk$P_rate[i]) |>
      as.h2o()
    print(df)
    pr <- h2o::h2o.predict(uploaded_model, df) |> as.data.frame()
    colnames(pr) <- paste("yield", npk$N_rate[i], npk$P_rate[i], sep = ".")
    if(i == 1){
      final_df <- pr
    }else{
      final_df <- cbind(final_df, pr)
    }
  }
  
  head(final_df)
  dim(final_df)
  
  # multi core
  # n_cores <- parallel::detectCores()/2
  # my_cluster <- parallel::makeCluster(n_cores, type = "PSOCK")
  # doParallel::registerDoParallel(cl = my_cluster)
  # 
  # final_df <- c()
  # foreach( i = 1:nrow(npk), .combine = 'c', .packages = "h2o") %dopar% {
  #  print(i)
  #  df <- pred_df |> dplyr::mutate(n_rate2 = npk$n_rate2[i], p_rate2 = npk$p_rate2[i]) |>
  #    as.h2o()
  #  pr <- h2o::h2o.predict(ML_gbm, df) |> as.data.frame()
  #  colnames(pr) <- paste("yield", npk$n_rate2[i], npk$p_rate2[i], sep = ".")
  #  if(i == 1){
  #    final_df <- pr
  #  }else{
  #    final_df <- cbind(final_df, pr)
  #  }
  # }
  # 
  # select the max value from the columns
  
  #max_col <- as.data.frame(colnames(final_df)[apply(final_df, 1, which.max)])
  
  final_df_sp <- cbind(pred_coord, final_df)
  dim(final_df_sp)
  head(final_df_sp)
  #max_col_sp <- cbind(pred_coord, max_col)
  
  #select output location
  # pathOut <- paste("~/shared-data/Data", crop, "result/geoSpatial", sep = "/")
  
  if(!dir.exists(pathOut)){
    suppressWarnings(dir.create(pathOut, recursive = T))
  }
  saveRDS(final_df_sp, paste(pathOut, paste0("final_df_208_sp_", predict, ".rds"), sep = "/"))
  ####
}



predictParallel(predict = "normal", crop = "Sorghum")
predictParallel(predict = "below", crop = "Sorghum")
predictParallel(predict = "above", crop = "Sorghum")



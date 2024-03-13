#################################################################################################################
## sourcing required packages 
#################################################################################################################
# "geosphere",
packages_required <- c("terra", "sf", "rgl", "sp", "geodata", "tidyverse",  "countrycode", "lubridate", "plyr", "parallel", "foreach")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

#####################################################################################################################################
#' @description a function to be used to define path, input data (GPS files), geo-spatial layers
#'
#' @param inputData is the data containing c("NAME_1","NAME_2", "lon","lat", "planting_month_date", "harvest_month_date")
#' @param varsbasePath is the path to access the global geo-spatial layers (CHIRPS, ISRIC, .. layers)
#' @param soilProfile TRUE if the data from soil Grids the 6 profiles are needed, FALSE if only the top 30 cm are needed. 
#' @param pathOut the path to store the extracted result geo-spatial data 
#' @author Meklit C.
#'
#' @return

#' @examples (inputData = readRDS("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ethiopia_harmonizeDST/Maize/raw/Maize_AOI.RDS"), 
#'            varsbasePath = "~/shared-data/Data/General/Global_GeoData/Landing",pathOut = "")
Paths_Vars <- function(inputData, varsbasePath, soilProfile =TRUE, pathOut=NULL){
  
  readLayers_soil_isric <- NULL
  shapefileHC <- NULL
  
  
  
  listRasterRF <-list.files(path=paste0(varsbasePath, "Rainfall/chirps"), pattern=".nc$", full.names = TRUE)[-c(1:2)]
  listRasterTmax <-list.files(path=paste0(varsbasePath, "TemperatureMax/AgEra"), pattern=".nc$", full.names = TRUE)
  listRasterTMin <-list.files(path=paste0(varsbasePath, "TemperatureMin/AgEra"), pattern=".nc$", full.names = TRUE)
  listRasterRH <-list.files(path=paste0(varsbasePath, "RelativeHumidity/AgEra"), pattern=".nc$", full.names = TRUE)
  listRasterSR <-list.files(path=paste0(varsbasePath, "SolarRadiation/AgEra"), pattern=".nc$", full.names = TRUE)
  listRasterWS <-list.files(path=paste0(varsbasePath, "WindSpeed/AgEra"), pattern=".nc$", full.names = TRUE)
  
  if(soilProfile == TRUE){
    listRaster_soil <-list.files(path=paste0(varsbasePath, "Soil/soilGrids/profile"), pattern=".tif$")
    readLayers_soil <- terra::rast(paste(paste0(varsbasePath, "Soil/soilGrids/profile"), listRaster_soil, sep="/"))
    shapefileHC <- st_read(paste0(varsbasePath, "Soil/HC27/HC27 CLASSES.shp"), quiet= TRUE)%>%
      st_make_valid()
  }else{
    listRaster_soil <-list.files(path=paste0(varsbasePath, "Soil/iSDA"), pattern=".tif$")
    readLayers_soil <- terra::rast(paste(paste0(varsbasePath, "Soil/iSDA"), listRaster_soil, sep="/"))
    listRaster_soil_isric <-list.files(path=paste0(varsbasePath, "Soil/soilGrids"), pattern=".tif$")
    readLayers_soil_isric <- terra::rast(paste(paste0(varsbasePath, "Soil/soilGrids"), listRaster_soil_isric, sep="/"))
  }
  return(list(inputData, listRasterRF, listRasterTmax, listRasterTMin, listRasterRH,listRasterSR, listRasterWS, readLayers_soil, readLayers_soil_isric, shapefileHC, pathOut))
}




#################################################################################################################
# DATA SOURCE https://data.chc.ucsb.edu/products/CHIRPS-2.0/ for rainfall
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-agrometeorological-indicators?tab=form fr AgEra 5 data
# Is a helper function for extract_geoSpatialPointData. Extract geo-spatial data with time dimension 
#' @description this functions loops through all .nc files (~30 - 40 years) for rain. temperature, solar radiation, wind speed and relative humidity. 
#' Planting_month_date should be set to one month prior to the earliest possible planting month and date so that data is available to-set initial conditions while running crop model. 
#' 
#' @param country country name to be used to extract the first two level of administrative units to attach to the data. 
#' @param inputData is a data frame and must have the c(lat, lon, plantingDate, harvestDate). For field observations, plantingDate  harvestDate should be given in yyyy-mm-dd format. 
#' @param AOI TRUE if data for multiple years is required. FALSE if data is required for field trials, for which the actual interval between the planting and harvest dates will be used. 
#' @param Planting_month_date if AOI is TRUE, Planting_month_date should be provided in mm-dd format. weather data across years between Planting_month_date and Harvest_month_date will be provided. 
#' @param Harvest_month_date if AOI is TRUE, Harvest_month_date should be provided in mm-dd format.  weather data across years between Planting_month_date and Harvest_month_date will be provided
#' @param varName is the name of the variable for which data is required and it is one of c("Rainfall", "temperatureMax", "temperatureMin", "relativeHumidity", "solarRadiation", "windSpeed")
#' @param plantingWindow number of weeks starting considering the Planting_month_date as earliest planting week. It is given when several planting dates are to be tested to determine optimal planting date and it should be given in  
#' @param jobs defines how many cores to use for parallel data sourcing
#' 
#' @return based on the provided variable name, this function returns, daily data for every GPS together with longitude, latitude, planting Date, harvest Date, NAME_1, NAME_2 and 
#' daily data with columns labelled with the concatenation of variable name and Julian day of data.  When AOI is set to FALSE, every GPS location is allowed to have 
#' its own unique planting and harvest dates and in this case, because the different GPS location can have non-overlapping dates, NA values are filled for dates prior to
#' planting and later than harvest dates. When AOI is true, the user defined Planting_month_date and Harvest_month_date is considered for all locations and data is provided across the years. 
#' The weather data is extracted 1 months before the actual planting month 
#' @examples: inputData <- data.frame(lon=c(29.3679, 29.3941,  29.390), lat=c(-1.539, -1.716, -1.716), 
#' plantingDate  = c("2020-08-27", "2020-09-04", "2020-09-04"),
#' harvestDate = c("2020-12-29", "2020-12-29", "2020-12-29"))
#' get_weather_pointData(inputData = inputData, country = "Rwanda", AOI=FALSE, 
#'                     Planting_month_date=NULL, Harvest_month_date=NULL, varName="temperatureMin", jobs=10)
#'                
get_weather_pointData <- function(country, inputData,  AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, varName, listRaster, plantingWindow=1, jobs){
  if(AOI == TRUE){
    if(is.null(Planting_month_date) | is.null(Harvest_month_date)){
      print("with AOI=TRUE, Planting_month_date, Harvest_month_date can not be null, please refer to the documentation and provide mm-dd for both parameters")
      return(NULL)
    }
    
    ## check if both planting and harvest dates are in the same year
    Planting_month <- as.numeric(str_extract(Planting_month_date, "[^-]+"))
    Harvest_month <- as.numeric(str_extract(Harvest_month_date, "[^-]+"))
    
    ## py and hy are used only as place holder for formatting purposes
    if(Planting_month < Harvest_month){
      planting_harvest_sameYear <- TRUE
      py <- 2000
      hy <- 2000
    }else{
      planting_harvest_sameYear <- FALSE
      py <- 2000
      hy <- 2001
    }
    
    ## set planting date one moth prior to the given Planting_month_date so that initial condition for the crop model could be set correctly
    Planting_month_date <- as.Date(paste0(py, "-",Planting_month_date)) ## the year is only a place holder to set planting month 1 month earlier
    Planting_month_date <- Planting_month_date %m-% months(1)
    
    ## set harvest date one month later to the make sure there is enough weather data until maturity 
    Harvest_month_date <- as.Date(paste0(hy, "-",Harvest_month_date)) ## the year is only a place holder 
    
    ## if multiple planting dates are to be tested, adjust the Harvest_month_date to extract weather data for the later planting dates.  
    if(plantingWindow > 1 & plantingWindow < 5){
      Harvest_month_date <- Harvest_month_date %m+% months(1)
    }else if(plantingWindow >= 5 & plantingWindow <=8){
      Harvest_month_date <- Harvest_month_date %m+% months(2)
    }
  }
  
  ## 1. read all the raster files 
  
  # if(AOI == TRUE & varName == "Rainfall"){
  #   listRaster <- listRaster[20:42]
  # }else if (AOI == TRUE){
  #   listRaster <- listRaster[22:44]
  # }
  # 
  
  
  
  ## 2. format the input data with GPS, dates and ID and add administrative unit info
  if(AOI == TRUE){
    countryCoord <- unique(inputData[, c("lon", "lat")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    ## After checking if planting and harvest happens in the same year, get the date of the year 
    countryCoord$startingDate <- Planting_month_date
    countryCoord$endDate <- Harvest_month_date
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord) <- c("longitude", "latitude", "startingDate", "endDate")
    countryCoord$ID <- c(1:nrow(countryCoord))
    ground <- countryCoord[, c("longitude", "latitude", "startingDate", "endDate", "ID")]
    
  }else{
    inputData <- unique(inputData[, c("lon", "lat","ID", "plantingDate", "harvestDate")])
    inputData$plantingDate <- as.Date(inputData$plantingDate)
    inputData$harvestDate <- as.Date(inputData$harvestDate)
    inputData$plantingDate <- inputData$plantingDate %m-% months(1)
    inputData <- inputData[complete.cases(inputData), ]
    names(inputData) <- c("longitude", "latitude", "ID","startingDate", "endDate")
    ground <- inputData
  }
  
  # ground$harvestDate <- as.Date(ground$harvestDate, "%Y-%m-%d")
  countryShp <- geodata::gadm(country, level = 2, path='.')
  dd2 <- raster::extract(countryShp, ground[, c("longitude", "latitude")])[, c("NAME_1", "NAME_2")]
  ground$NAME_1 <- dd2$NAME_1
  ground$NAME_2 <- dd2$NAME_2
  
  ## 3.get the seasonal rainfall parameters for AOI
  
  if(AOI == TRUE){
    if (planting_harvest_sameYear ==  TRUE) {
      
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      
      rf_result <- foreach(i=1:length(listRaster), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        rasti <- listRaster[i]
        pl_j <-as.POSIXlt(unique(ground$startingDate))$yday
        hv_j <-as.POSIXlt(unique(ground$endDate))$yday
        PlHvD <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
        xy <- ground[, c("longitude", "latitude")]
        raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        raini <- raini[,-1]
        if(varName %in% c("temperatureMax","temperatureMin")){
          raini <- raini-274
        }else if (varName == "solarRadiation"){
          raini <- raini/1000000
        }
        ground_adj <- ground
        lubridate::year(ground_adj$startingDate) <- as.numeric(str_extract(rasti, "[[:digit:]]+"))
        lubridate::year(ground_adj$endDate) <- as.numeric(str_extract(rasti, "[[:digit:]]+"))
        start <- as.Date(unique(ground_adj$startingDate))
        maxDaysDiff <- abs(max(min(pl_j) - max(hv_j)))
        end <- start + as.difftime(maxDaysDiff, units="days")
        ddates <- seq(from=start, to=end, by=1)
        names(raini) <- paste(varName, ddates[1:length(names(raini))], sep="_")
        # names(raini) <- paste(varName, sub("^[^_]+", "", names(raini)), sep="")
        ground_adj$startingDate <- as.character(ground_adj$startingDate)
        ground_adj$endDate <- as.character(ground_adj$endDate)
        ground2 <- cbind(ground_adj, raini)
      }
      
      data_points <- dplyr::bind_rows(rf_result)
      stopCluster(cls)
    }else{
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      ## Rainfall
      rf_result2 <- foreach(i = 1:(length(listRaster)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        listRaster <- listRaster[order(listRaster)]
        rast1 <- listRaster[i]
        rast2 <- listRaster[i+1]
        ground_adj <- ground
        lubridate::year(ground_adj$startingDate) <- as.numeric(str_extract(rast1, "[[:digit:]]+"))
        lubridate::year(ground_adj$endDate) <- as.numeric(str_extract(rast2, "[[:digit:]]+"))
        start <- as.Date(unique(ground_adj$startingDate))
        maxDaysDiff <- as.numeric(max(ground_adj$endDate) - min(ground_adj$startingDate))
        end <- start + as.difftime(maxDaysDiff, units="days")
        ddates <- seq(from=start, to=end, by=1)
        # Convert planting Date and harvesting in Julian Day 
        pl_j <-as.POSIXlt(unique(ground_adj$startingDate))$yday
        hv_j <-as.POSIXlt(unique(ground_adj$endDate))$yday
        rasti1 <- terra::rast(rast1, lyrs=c(pl_j:terra::nlyr(terra::rast(rast1))))
        rasti2 <- terra::rast(rast2, lyrs=c(1:hv_j))
        PlHvD <- c(rasti1, rasti2)
        xy <- ground[, c("longitude", "latitude")]
        raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        raini <- raini[,-1]
        if(varName %in% c("temperatureMax","temperatureMin")){
          raini <- raini-274
        }else if (varName == "solarRadiation"){
          raini <- raini/1000000
        }
        names(raini) <- paste(varName, ddates, sep="_")
        ground_adj$startingDate <- as.character(ground_adj$startingDate)
        ground_adj$endDate <- as.character(ground_adj$endDate)
        ground2 <- cbind(ground_adj, raini)
      }
      
      data_points <- dplyr::bind_rows(rf_result2)
      stopCluster(cls)
    }
    
  } else {
    
    
    # Get the Year
    ground$yearPi <- as.numeric(format(as.POSIXlt(ground$startingDate), "%Y"))
    ground$yearHi <- as.numeric(format(as.POSIXlt(ground$endDate), "%Y"))
    
    ## drop data with planting dates before 1981, as there is no global layer available for years before 1981 pfr rain and 1979 for AgeEra files
    ground <- droplevels(ground[ground$yearPi >= 1981 & ground$yearHi >= 1981, ])
    
    # Convert planting date and harvesting date in Julian Day
    ground$ pl_j <-as.POSIXlt(ground$startingDate)$yday
    ground$hv_j <-as.POSIXlt(ground$endDate)$yday
    
    # get the max number of days on the field to be used as column names. 
    start <- as.Date(min(ground$startingDate))
    maxDaysDiff <- abs(max(min(ground$pl_j) - max(ground$hv_j)))
    end <- maxDaysDiff +  as.Date(max(ground$endDate)) # start + as.difftime(maxDaysDiff, units="days")
    ddates <- seq(from=start, to=end, by=1)
    
    # create list of all possible column names to be able to row bind data from different sites with different planting and harvest dates ranges
    # rf_names <- c(paste0(varName, "_",  c(min(ground$pl_j):max(ground$hv_j))))
    rf_names <- c(paste0(varName, "_",  ddates))
    rf_names2 <-  as.data.frame(matrix(nrow=length(rf_names), ncol=1))
    colnames(rf_names2) <- "dataDate"
    rf_names2[,1] <- rf_names
    rf_names2$ID <- c(1:nrow(rf_names2))
    
    
    data_points <- NULL
    for(i in 1:nrow(ground)){
      print(i)
      groundi <- ground[i, c("longitude", "latitude", "ID", "startingDate", "endDate", "ID", "NAME_1", "NAME_2","yearPi", "yearHi", "pl_j", "hv_j")]
      yearPi <- as.numeric(groundi$yearPi)
      yearHi <- as.numeric(groundi$yearHi)
      pl_j <- groundi$pl_j
      hv_j <- groundi$hv_j
      
      # Case planting and harvesting dates span the same year
      if (yearPi == yearHi) {
        rasti<-listRaster[which(grepl(yearPi, listRaster, fixed=TRUE) == T)]
        rasti <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
      }
      
      # Case planting and harvesting dates span two different years
      if (yearPi < yearHi) {
        rasti1 <- listRaster[which(grepl(yearPi, listRaster, fixed=TRUE) == T)]
        rasti1 <- terra::rast(rasti1, lyrs=c(pl_j:terra::nlyr(terra::rast(rasti1))))
        rasti2 <-listRaster[which(grepl(yearHi, listRaster, fixed=TRUE) == T)]
        rasti2 <- terra::rast(rasti2, lyrs=c(1:hv_j))
        rasti <- c(rasti1, rasti2)
        
      }
      
      ### Extract the information for the i-th row 
      
      xy <- groundi[, c("longitude", "latitude")]
      xy <- xy %>%
        mutate_if(is.character, as.numeric)
      
      raini <- terra::extract(rasti, xy,method='simple', cells=FALSE)
      raini <- raini[,-1]
      if(varName %in% c("temperatureMax","temperatureMin")){
        raini <- raini-274
      }else if (varName == "solarRadiation"){
        raini <- raini/1000000
      }
      
      start <- as.Date(unique(groundi$startingDate))
      maxDaysDiff <- as.numeric(groundi$endDate - groundi$startingDate)
      end <- start + as.difftime(maxDaysDiff, units="days")
      ddates <- seq(from=start, to=end, by=1)
      names(raini) <- paste(varName, ddates, sep="_")
      raini <- as.data.frame(t(raini))
      raini$dataDate <- rownames(raini)
      rownames(raini) <- NULL
      
      ## merging data for different trials with differing growing period requires having data for the whole period of time
      raini <- merge(raini, rf_names2, by="dataDate", all.y=TRUE)
      raini <- raini[order(raini$ID),]
      rownames(raini) <- raini$dataDate
      raini <- raini %>% dplyr::select(-c(ID,dataDate))
      raini2 <- as.data.frame(t(raini))
      rownames(raini2) <- NULL
      raini2 <- cbind(groundi, raini2)
      data_points <- rbind(data_points, raini2)
    }
  }
  
  data_points <- data_points %>% 
    select_if(~sum(!is.na(.)) > 0)
  return(data_points)
}



################################################################################
# https://rdrr.io/cran/geodata/man/soil_grids.html
# https://rdrr.io/cran/geodata/man/soil_af.html
# https://rdrr.io/cran/geodata/man/soil_af_isda.html
# https://rdrr.io/cran/geodata/man/elevation.html DEm data from SRTM
#' @description Is a helper function for extract_geoSpatialPointData. Extract geo-spatial data with no temporal dimension, i,e,. soil properties and topography variables
#' 
#' @param country country name to be sued to extract the first two level of administrative units to attach to the data. 
#' @param inputData is a data frame and must have the c(lat, lon). The data must have ID 
#' @param soilProfile is true/false, if true data, isirc data for the six soil profiles will be processed. This is required for DSSAT and other crop models. 
#' @param pathOut the path to store the extracted result geo-spatial data 
#' @param varsbasePath is the path to access the global geo-spatial layers (CHIRPS, ISRIC, .. layers)
#' 
#' @return a data frame with lon, lat,teh top two admistrnative zones, soil properties with columns named with variable names attached with depth,  
#' elevations variables attached for every GPS location 
#' @examples: get_soil_DEM_pointData(country = "Rwanda", soilProfile = FALSE, pathOut = getwd(),
#' inputData = data.frame(lon=c(29.35667, 29.36788), lat=c(-1.534350, -1.538792)))
get_soil_DEM_pointData <- function(country, inputData, varsbasePath, soilProfile = FALSE, pathOut, Layers_soil = Layers_soil, 
                                   Layers_soil_isric= Layers_soil_isric, shapefileHC = shapefileHC, AOI){
  
  
  ## 2. read the shape file of the country and crop the global data
  countryShp <- geodata::gadm(country, level = 2, path='.')
  inputData$country = country
  
  dd2 <- raster::extract(countryShp, inputData[, c("lon", "lat")])[, c("NAME_1", "NAME_2")]
  inputData$NAME_1 <- dd2$NAME_1
  inputData$NAME_2 <- dd2$NAME_2
  
  if(AOI == TRUE){
    inputData$ID <- c(1:nrow(inputData))
  }
  
  inputData2 <- unique(inputData[, c("lon", "lat", "ID","NAME_1", "NAME_2", "country")])
  inputData2 <- inputData2[complete.cases(inputData2), ]
  gpsPoints <- unique(inputData2[, c("lon", "lat")])
  gpsPoints$lon <- as.numeric(gpsPoints$lon)
  gpsPoints$lat <- as.numeric(gpsPoints$lat)
  # gpsPoints <- gpsPoints[, c("x", "y")]
  areasCovered <- unique(c(raster::extract(countryShp, gpsPoints)$NAME_2))
  areasCovered <- areasCovered[!is.na(areasCovered)]
  print(areasCovered)
  
  
  for(aC in areasCovered){
    print(aC)
    countryShpA <- countryShp[countryShp$NAME_2 == aC]
    croppedLayer_soil <- terra::crop(Layers_soil, countryShpA)
    ## 3. apply pedo-transfer functions to get soil organic matter and soil hydraulics variables 
    if (soilProfile == TRUE){
      
      depths <- c("0-5cm","5-15cm","15-30cm","30-60cm","60-100cm","100-200cm")  
      ## get soil organic matter as a function of organic carbon
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("SOM_",depths[i])]] <- (croppedLayer_soil[[paste0("soc_",depths[i])]] * 2)/10
      }
      
      
      ##### permanent wilting point (cm3/cm3) ####
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (-0.024 * croppedLayer_soil[[paste0("sand_",depths[i])]]/100) + 0.487 *
          croppedLayer_soil[[paste0("clay_",depths[i])]]/100 + 0.006 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
          0.005*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
          0.013*(croppedLayer_soil[[paste0("clay_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) +
          0.068*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay_",depths[i])]]/100 ) + 0.031
        croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (croppedLayer_soil[[paste0("PWP_",depths[i])]] + 
                                                            (0.14 * croppedLayer_soil[[paste0("PWP_",depths[i])]] - 0.02))
      }
      
      ##### FC (cm3/cm3) ######
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("FC_",depths[i])]] <- -0.251 * croppedLayer_soil[[paste0("sand_",depths[i])]]/100 + 0.195 * 
          croppedLayer_soil[[paste0("clay_",depths[i])]]/100 + 0.011 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
          0.006*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
          0.027*(croppedLayer_soil[[paste0("clay_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) + 
          0.452*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay_",depths[i])]]/100) + 0.299
        croppedLayer_soil[[paste0("FC_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]] + (1.283 * croppedLayer_soil[[paste0("FC_",depths[i])]]^2 - 0.374 * croppedLayer_soil[[paste0("FC_",depths[i])]] - 0.015))
        
      }
      
      
      ##### soil water at saturation (cm3/cm3) ######
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- 0.278*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100)+0.034*
          (croppedLayer_soil[[paste0("clay_",depths[i])]]/100)+0.022*croppedLayer_soil[[paste0("SOM_",depths[i])]] -
          0.018*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])- 0.027*
          (croppedLayer_soil[[paste0("clay_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])-
          0.584 * (croppedLayer_soil[[paste0("sand_",depths[i])]]/100*croppedLayer_soil[[paste0("clay_",depths[i])]]/100)+0.078
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("SWS_",depths[i])]] +(0.636*croppedLayer_soil[[paste0("SWS_",depths[i])]]-0.107))
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]]+croppedLayer_soil[[paste0("SWS_",depths[i])]]-(0.097*croppedLayer_soil[[paste0("sand_",depths[i])]]/100)+0.043)
        
      }
      
      ##### saturated conductivity (mm/h) ######
      for(i in 1:length(depths)) {
        b = (log(1500)-log(33))/(log(croppedLayer_soil[[paste0("FC_",depths[i])]])-log(croppedLayer_soil[[paste0("PWP_",depths[i])]]))
        lambda <- 1/b
        croppedLayer_soil[[paste0("KS_",depths[i])]] <- 1930*((croppedLayer_soil[[paste0("SWS_",depths[i])]]-croppedLayer_soil[[paste0("FC_",depths[i])]])^(3-lambda))
      }
      
      soilsData <- croppedLayer_soil
      
    }else{
      
      depths <- c("0-20cm","20-50cm")  
      
      ## get soil organic matter as a function of organic carbon
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("SOM_",depths[i])]] <- (croppedLayer_soil[[paste0("oc_",depths[i])]] * 2)/10
      }
      
      ##### permanent wilting point (cm3/cm3) ####
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (-0.024 * croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100) + 0.487 *
          croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 + 0.006 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
          0.005*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
          0.013*(croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) +
          0.068*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 ) + 0.031
        croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (croppedLayer_soil[[paste0("PWP_",depths[i])]] + (0.14 * croppedLayer_soil[[paste0("PWP_",depths[i])]] - 0.02))
      }
      
      
      
      ##### FC (cm3/cm3) ######
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("FC_",depths[i])]] <- -0.251 * croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 + 0.195 * 
          croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 + 0.011 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
          0.006*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
          0.027*(croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) + 
          0.452*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100) + 0.299
        croppedLayer_soil[[paste0("FC_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]] + (1.283 * croppedLayer_soil[[paste0("FC_",depths[i])]]^2 - 0.374 * croppedLayer_soil[[paste0("FC_",depths[i])]] - 0.015))
        
      }
      
      
      ##### soil water at saturation (cm3/cm3) ######
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- 0.278*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100)+0.034*
          (croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100)+0.022*croppedLayer_soil[[paste0("SOM_",depths[i])]] -
          0.018*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])- 0.027*
          (croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])-
          0.584 * (croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100*croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100)+0.078
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("SWS_",depths[i])]] +(0.636*croppedLayer_soil[[paste0("SWS_",depths[i])]]-0.107))
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]]+croppedLayer_soil[[paste0("SWS_",depths[i])]]-(0.097*croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100)+0.043)
        
      }
      
      ##### saturated conductivity (mm/h) ######
      for(i in 1:length(depths)) {
        b = (log(1500)-log(33))/(log(croppedLayer_soil[[paste0("FC_",depths[i])]])-log(croppedLayer_soil[[paste0("PWP_",depths[i])]]))
        lambda <- 1/b
        croppedLayer_soil[[paste0("KS_",depths[i])]] <- 1930*((croppedLayer_soil[[paste0("SWS_",depths[i])]]-croppedLayer_soil[[paste0("FC_",depths[i])]])^(3-lambda))
      }
      
      names(croppedLayer_soil) <- gsub("0-20cm", "top", names(croppedLayer_soil))
      names(croppedLayer_soil) <- gsub("20-50cm", "bottom", names(croppedLayer_soil))
      names(croppedLayer_soil) <- gsub("_0-200cm", "", names(croppedLayer_soil))
      names(croppedLayer_soil) <- gsub("\\.", "_",  names(croppedLayer_soil)) 
      croppedLayer_isric <- terra::crop(Layers_soil_isric, countryShpA)
      names(croppedLayer_isric) <- gsub("0-30cm", "0_30", names(croppedLayer_isric))
      soilsData <- c(croppedLayer_soil, croppedLayer_isric)
    }
    if(aC == areasCovered[1]){
      soilData_allregion <- soilsData
    }else{
      soilData_allregion <- merge(soilData_allregion, soilsData)
    }
  }
  
  
  ## 4. Extract point soil data 
  pointDataSoil <- as.data.frame(raster::extract(soilData_allregion, gpsPoints))
  pointDataSoil <- pointDataSoil %>% 
    dplyr::select(-c(ID)) %>% 
    dplyr::mutate(lon=gpsPoints$lon, lat=gpsPoints$lat) 
  pointDataSoil <- merge(unique(inputData2[, c("country", "NAME_1", "NAME_2", "ID","lon", "lat")]), pointDataSoil, by=c("lon","lat"))
  
  ## 5. Extract DEM data: at lon and lat at steps of 5 degree
  listRaster_dem <- terra::rast(paste0(varsbasePath,"ETH_dem.tif"))
  dem <- terra::crop(listRaster_dem, countryShp[countryShp$NAME_2 %in% unique(inputData2$NAME_2)])
  slope <- terra::terrain(dem, v = 'slope', unit = 'degrees')
  tpi <- terra::terrain(dem, v = 'TPI')
  tri <- terra::terrain(dem, v = 'TRI')
  
  ### ideally these four dem layers will be made to a list so that point extraction will be done at once, bu CG Labs capacity does not allow that ...
  topoLayer <- terra::rast(list(dem, slope, tpi, tri))
  datatopo <- terra::extract(topoLayer, gpsPoints, method='simple', cells=FALSE)
  datatopo <- datatopo %>% dplyr::select(-c(ID)) %>% 
    dplyr::mutate(lon=gpsPoints$lon, lat=gpsPoints$lat) 
  names(datatopo) <- c("altitude", "slope", "TPI", "TRI","lon", "lat")
  
  pointDataSoil <- unique(merge(pointDataSoil, datatopo, by=c("lon", "lat")))
  
  
  
  ## 6. Extract harvest choice soil class and drainage rate (just for profile =TRUE)
  if(soilProfile == TRUE){
    coordinates_df <- data.frame(lat=pointDataSoil$lat, lon=pointDataSoil$lon)
    coordinates_sf <- st_as_sf(coordinates_df, coords = c("lon", "lat"), crs = 4326)
    intersecting_polygons <-st_join(coordinates_sf, shapefileHC)
    # Extract the geometry (latitude and longitude) from the 'joined_data' object
    intersecting_polygons  <-intersecting_polygons  %>%
      mutate(lon = st_coordinates(intersecting_polygons)[, "X"], 
             lat = st_coordinates(intersecting_polygons)[, "Y"]) 
    intersecting_polygons  <-as.data.frame(intersecting_polygons)
    intersecting_polygons$geometry <- NULL
    intersecting_polygons$ID <- NULL
    
    
    # Join the LDR (drainage rate) values to the intersecting_polygons data
    LDR_data <- data.frame(LDR = c(rep(0.2, 9), rep(0.5, 9), rep(0.75, 9)),
                           GRIDCODE = seq(1:27))
    
    LDR_data <- merge(intersecting_polygons,LDR_data)
    LDR_data$GRIDCODE <- NULL
    pointDataSoil <- unique(merge(pointDataSoil, LDR_data, by=c("lon", "lat")))
  }
  
  
  return(pointDataSoil)
}

################################################################################
#' Title Extract soil, DEM and daily weather data
#' This function reads the input data for GPS and dates for specific country-use Case-crop combination, in data-curation result folder and should be saved 
#' for the trial sites named as "compiled_fieldData.RDS" and for target areas as AOI_GPS.RDS. The input data should have lon, lat, ID, planting and harvest dates
#'
#' @param country country name to be used for cropping, extracting the top two administrative region names and to define input and output paths
#' @param varsbasePath is the path to access the global geo-spatial layers (CHIRPS, ISRIC, .. layers)
#' @param AOI is TRUE is the input data has defined planting and harvest dates otherwise FALSE
#' @param pathOut the path to store the extracted result geo-spatial data 
#' @param Planting_month_date planting month and date in mm-dd format and must be provided if AOI is TRUE. It is the earliest possible planting date in the target area. 
#' @param Harvest_month_date harvest month and date in mm-dd format and must be provided if AOI is TRUE 
#' @param plantingWindow is given when several planting dates are to be tested to determine optimal planting date and it should be given in number of weeks starting from Planting_month_date 
#' @param weatherData is TRUE is weather data is required otherwise FALSE
#' @param soilData is TRUE if soil data is required otherwise FALSE
#' @param soilProfile is TRUE if soil data from the six profile of ISRIC is required, otherwise set to FALSE
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name
#' @param jobs number of cores used to parallel weather data extraction
#'
#' @return If weatherData is TRUE, list of data frames with daily data for c("Rainfall", "temperatureMax", "temperatureMin", "relativeHumidity", "solarRadiation", "windSpeed") is returned. 
#' If soilData is set TRUE, soil properties at different depth plus elevation and derivatives of DEM are returned. These results are written out in paths defined by country, useCaseName, and crop 
#' and either raw or result of the different AgWise modules space in CG Labs. If AOI is set TRUE, the weather data between the Planting_month_date and Harvest_month_date and for 1979 - 2022 data will be returned. 

#' @examples extract_geoSpatialPointData(country = "Rwanda", AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, 
#' soilData = TRUE, weatherData = TRUE,soilProfile = FALSE, jobs =10)
extract_geoSpatialPointData <- function(country=country, inputData = NULL,varsbasePath,
                                        AOI=FALSE,Planting_month_date=NULL, Harvest_month_date=NULL, plantingWindow=1, 
                                        weatherData = TRUE, soilData = TRUE, soilProfile = FALSE, season = 1, pathOut = NULL, jobs=10){
  
  ARD <- Paths_Vars(inputData = inputData, varsbasePath=varsbasePath, soilProfile =soilProfile, pathOut = pathOut)
  
  inputData <- ARD[[1]]
  listRasterRF <- ARD[[2]]
  listRasterTmax <- ARD[[3]]
  listRasterTMin <- ARD[[4]]
  listRasterRH <- ARD[[5]]
  listRasterSR <- ARD[[6]]
  listRasterWS <- ARD[[7]]
  Layers_soil <- ARD[[8]]
  Layers_soil_isric <- ARD[[9]]
  shapefileHC <- ARD[[10]]
  pathOut <- ARD[[11]]
  
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  if(weatherData == TRUE){
    i=1
    wData <- list()
    for(varName in c("Rainfall", "temperatureMax", "temperatureMin", "relativeHumidity", "solarRadiation", "windSpeed")){
      
      if(varName == "Rainfall"){
        listRaster <- listRasterRF
      }else if (varName == "temperatureMax"){
        listRaster <- listRasterTmax
      }else if (varName == "temperatureMin"){
        listRaster <- listRasterTMin
      }else if(varName == "relativeHumidity"){
        listRaster <- listRasterRH
      }else if(varName == "solarRadiation"){
        listRaster <- listRasterSR
      }else if(varName == "windSpeed"){
        listRaster <- listRasterWS
      }
      
      if(AOI == TRUE){
        listRaster <- listRaster[grep("2000", listRaster):grep("2022", listRaster)] 
      }
      
      
      vData <- get_weather_pointData(inputData = inputData, 
                                     country = country, AOI=AOI, Planting_month_date=Planting_month_date, plantingWindow, 
                                     Harvest_month_date=Harvest_month_date, varName=varName, listRaster= listRaster, jobs=jobs)
      
      w_name <- ifelse(AOI == TRUE, paste(varName, "_Season_", season, "_PointData_AOI.RDS", sep=""), paste(varName, "_PointData_trial.RDS", sep=""))
      saveRDS(vData, paste(pathOut, w_name, sep="/"))
      print(paste("Data sourcing for ", varName, " is done", sep=""))
      rm(vData)
      i=i+1
    }
  }
  
  
  if(soilData == TRUE & season == 1){
    sData <- get_soil_DEM_pointData(country = country, inputData = inputData, varsbasePath=varsbasePath,
                                    soilProfile = soilProfile, pathOut=pathOut,AOI = AOI,
                                    Layers_soil = Layers_soil, Layers_soil_isric=Layers_soil_isric, shapefileHC = shapefileHC)
    
    if(AOI == TRUE){
      if (soilProfile == TRUE){
        s_name <- "SoilDEM_PointData_AOI_profile.RDS"
      }else{
        s_name <- "SoilDEM_PointData_AOI.RDS"
      }
    }else{
      if (soilProfile == TRUE){
        s_name <- "SoilDEM_PointData_trial_profile.RDS"
      }else{
        s_name <- "SoilDEM_PointData_trial.RDS"
      }
    }
    saveRDS(sData, paste(pathOut, s_name, sep="/"))
  }
  
}




#################################################################################################################


# 3. Is a helper function for get_WeatherSummarydata to get seasonal rainfall parameters for point data over the cropping season  -------------------------------------------
#' @description is a function to get total rainfall, number of rainy days and monthly rainfall, and working when the planting and harvest happen in different years
#' @param raster1 the .nc file for the planting year, within get_rf_pointdata function, this is provided by the function 
#' @param raster2 the .nc file for the harvest year, within get_rf_pointdata function, this is provided by the function 
#' @param gpsdata a data frame with longitude and latitude 
#' @param pl_j the planting date as the date of the year
#' @param hv_j the harvest date as the date of the year
#'
#' @return  a data frame with total rainfall, number of rainy days and monthly rainfall
#' @example summary_pointdata_rainfall(rastLayer1="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/1981.nc",
# raster2="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/1982.nc",
# gpsdata=data.frame(longitude = c(29.375, 30.125), latitude = c(-2.825, -2.425)),  
# pl_j=35, hv_j=150, planting_harvest_sameYear = TRUE)
summarize_pointdata <- function(rastLayerRF_1=NULL, rastLayerRF_2=NULL, 
                                rastLayerTmax_1=NULL, rastLayerTmax_2=NULL,
                                rastLayerTmin_1=NULL, rastLayerTmin_2=NULL,
                                rastLayerRH_1=NULL, rastLayerRH_2=NULL,
                                rastLayerSR_1=NULL, rastLayerSR_2=NULL,
                                rastLayerWS_1=NULL, rastLayerWS_2=NULL,
                                gpsdata, pl_j, hv_j, planting_harvest_sameYear){
  
  # 3.1. Read the rainfall data and shape the ground data ####
  if(planting_harvest_sameYear == TRUE){
    PlHvD_RF <- terra::rast(rastLayerRF_1, lyrs=c(pl_j:hv_j)) 
    PlHvD_Tmax <- terra::rast(rastLayerTmax_1, lyrs=c(pl_j:hv_j)) 
    PlHvD_Tmin <- terra::rast(rastLayerTmin_1, lyrs=c(pl_j:hv_j)) 
    PlHvD_RH <- terra::rast(rastLayerRH_1, lyrs=c(pl_j:hv_j)) 
    PlHvD_SR <- terra::rast(rastLayerSR_1, lyrs=c(pl_j:hv_j)) 
    PlHvD_WS <- terra::rast(rastLayerWS_1, lyrs=c(pl_j:hv_j)) 
    
  }else{
    rastRF_i1 <- if(class(terra::rast(rastLayerRF_1))[1]=='SpatRaster'){terra::rast(rastLayerRF_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerRF_1))))}
    rastRF_i2 <- if(class(terra::rast(rastLayerRF_2))[1]=='SpatRaster'){terra::rast(rastLayerRF_2, lyrs=c(1:hv_j))}
    PlHvD_RF <- c(rastRF_i1, rastRF_i2)
    
    rastTmax_i1 <- if(class(terra::rast(rastLayerTmax_1))[1]=='SpatRaster'){terra::rast(rastLayerTmax_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerTmax_1))))}
    rastTmax_i2 <- if(class(terra::rast(rastLayerTmax_2))[1]=='SpatRaster'){terra::rast(rastLayerTmax_2, lyrs=c(1:hv_j))}
    PlHvD_Tmax <- c(rastTmax_i1, rastTmax_i2)
    
    rastTmin_i1 <- if(class(terra::rast(rastLayerTmin_1))[1]=='SpatRaster'){terra::rast(rastLayerTmin_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerTmin_1))))}
    rastTmin_i2 <- if(class(terra::rast(rastLayerTmin_2))[1]=='SpatRaster'){terra::rast(rastLayerTmin_2, lyrs=c(1:hv_j))}
    PlHvD_Tmin <- c(rastTmin_i1, rastTmin_i2)
    
    rastRH_i1 <- if(class(terra::rast(rastLayerRH_1))[1]=='SpatRaster'){terra::rast(rastLayerRH_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerRH_1))))}
    rastRH_i2 <- if(class(terra::rast(rastLayerRH_2))[1]=='SpatRaster'){terra::rast(rastLayerRH_2, lyrs=c(1:hv_j))}
    PlHvD_RH <- c(rastRH_i1, rastRH_i2)
    
    rastSR_i1 <- if(class(terra::rast(rastLayerSR_1))[1]=='SpatRaster'){terra::rast(rastLayerSR_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerSR_1))))}
    rastSR_i2 <- if(class(terra::rast(rastLayerSR_2))[1]=='SpatRaster'){terra::rast(rastLayerSR_2, lyrs=c(1:hv_j))}
    PlHvD_SR <- c(rastSR_i1, rastSR_i2)
    
    rastWS_i1 <- if(class(terra::rast(rastLayerWS_1))[1]=='SpatRaster'){terra::rast(rastLayerWS_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerWS_1))))}
    rastWS_i2 <- if(class(terra::rast(rastLayerWS_2))[1]=='SpatRaster'){terra::rast(rastLayerWS_2, lyrs=c(1:hv_j))}
    PlHvD_WS <- c(rastWS_i1, rastWS_i2)
  }
  
  xy <- gpsdata[, c("longitude", "latitude")]
  
  RFi <- if(class(PlHvD_RF) == "SpatRaster"){terra::extract(PlHvD_RF, xy, method='simple', cells=FALSE)}
  RFi <- RFi[,-1]
  
  Tmaxi <- if(class(PlHvD_Tmax) == "SpatRaster"){terra::extract(PlHvD_Tmax, xy, method='simple', cells=FALSE)}
  Tmaxi <- Tmaxi[,-1]
  Tmaxi <- Tmaxi-274
  
  Tmini <- if(class(PlHvD_Tmin) == "SpatRaster"){terra::extract(PlHvD_Tmin, xy, method='simple', cells=FALSE)}
  Tmini <- Tmini[,-1]
  Tmini <- Tmini-274
  
  RHi <- if(class(PlHvD_RH) == "SpatRaster"){terra::extract(PlHvD_RH, xy, method='simple', cells=FALSE)}
  RHi <- RHi[,-1]
  
  SRi <- if(class(PlHvD_SR) == "SpatRaster"){terra::extract(PlHvD_SR, xy, method='simple', cells=FALSE)}
  SRi <- SRi[,-1]
  SRi <- SRi/1000000
  
  WSi <- if(class(PlHvD_WS) == "SpatRaster"){terra::extract(PlHvD_WS, xy, method='simple', cells=FALSE)}
  WSi <- WSi[,-1]
  
  
  
  # 3.2. Get the rainfall seasonal parameters at a location ####
  ## The total rainfall over the growing period
  
  rainiq <- t(RFi)
  gpsdata$totalRF <- colSums(rainiq)
  
  ## The number of rainy days (thr >= 2 mm) over the growing period 
  gpsdata$nrRainyDays <- NULL
  for (m in 1:nrow(RFi)){
    # print(m)
    mdata <- RFi[m, ]
    mdata[mdata < 2] <- 0
    mdata[mdata >= 2] <- 1
    gpsdata$nrRainyDays[m] <- sum(mdata)
    
    ## The monthly rainfall, at 31 days interval and the remaining  days at the end, over the growing period
    mrdi <- RFi[m, ]
    mtmaxi <- Tmaxi[m,]
    mtmini <- Tmini[m,]
    mrhi <- RHi[m,]
    msri <- SRi[m,]
    mwsi <- WSi[m,]
    
    mdiv <- unique(c(seq(1, length(mrdi), 30), length(mrdi)))
    
    mdivq <- length(mrdi)%/%31
    mdivr <- length(mrdi)%%31
    
    ##################
    mrf <- NULL
    for (q in 1:mdivq){
      mrf <- c(mrf, sum(mrdi[((q*31)-30):(q*31)]))	
    }
    # Then add the remainder
    mrf <- c(mrf, sum(mrdi[(q*31):((q*31)+mdivr)]))
    
    mtmax <- NULL
    for (q in 1:mdivq){
      mtmax <- c(mtmax, mean(as.numeric(mtmaxi[((q*31)-30):(q*31)])))	
    }
    # Then add the remainder
    mtmax <- c(mtmax, mean(as.numeric(mtmaxi[(q*31):((q*31)+mdivr)])))
    
    mtmin <- NULL
    for (q in 1:mdivq){
      mtmin <- c(mtmin, mean(as.numeric(mtmini[((q*31)-30):(q*31)])))	
    }
    # Then add the remainder
    mtmin <- c(mtmin, mean(as.numeric(mtmini[(q*31):((q*31)+mdivr)])))
    
    mrh <- NULL
    for (q in 1:mdivq){
      mrh <- c(mrh, mean(as.numeric(mrhi[((q*31)-30):(q*31)])))	
    }
    # Then add the remainder
    mrh <- c(mrh, mean(as.numeric(mrhi[(q*31):((q*31)+mdivr)])))
    
    msr <- NULL
    for (q in 1:mdivq){
      msr <- c(msr, mean(as.numeric(msri[((q*31)-30):(q*31)])))	
    }
    # Then add the remainder
    msr <- c(msr, mean(as.numeric(msri[(q*31):((q*31)+mdivr)])))
    
    mws <- NULL
    for (q in 1:mdivq){
      mws <- c(mws, mean(as.numeric(mwsi[((q*31)-30):(q*31)])))	
    }
    # Then add the remainder
    mws <- c(mws, mean(as.numeric(mwsi[(q*31):((q*31)+mdivr)])))
    
    ###################################
    
    # for (q in 1:length(mdivq)){
    #   mrf <- c(mrf, sum(mdiv[q:31*q]))
    # }
    # 
    # mrf <- c()
    # mtmax <- c()
    # mtmin <- c()
    # mrh <- c()
    # msr <- c()
    # mws <- c()
    
    # for (k in 1:(length(mdiv)-1)) {
    #   # print(k)
    #   if(k == 1){
    #     mrf <- c(mrf, sum(mrdi[c(mdiv[k]:mdiv[k+1])]))
    #     mtmax <- c(mtmax, mean(as.numeric(mtmaxi[c(mdiv[k]:mdiv[k+1])])))
    #     mtmin <- c(mtmin, mean(as.numeric(mtmini[c(mdiv[k]:mdiv[k+1])])))
    #     mrh <- c(mrh, mean(as.numeric(mrhi[c(mdiv[k]:mdiv[k+1])])))
    #     msr <- c(msr, mean(as.numeric(msri[c(mdiv[k]:mdiv[k+1])])))
    #     mws <- c(mws, mean(as.numeric(mwsi[c(mdiv[k]:mdiv[k+1])])))
    #   }else{
    #     mrf <- c(mrf, sum(mrdi[c((mdiv[k]+1):(mdiv[k+1]))]))
    #     mtmax <- c(mtmax, mean(as.numeric(mtmaxi[c((mdiv[k]+1):(mdiv[k+1]))])))
    #     mtmin <- c(mtmin, mean(as.numeric(mtmini[c((mdiv[k]+1):(mdiv[k+1]))])))
    #     mrh <- c(mrh, mean(as.numeric(mrhi[c((mdiv[k]+1):(mdiv[k+1]))])))
    #     msr <- c(msr, mean(as.numeric(msri[c((mdiv[k]+1):(mdiv[k+1]))])))
    #     mws <- c(mws, mean(as.numeric(mwsi[c((mdiv[k]+1):(mdiv[k+1]))])))
    #   }
    # }}
    # 
    
    if(length(mrf) > 15){## if the crop is > 15 months on the field ( to account for cassava as well)
      mrf <- c(mrf, rep("NA", 15 -length(mrf)))
      mtmax <- c(mtmax, rep("NA", 15 - length(mtmax)))
      mtmin <- c(mtmin, rep("NA", 15 -length(mtmin)))
      mrh <- c(mrh, rep("NA", 15 -length(mrh)))
      msr <- c(msr, rep("NA", 15 -length(msr)))
      mws <- c(mws, rep("NA", 15 -length(mws)))
    }
    
    mrf_names <- c(paste0("Rain_month", c(1:15)))
    mtmax_names <- c(paste0("Tmax_month", c(1:15)))
    mtmin_names <- c(paste0("Tmin_month", c(1:15)))
    mrh_names <- c(paste0("relativeHumid_month", c(1:15)))
    msr_names <- c(paste0("solarRad_month", c(1:15)))
    mws_names <- c(paste0("windSpeed_month", c(1:15)))
    
    
    for (h in 1:length(mrf_names)) {
      colname <- mrf_names[h]
      gpsdata[[colname]][m] <- mrf[h]
      
      colname <- mtmax_names[h]
      gpsdata[[colname]] <- mtmax[h]
      
      colname <- mtmin_names[h]
      gpsdata[[colname]] <- mtmin[h]
      
      colname <- mrh_names[h]
      gpsdata[[colname]] <- mrh[h]
      
      colname <- msr_names[h]
      gpsdata[[colname]] <- msr[h]
      
      colname <- mws_names[h]
      gpsdata[[colname]] <- mws[h]
    }
    
    
    if(planting_harvest_sameYear== TRUE){
      gpsdata$plantingYear <- str_extract(rastLayerRF_1, "[[:digit:]]+")
      gpsdata$harvestYear <- str_extract(rastLayerRF_1, "[[:digit:]]+")
    }else{
      gpsdata$plantingYear <- str_extract(rastLayerRF_1, "[[:digit:]]+")
      gpsdata$harvestYear <- str_extract(rastLayerRF_2, "[[:digit:]]+")
    }
    
    gpsdata <- gpsdata %>% 
      select_if(~sum(!is.na(.)) > 0)
    
    return(gpsdata)
  }
  
}

# 5. Extract the season rainfall parameters for point based data -------------------------------------------
#' @description this functions loops through all .nc files (~30 -40 years) for rainfall to provide point based seasonal rainfall parameters.
#' @details for AOI it requires a "AOI_GPS.RDS" data frame with c("longitude","latitude") columns being saved in 
#'                            paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/", Crop, "/raw", sep="") 
#'          for trial sites it requires a "compiled_fieldData.RDS" data frame with c("lon", "lat", "plantingDate", "harvestDate") beinf saved in 
#'                    paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result", sep="")
#'
#' @param country country name
#' @param varsbasePath is the path to access the global geo-spatial layers (CHIRPS, ISRIC, .. layers)
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param pathOut the path to store the extracted result geo-spatial data 
#' @param Planting_month_date is provided as mm-dd, and for AOI, it is the intended planting date defined using expert knowledge, crop models, remote sensing analysis, etc, preferably it should also make use of climate forecast info
#' @param Harvest_month_date is provided as mm-dd, defined in similar way as Planting_month_date
#' @param sourceFnc is the get_geoSpatialData.R with teh path, which is needed within the parallel computation
#' @param jobs defines how many cores to use for parallel data sourcing
#' 
#' @return a data frame containing the col information & columns corresponding to the rainfall parameters#' 
#'        totalRF : Total rainfall between pl_Date and hv_Date (mm)
#'        nrRainyDays : Number of rainy days between pl_Date and hv_Date (days)
#'        di : Average daily rainfall between pl_Date and hv_Date (mm/day)
#'        Rain_monthx: total monthly rainfall 
#' @examples: get_rf_pointSummarydata(country = "Rwanda"; AOI = FALSE; overwrite = TRUE;
#' Planting_month_date = "07-01";  Harvest_month_date = "11-30";jobs=10, id = "TLID")
#' 


get_WeatherSummarydata <- function(country, AOI = FALSE, inputData = NULL, varsbasePath=varsbasePath,
                                   Planting_month_date = NULL, Harvest_month_date = NULL, zone=NULL, 
                                   season=1, pathOut = NULL, sourceFnc = sourceFnc,jobs = 10){
  
  ARD <- Paths_Vars(inputData = inputData, varsbasePath=varsbasePath, soilProfile = TRUE, pathOut = pathOut)
  
  # Input layers
  listRaster_RF <- ARD[[2]]
  listRaster_Tmax <- ARD[[3]]
  listRaster_Tmin <- ARD[[4]]
  listRaster_RH <- ARD[[5]]
  listRaster_SR <- ARD[[6]]
  listRaster_WS <- ARD[[7]]
  
  
  if(AOI == TRUE){
    listRaster_RF <- listRaster_RF[c(grep(2000,listRaster_RF ): grep(2022,listRaster_RF ))] 
    listRaster_Tmax <- listRaster_Tmax[c(grep(2000,listRaster_Tmax ): grep(2022,listRaster_Tmax ))]
    listRaster_Tmin <- listRaster_Tmin[c(grep(2000,listRaster_Tmin ): grep(2022,listRaster_Tmin ))]
    listRaster_RH <- listRaster_RH[c(grep(2000,listRaster_RH ): grep(2022,listRaster_RH ))]
    listRaster_SR <- listRaster_SR[c(grep(2000,listRaster_SR ): grep(2022,listRaster_SR ))]
    listRaster_WS <- listRaster_WS[c(grep(2000,listRaster_WS ): grep(2022,listRaster_WS ))]
  }
  
  
  # Creation of the output dir
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  
  # Input point data AOI / Trial
  if(AOI == TRUE){
    countryCoord <- inputData
    countryCoord$ID <- c(1:nrow(inputData))
    countryCoord <- unique(countryCoord[, c("lon", "lat", "ID")])
    
    ## check if both planting and harvest dates are in the same year
    Planting_month <- as.numeric(str_extract(Planting_month_date, "[^-]+"))
    harvest_month <- as.numeric(str_extract(Harvest_month_date, "[^-]+"))
    if(Planting_month < harvest_month){
      planting_harvest_sameYear <- TRUE
    }else{
      planting_harvest_sameYear <- FALSE
    }
    
    # add a place holder for the year to get the julian date  
    if(planting_harvest_sameYear ==TRUE){
      countryCoord$plantingDate <- paste(2001, Planting_month_date, sep="-")
      countryCoord$harvestDate <- paste(2001, Harvest_month_date, sep="-")
    }else{
      countryCoord$plantingDate <- paste(2001, Planting_month_date, sep="-")
      countryCoord$harvestDate <- paste(2002, Harvest_month_date, sep="-")
    }
    countryCoord <- countryCoord[complete.cases(countryCoord),]
    names(countryCoord) <- c("longitude", "latitude", "ID" ,"plantingDate", "harvestDate")
    ground <- countryCoord
  }else{
    countryCoord <- unique(inputData[, c("lon", "lat", "ID", "plantingDate", "harvestDate")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord) <- c("longitude", "latitude","ID" ,"plantingDate", "harvestDate")
    ground <- countryCoord
  }
  
  
  ground$Planting <- as.Date(ground$plantingDate, "%Y-%m-%d")
  ground$Harvesting <- as.Date(ground$harvestDate, "%Y-%m-%d") 
  countryShp <- geodata::gadm(country, level = 2, path='.')
  dd2 <- raster::extract(countryShp, ground[, c("longitude", "latitude")])[, c("NAME_1", "NAME_2")]
  ground$NAME_1 <- dd2$NAME_1
  ground$NAME_2 <- dd2$NAME_2
  
  ground$pyear <- as.numeric(format(as.POSIXlt(ground$plantingDate), "%Y"))
  ground <- ground[ground$pyear >= 1981, ]
  
  
  # Compute the seasonal rainfall parameters
  if(AOI == TRUE){
    # Convert planting Date and harvesting in Julian Day 
    pl_j <-as.POSIXlt(unique(ground$Planting))$yday
    hv_j <-as.POSIXlt(unique(ground$Harvesting))$yday
    
    if (planting_harvest_sameYear ==  TRUE) {
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      # Loop over all the years 
      rf_result <- foreach(i=1:(length(listRaster_RF)), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        rastRF_1 <- listRaster_RF[i]
        rastTmax_1 <- listRaster_Tmax[i]
        rastTmin_1 <- listRaster_Tmin[i]
        rastRH_1 <- listRaster_RH[i]
        rastSR_1 <- listRaster_SR[i]
        rastWS_1 <- listRaster_WS[i]
        
        source(sourceFnc, local=TRUE)
        # source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData.R", local = TRUE)
        
        summarize_pointdata(rastLayerRF_1=rastRF_1, rastLayerRF_2 = NULL,
                            rastLayerTmax_1=rastTmax_1, rastLayerTmax_2 = NULL,
                            rastLayerTmin_1=rastTmin_1, rastLayerTmin_2 = NULL,
                            rastLayerRH_1=rastRH_1, rastLayerRH_2 = NULL,
                            rastLayerSR_1=rastSR_1, rastLayerSR_2 = NULL,
                            rastLayerWS_1=rastWS_1, rastLayerWS_2 = NULL,
                            gpsdata = ground, pl_j=pl_j, hv_j=hv_j, 
                            planting_harvest_sameYear = planting_harvest_sameYear)
      }
      rainfall_points <- do.call(rbind, rf_result)
      
    }
    
    
    if (planting_harvest_sameYear ==  FALSE) {
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      
      
      rf_result2 <- foreach(i = 1:(length(listRaster_RF)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        source(sourceFnc, local=TRUE)
        # source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData.R", local = TRUE)
        rastRF_1 <- listRaster_RF[i]
        rastRF_2 <- listRaster_RF[i+1]
        
        rastTmax_1 <- listRaster_Tmax[i]
        rastTmax_2 <- listRaster_Tmax[i+1]
        
        rastTmin_1 <- listRaster_Tmin[i]
        rastTmin_2 <- listRaster_Tmin[i+1]
        
        rastRH_1 <- listRaster_RH[i]
        rastRH_2 <- listRaster_RH[i+1]
        
        rastSR_1 <- listRaster_SR[i]
        rastSR_2 <- listRaster_SR[i+1]
        
        rastWS_1 <- listRaster_WS[i]
        rastWS_2 <- listRaster_WS[i+1]
        
        
        summarize_pointdata(rastLayerRF_1=rastRF_1, rastLayerRF_2 = rastRF_2, 
                            rastLayerTmax_1=rastTmax_1, rastLayerTmax_2 = rastTmax_2,
                            rastLayerTmin_1=rastTmin_1, rastLayerTmin_2 = rastTmin_2,
                            rastLayerRH_1=rastRH_1, rastLayerRH_2 = rastRH_2,
                            rastLayerSR_1=rastSR_1, rastLayerSR_2 = rastSR_2,
                            rastLayerWS_1=rastWS_1, rastLayerWS_2 = rastWS_2,
                            gpsdata = ground, pl_j=pl_j, hv_j=hv_j, 
                            planting_harvest_sameYear = planting_harvest_sameYear)
      }
      rainfall_points <- do.call(rbind, rf_result2)
      
    }
    
    stopCluster(cls)
    # Compute the seasonal rainfall parameters for trial data: having varying planting and harvest dates
  } else {
    
    rainfall_points <- NULL
    for(i in 1:nrow(ground)){
      print(i)
      groundi <- ground[i,]
      yearPi <- format(as.POSIXlt(groundi$Planting), "%Y")
      yearHi <- format(as.POSIXlt(groundi$Harvesting), "%Y")
      pl_j <-as.POSIXlt(groundi$Planting)$yday
      hv_j <-as.POSIXlt(groundi$Harvesting)$yday
      
      # one layer per trial when pla ting and harvest year are the same, two otherwise
      if (yearPi == yearHi) {
        rastRF_i <-listRaster_RF[which(grepl(yearPi, listRaster_RF, fixed=TRUE) == T)]
        rastRF_i <- terra::rast(rastRF_i, lyrs=c(pl_j:hv_j))
        
        rastTmax_i <-listRaster_Tmax[which(grepl(yearPi, listRaster_Tmax, fixed=TRUE) == T)]
        rastTmax_i <- terra::rast(rastTmax_i, lyrs=c(pl_j:hv_j))
        
        rastTmin_i <-listRaster_Tmin[which(grepl(yearPi, listRaster_Tmin, fixed=TRUE) == T)]
        rastTmin_i <- terra::rast(rastTmin_i, lyrs=c(pl_j:hv_j))
        
        rastRH_i <-listRaster_RH[which(grepl(yearPi, listRaster_RH, fixed=TRUE) == T)]
        rastRH_i <- terra::rast(rastRH_i, lyrs=c(pl_j:hv_j))
        
        rastSR_i <-listRaster_SR[which(grepl(yearPi, listRaster_SR, fixed=TRUE) == T)]
        rastSR_i <- terra::rast(rastSR_i, lyrs=c(pl_j:hv_j))
        
        rastWS_i <-listRaster_WS[which(grepl(yearPi, listRaster_WS, fixed=TRUE) == T)]
        rastWS_i <- terra::rast(rastWS_i, lyrs=c(pl_j:hv_j))
      }else{
        rastRF_i1 <-listRaster_RF[which(grepl(yearPi, listRaster_RF, fixed=TRUE) == T)]
        rastRF_i1 <- terra::rast(rastRF_i1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastRF_i1))))
        rastRF_i2 <-listRaster_RF[which(grepl(yearHi, listRaster_RF, fixed=TRUE) == T)]
        rastRF_i2 <- terra::rast(rastRF_i2, lyrs=c(1:hv_j))
        rastRF_i <- c(rastRF_i1, rastRF_i2)
        
        rastTmax_i1 <-listRaster_Tmax[which(grepl(yearPi, listRaster_Tmax, fixed=TRUE) == T)]
        rastTmax_i1 <- terra::rast(rastTmax_i1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastTmax_i1))))
        rastTmax_i2 <-listRaster_Tmax[which(grepl(yearHi, listRaster_Tmax, fixed=TRUE) == T)]
        rastTmax_i2 <- terra::rast(rastTmax_i2, lyrs=c(1:hv_j))
        rastTmax_i <- c(rastTmax_i1, rastTmax_i2)
        
        rastTmin_i1 <-listRaster_Tmin[which(grepl(yearPi, listRaster_Tmin, fixed=TRUE) == T)]
        rastTmin_i1 <- terra::rast(rastTmin_i1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastTmin_i1))))
        rastTmin_i2 <-listRaster_Tmin[which(grepl(yearHi, listRaster_Tmin, fixed=TRUE) == T)]
        rastTmin_i2 <- terra::rast(rastTmin_i2, lyrs=c(1:hv_j))
        rastTmin_i <- c(rastTmin_i1, rastTmin_i2)
        
        rastRH_i1 <-listRaster_RH[which(grepl(yearPi, listRaster_RH, fixed=TRUE) == T)]
        rastRH_i1 <- terra::rast(rastRH_i1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastRH_i1))))
        rastRH_i2 <-listRaster_RH[which(grepl(yearHi, listRaster_RH, fixed=TRUE) == T)]
        rastRH_i2 <- terra::rast(rastRH_i2, lyrs=c(1:hv_j))
        rastRH_i <- c(rastRH_i1, rastRH_i2)
        
        rastSR_i1 <-listRaster_SR[which(grepl(yearPi, listRaster_SR, fixed=TRUE) == T)]
        rastSR_i1 <- terra::rast(rastSR_i1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastSR_i1))))
        rastSR_i2 <-listRaster_SR[which(grepl(yearHi, listRaster_SR, fixed=TRUE) == T)]
        rastSR_i2 <- terra::rast(rastSR_i2, lyrs=c(1:hv_j))
        rastSR_i <- c(rastSR_i1, rastSR_i2)
        
        rastWS_i1 <-listRaster_WS[which(grepl(yearPi, listRaster_WS, fixed=TRUE) == T)]
        rastWS_i1 <- terra::rast(rastWS_i1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastWS_i1))))
        rastWS_i2 <-listRaster_WS[which(grepl(yearHi, listRaster_WS, fixed=TRUE) == T)]
        rastWS_i2 <- terra::rast(rastWS_i2, lyrs=c(1:hv_j))
        rastWS_i <- c(rastWS_i1, rastWS_i2)
        
      }
      
      ##Extract the information for the i-th row ####
      xy <- groundi[, c("longitude", "latitude")]
      xy <- xy %>%
        mutate_if(is.character, as.numeric)
      
      rainfall_points_i <- terra::extract(rastRF_i, xy,method='simple', cells=FALSE)
      Tmax_points_i <- terra::extract(rastTmax_i, xy,method='simple', cells=FALSE)
      Tmin_points_i <- terra::extract(rastTmin_i, xy,method='simple', cells=FALSE)
      RH_points_i <- terra::extract(rastRH_i, xy,method='simple', cells=FALSE)
      SR_points_i <- terra::extract(rastSR_i, xy,method='simple', cells=FALSE)
      WS_points_i <- terra::extract(rastWS_i, xy,method='simple', cells=FALSE)
      
      
      Tmax_points_i <- Tmax_points_i-274
      Tmin_points_i <- Tmin_points_i-274
      SR_points_i <- SR_points_i/1000000
      
      
      ## get year
      groundi$Year <- yearPi
      
      # Compute the total amount of rainfall
      groundi$totalRF <- sum(rainfall_points_i[c(2:length(rainfall_points_i))])
      
      # Compute the Number of rainy day
      nrdi <- rainfall_points_i[c(2:length(rainfall_points_i))]
      nrdi[nrdi < 2] <- 0
      nrdi[nrdi >= 2] <- 1
      groundi$nrRainyDays <- sum(nrdi)
      
      # Compute monthly total, at 31 days interval and the remaining  days at the end
      mrdi <- rainfall_points_i[c(2:length(rainfall_points_i))]
      mtmaxi <- Tmax_points_i[c(2:length(Tmax_points_i))]
      mtmini <- Tmin_points_i[c(2:length(Tmin_points_i))]
      mrhi <- RH_points_i[c(2:length(RH_points_i))]
      msri <- SR_points_i[c(2:length(SR_points_i))]
      mwsi <- WS_points_i[c(2:length(WS_points_i))]
      
      
      mdiv <- unique(c(seq(1, length(mrdi), 30), length(mrdi)))
      length(mtmini)
      
      mrf <- c()
      mtmax <- c()
      mtmin <- c()
      mrh <- c()
      msr <- c()
      mws <- c()
      for (k in 1:(length(mdiv)-1)) {
        print(k)
        if(k == 1){
          mrf <- c(mrf, sum(mrdi[c(mdiv[k]:mdiv[k+1])]))
          mtmax <- c(mtmax, mean(as.numeric(mtmaxi[c(mdiv[k]:mdiv[k+1])])))
          mtmin <- c(mtmin, mean(as.numeric(mtmini[c(mdiv[k]:mdiv[k+1])])))
          mrh <- c(mrh, mean(as.numeric(mrhi[c(mdiv[k]:mdiv[k+1])])))
          msr <- c(msr, mean(as.numeric(msri[c(mdiv[k]:mdiv[k+1])])))
          mws <- c(mws, mean(as.numeric(mwsi[c(mdiv[k]:mdiv[k+1])])))
        }else{
          mrf <- c(mrf, sum(mrdi[c((mdiv[k]+1):(mdiv[k+1]))]))
          mtmax <- c(mtmax, mean(as.numeric(mtmaxi[c((mdiv[k]+1):(mdiv[k+1]))])))
          mtmin <- c(mtmin, mean(as.numeric(mtmini[c((mdiv[k]+1):(mdiv[k+1]))])))
          mrh <- c(mrh, mean(as.numeric(mrhi[c((mdiv[k]+1):(mdiv[k+1]))])))
          msr <- c(msr, mean(as.numeric(msri[c((mdiv[k]+1):(mdiv[k+1]))])))
          mws <- c(mws, mean(as.numeric(mwsi[c((mdiv[k]+1):(mdiv[k+1]))])))
        }
      }
      
      ## if the crop is > 15 months on the field (to make it work for cassava, hatcan have 15 months growing period)
      if(length(mrf) > 12){
        mrf <- c(mrf, rep("NA", 12 - length(mrf)))
        mtmax <- c(mtmax, rep("NA", 12 - length(mtmax)))
        mtmin <- c(mtmin, rep("NA", 12 -length(mtmin)))
        mrh <- c(mrh, rep("NA", 12 -length(mrh)))
        msr <- c(msr, rep("NA", 12 -length(msr)))
        mws <- c(mws, rep("NA", 12 -length(mws)))
      }
      
      mrf_names <- c(paste0("Rainfall_month", c(1:12)))
      mtmax_names <- c(paste0("Tmax_month", c(1:12)))
      mtmin_names <- c(paste0("Tmin_month", c(1:12)))
      mrh_names <- c(paste0("relativeHumid_month", c(1:12)))
      msr_names <- c(paste0("solarRad_month", c(1:12)))
      mws_names <- c(paste0("windSpeed_month", c(1:12)))
      
      
      for (h in 1:length(mrf_names)) {
        colname <- mrf_names[h]
        groundi[[colname]] <- mrf[h]
        
        colname <- mtmax_names[h]
        groundi[[colname]] <- mtmax[h]
        
        colname <- mtmin_names[h]
        groundi[[colname]] <- mtmin[h]
        
        colname <- mrh_names[h]
        groundi[[colname]] <- mrh[h]
        
        colname <- msr_names[h]
        groundi[[colname]] <- msr[h]
        
        colname <- mws_names[h]
        groundi[[colname]] <- mws[h]
      }
      
      groundi <- subset(groundi, select=-c(Planting, Harvesting, Year))
      
      rainfall_points <- bind_rows(rainfall_points, groundi)
    }
  }
  
  
  ## drop NA columns and save result
  rainfall_points <- rainfall_points %>% 
    select_if(~sum(!is.na(.)) > 0)
  
  if(AOI == TRUE){
    if(!is.null(zone)){
      fname <- paste("weatherSummaries_Season_", season, zone, "_AOI.RDS",sep="")
    }else{
      fname <- paste("weatherSummaries_Season_", season, "_AOI.RDS",sep="")
    }
    
  }else{
    fname <- "weatherSummaries_trial.RDS"
  }
  
  saveRDS(object = rainfall_points, file=paste0(pathOut, fname))
  
  return(rainfall_points)
}


## TODO the content for acidSoils is put within a function so that sourcing this R file will not run it 
##  get_weather_seasonality is already covered by the fucntions above, I do not undertand why we need this one?
##############################################################################
## acid soil dat sourcing developed by icirsat TODO marget
##############################################################################

acidSoils <- function(){
  #setwd("C:/Users/MPatil/OneDrive - ICRISAT/_ICRISAT/EiA")
  
  country <- geodata::world(path='.', resolution=5, level=0)
  isocodes <- geodata::country_codes()
  isocodes_eth<-subset(isocodes,NAME=='Ethiopia')
  eth <- subset(country, country$GID_0 %in% isocodes_eth$ISO3)
  
  ##Data preparation
  
  ph5  <- geodata::soil_af(var='pH', depth=5 , path='.')
  ph15 <- geodata::soil_af(var='pH', depth=15, path='.')
  ph30 <- geodata::soil_af(var='pH', depth=30, path='.')
  ph <- (5*ph5 + 10*ph15 + 15*ph30) / 30
  ph <- terra::crop(x=ph, y=eth, mask=T)
  terra::writeRaster(ph, 'ph_eth.tiff', overwrite=T)
  ph<-terra::rast('ph_eth.tiff')
  # load geosurvey
  
  geosurvey <- geodata::cropland(source='QED', path='.')
  geosurvey <- terra::crop(x=geosurvey, y=eth, mask=T)
  m <- c(0, 0.1, NA, 0.1, 1, 1) # exclude cropland below 10% probability   
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  geosurvey <- terra::classify(geosurvey, rclmat)
  geosurvey <- terra::resample(geosurvey, ph) # to align geo-survey with soil-grids
  terra::writeRaster(geosurvey, './geosurvey_processed.tiff', overwrite=T)
  
  # ph cropland
  ph_cropland <- ph * geosurvey
  names(ph_cropland) <- 'ph'
  terra::writeRaster(ph_cropland, filename='soilgrids_ph_cropland.tiff', overwrite=T)
  ph_cropland_final <- ph_cropland
  ph_cropland_final[ph_cropland_final > 5.5] <- NA
  terra::writeRaster(ph_cropland_final, './ph_cropland_final.tiff', overwrite=T)
  
  clay5  <- geodata::soil_af(var='clay', depth=5 , path='.')
  clay15  <- geodata::soil_af(var='clay', depth=15 , path='.')
  clay30  <- geodata::soil_af(var='clay', depth=30 , path='.')
  clay <- (5*clay5 + 10*clay15 + 15*clay30) / 30
  clay <- terra::crop(x=clay, y=eth, mask=T)
  terra::writeRaster(clay, 'clay_eth.tiff', overwrite=T)
  
  silt5  <- geodata::soil_af(var='silt', depth=5 , path='.')
  silt15  <- geodata::soil_af(var='silt', depth=15 , path='.')
  silt30  <- geodata::soil_af(var='silt', depth=30 , path='.')
  silt <- (5*silt5 + 10*silt15 + 15*silt30) / 30
  silt <- terra::crop(x=silt, y=eth, mask=T)
  terra::writeRaster(silt, 'silt_eth.tiff', overwrite=T)
  
  texture<-sum(silt,clay)
  terra::writeRaster(texture,'texture_eth.tiff',overwrite=T)
  
  soc5  <- geodata::soil_af(var='SOC', depth=5 , path='.')
  soc15  <- geodata::soil_af(var='SOC', depth=15 , path='.')
  soc30  <- geodata::soil_af(var='SOC', depth=30 , path='.')
  soc <- (5*soc5 + 10*soc15 + 15*soc30) / 30
  soc <- terra::crop(x=soc, y=eth, mask=T)
  terra::writeRaster(soc, 'soc_eth.tiff', overwrite=T)
  
  p  <- geodata::soil_af_elements(var='P',  path='.')
  p <- terra::crop(x=p, y=eth, mask=T)
  terra::writeRaster(p, 'exP_eth.tiff', overwrite=T)
  
  exAcid5  <- geodata::soil_af(var='acid-exch', depth=5 , path='.')
  exAcid15  <- geodata::soil_af(var='acid-exch', depth=15 , path='.')
  exAcid30  <- geodata::soil_af(var='acid-exch', depth=30 , path='.')
  exAcid <- (5*acidEx5 + 10*acidEx15 + 15*acidEx30) / 30
  exAcid <- terra::crop(x=exAcid, y=eth, mask=T)
  terra::writeRaster(exAcid, 'acidEx_eth.tiff', overwrite=T)
  
  mgEx20  <- geodata::soil_af(var='Mg-exch', depth=20 , path='.')
  mgEx <- terra::crop(x=mgEx20, y=eth, mask=T)
  terra::writeRaster(mgEx, 'mgEx_eth.tiff', overwrite=T)
  
  
  caEx20  <- geodata::soil_af(var='Ca-exch', depth=20 , path='.')
  caEx <- terra::crop(x=caEx20, y=eth, mask=T)
  terra::writeRaster(caEx, 'caEx_eth.tiff', overwrite=T)
  
  kEx20  <- geodata::soil_af(var='K-exch', depth=20 , path='.')
  kEx <- terra::crop(x=kEx20, y=eth, mask=T)
  terra::writeRaster(kEx, 'kEx_eth.tiff', overwrite=T)
  
  naEx20  <- geodata::soil_af(var='Na-exch', depth=20 , path='.')
  naEx <- terra::crop(x=naEx20, y=eth, mask=T)
  terra::writeRaster(naEx, 'naEx_eth.tiff', overwrite=T)
  
  alEx20  <- geodata::soil_af(var='Al-exch', depth=20 , path='.')
  alEx <- terra::crop(x=alEx20, y=eth, mask=T)
  terra::writeRaster(alEx, 'alEx_eth.tiff', overwrite=T)
  
  ecec<-sum(caEx,kEx,mgEx,naEx,alEx)
  
  terra::writeRaster(ecec,'ecec_eth.tiff',overwrite=T)
  
  tmin<-worldclim_country("Ethiopia", var="tmin",path='.')
  tmin<-min(tmin)
  tmin <- terra::crop(x=tmin, y=eth, mask=T)
  writeRaster(tmin,filename = "tmin_eth.tif")
  tmax<-worldclim_country("Ethiopia", var="tmax",path='.')
  tmax<-max(tmax)
  tmax <- terra::crop(x=tmax, y=eth, mask=T)
  writeRaster(tmax,filename = "tmax_eth.tif")
  rainfall<-worldclim_country("Ethiopia", var="prec",path='.')
  rainfall<-sum(rainfall)
  rainfall <- terra::crop(x=rainfall, y=eth, mask=T)
  writeRaster(rainfall,filename = "rainfall_eth.tif")
  tmin<-raster('tmin_eth.tif')
  tmax<-raster('tmax_eth.tif')
  rainfall<-raster('rainfall_eth.tif')
}


#################################################################################################################
## global 
#################################################################################################################
source("~/shared-data/Scripts/generic/dataSourcing/get_geoSpatialData.R")
country <- "Ethiopia"
varsbasePath <- "~/shared-data/Data/General/Global_GeoData/Landing/"

#################################################################################################################
#################################################################################################################
##### 1. AOI 
#################################################################################################################
## Sourcing the geo-spatial soil and weather data for AOI for crop model 
#################################################################################################################
inputDataAOI <- readRDS("~/shared-data/Data/Wheat/geoSpatial/Wheat_AOI.RDS")
pathOut = "~/shared-data/Data/Wheat/geoSpatial/geo_4cropModel/"

## Given the crop growing window is defined for every zone differently we need to run the data sourcing by zone 
for( zones in unique(inputDataAOI$Zone)){
  print(zones)
  zone_inputData <- inputDataAOI[inputDataAOI$Zone == zones ,]
  Planting_month_date <- unique(zone_inputData$planting_month_date)
  Harvest_month_date <- unique(zone_inputData$harvest_month_date)
  season <- 1
  plantingWindow <- 4
  
  pathOutZ = paste(pathOut, zones, "/", sep="")
  
  ## Data for crop models at AOI : weather + the 6 profiles of soil grids soil data
  Eth_Wheat_AOI_profileS1 <- extract_geoSpatialPointData(country = country, inputData=zone_inputData,
                                                         AOI=TRUE, Planting_month_date = Planting_month_date,  Harvest_month_date = Harvest_month_date,
                                                         soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, plantingWindow = plantingWindow, 
                                                         season = 1, jobs =10, pathOut=pathOutZ)
}




#################################################################################################################
## get the geo spatial data for AOI for ML training
#################################################################################################################
pathOut = "~/shared-data/Data/Wheat/geoSpatial/geo_4ML_AOI/"
AOI_soil_ML <- extract_geoSpatialPointData(country = country, inputData=inputDataAOI, varsbasePath=varsbasePath, pathOut= pathOut,
                                           AOI=TRUE, Planting_month_date=NULL, Harvest_month_date=NULL,
                                           soilData = TRUE, weatherData = FALSE, soilProfile = FALSE, season = 1,
                                           jobs =10)


## Given the crop growing window is defined for every zone differently we need to run the data sourcing by zone 
for(zones in unique(inputDataAOI$Zone)){
  print(zones)
  zone_inputData <- inputDataAOI[inputDataAOI$Zone == zones ,]
  Planting_month_date <- unique(zone_inputData$planting_month_date)
  Harvest_month_date <- unique(zone_inputData$harvest_month_date)
  season <- 1
  plantingWindow <- 4
  
  # pathOutZ = paste(pathOut, zones, "/", sep="")
  
  ## Data for crop models at AOI : weather + the 6 profiles of soil grids soil data
  AOI_summary_weather <- get_WeatherSummarydata(country = country, inputData = zone_inputData, AOI = TRUE, varsbasePath=varsbasePath,
                                                Planting_month_date=Planting_month_date, Harvest_month_date=Harvest_month_date, pathOut= pathOut, 
                                                sourceFnc="~/shared-data/Scripts/generic/dataSourcing/get_geoSpatialData.R", jobs=10)
  
}

## reading the files and merge

dat.files  <- list.files(path="~/shared-data/Data/Wheat/geoSpatial/geo_4ML_AOI",
                         recursive=TRUE, full.names=TRUE)
dat.files <- unique(dat.files[-26])

names4th <- c( "Rain_month4","Tmax_month4","Tmin_month4","relativeHumid_month4", "solarRad_month4","windSpeed_month4")
names5th <- c( "Rain_month5","Tmax_month5","Tmin_month5","relativeHumid_month5", "solarRad_month5","windSpeed_month5")
names6th <- c( "Rain_month6","Tmax_month6","Tmin_month6","relativeHumid_month6", "solarRad_month6","windSpeed_month6")

AOI_summary_weather2 <- NULL
for(i in 1:length(dat.files)){
  print(i)
  df1 <- readRDS(dat.files[i])
  if(all(!names4th %in% names(df1))){
    df1 <- df1 %>%
      dplyr::mutate(Rain_month4 = NA, Tmax_month4 =NA, Tmin_month4 = NA, relativeHumid_month4 =NA, solarRad_month4 = NA, windSpeed_month4=NA)
  }
  if(all(!names5th %in% names(df1))){
    df1 <- df1 %>%
      dplyr::mutate(Rain_month5 = NA, Tmax_month5 =NA, Tmin_month5 = NA, relativeHumid_month5 =NA, solarRad_month5 = NA, windSpeed_month5=NA)
  }
  if(all(!names6th %in% names(df1))){
    df1 <- df1 %>%
      dplyr::mutate(Rain_month6 = NA, Tmax_month6 =NA, Tmin_month6 = NA, relativeHumid_month6 =NA, solarRad_month6 = NA, windSpeed_month6=NA)
  }
  df1 <- df1 %>% 
    dplyr::select("longitude","latitude","ID","plantingDate","harvestDate","Planting", "Harvesting","NAME_1","NAME_2","pyear",
                  "totalRF","nrRainyDays", "Rain_month1","Tmax_month1","Tmin_month1", "relativeHumid_month1", "solarRad_month1",
                  "windSpeed_month1", "Rain_month2","Tmax_month2","Tmin_month2","relativeHumid_month2", "solarRad_month2","windSpeed_month2",
                  "Rain_month3","Tmax_month3","Tmin_month3","relativeHumid_month3", "solarRad_month3","windSpeed_month3", "Rain_month4",
                  "Tmax_month4","Tmin_month4","relativeHumid_month4", "solarRad_month4","windSpeed_month4","Rain_month5","Tmax_month5",
                  "Tmin_month5","relativeHumid_month5", "solarRad_month5","windSpeed_month5","Rain_month6","Tmax_month6","Tmin_month6",
                  "relativeHumid_month6", "solarRad_month6","windSpeed_month6","plantingYear","harvestYear")
  print("df1 is ready")
  AOI_summary_weather2 <- rbind(AOI_summary_weather2, df1)
  print("df1 is added")
}

saveRDS(AOI_summary_weather2, "~/shared-data/Data/Wheat/geoSpatial/geo_4ML_AOI/weatherSummaries_Season_1_AOI.RDS")



#################################################################################################################
#################################################################################################################
##### 2. trial locations 
#################################################################################################################
## Sourcing the geo-spatial soil and weather data for trials for crop model 
#################################################################################################################

## for trial locations: needs to be linked to the planting an harvest dates as defined by zone 
inputDataTrial <- readRDS("~/shared-data/Data/Wheat/fieldData/wheat_modelReady.rds")
inputDataTrial <- inputDataTrial %>% 
  dplyr::select(trial_id, year, NAME_1, NAME_2,long2, lat2) %>% 
  dplyr::rename(TLID = trial_id, lon=long2, lat=lat2) %>% 
  dplyr::mutate(Zone = paste(NAME_1, NAME_2, sep="_")) %>% 
  unique()
str(inputDataTrial)

PlD_wheat <- readRDS("~/shared-data/Data/Wheat/geoSpatial/Wheat_pLHv_byZone.RDS")
head(PlD_wheat)
PlD_wheat$NAME_1 <- gsub("Southern Nations, Nationalities", "SNNP", PlD_wheat$NAME_1)
PlD_wheat$Zone <- gsub("Southern Nations, Nationalities", "SNNP", PlD_wheat$Zone)

inputDataTrial <- merge(inputDataTrial, PlD_wheat, by=c("Zone", "NAME_1"), all.x=TRUE)
## if there are trials outside the wheat belt and do not have planting and harvest dates,
## assign the most frequent dates in the zone to the missing data
unique(inputDataTrial[is.na(inputDataTrial$planting_month_date),]$NAME_1)

PlD_wheat[PlD_wheat$NAME_1 %in% unique(inputDataTrial[is.na(inputDataTrial$planting_month_date),]$NAME_1), ]
inputDataTrial[inputDataTrial$NAME_1 == "Tigray", ]
inputDataTigray <- inputDataTrial %>% 
  dplyr::filter(NAME_1 == "Tigray") %>% 
  mutate(planting_month_date = "05-11",
         plMonth = "05",
         plDate = 11,
         HvWeeks= 15,
         pld= "2024-05-11",
         hvd="2024-10-08",
         hvMonth=10,
         hvDate="08",
         harvest_month_date="10-08") 
inputDataTrial <- inputDataTrial %>% 
  dplyr::filter(!NAME_1 %in% c("Tigray", "Dire Dawa")) %>% 
  rbind(inputDataTigray)
inputDataTrial$plantingDate <- paste(inputDataTrial$year, inputDataTrial$planting_month_date, sep="-")
inputDataTrial$harvestDate <- paste(inputDataTrial$year, inputDataTrial$harvest_month_date, sep="-")
head(inputDataTrial)
inputDataTrial$ID <- inputDataTrial$TLID ## this is very important to link differnt data frames later, 
length(unique(inputDataTrial$ID))
## get data for trial crop model
pathOut <- "~/shared-data/Data/Wheat/geoSpatial/geo_4cropModel_trial/"
Wheat_weather_soil_trial_profile <- extract_geoSpatialPointData(country = country, inputData=inputDataTrial,varsbasePath=varsbasePath,
                                                                AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL,
                                                                soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, season = 1,
                                                                jobs =10, pathOut= pathOut)



#################################################################################################################
## get the geo spatial data for trial locations for ML training
#################################################################################################################

pathOut <- "~/shared-data/Data/Wheat/geoSpatial/geo_4ML_trial/"

trial_summary_weather <- get_WeatherSummarydata(country = country, inputData = inputDataTrial, AOI = FALSE, varsbasePath=varsbasePath,
                                                Planting_month_date=NULL, Harvest_month_date=NULL, pathOut= pathOut, 
                                                sourceFnc="~/shared-data/Scripts/generic/dataSourcing/get_geoSpatialData.R", jobs=10)

trial_soil_ML <- extract_geoSpatialPointData(country = country, inputData=inputDataTrial, varsbasePath=varsbasePath, pathOut= pathOut,
                                             AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, soilData = TRUE, 
                                             weatherData = FALSE, soilProfile = FALSE, season = 1, jobs =10)




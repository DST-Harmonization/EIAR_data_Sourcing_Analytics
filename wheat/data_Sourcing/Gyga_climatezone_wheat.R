

source("~/shared-data/Scripts/generic/Gyga_WLY/Gyga_climatezone.R")
#####################################################################################################################################
## GYGA WLY for the three crops by CLIMATEZONE, CROP, HARVESTYEAR 
## VIP: not all climate zones in Ethiopia have estimated WLY in Gyga and that poses a problem in using this data
## https://www.yieldgap.org/web/guest/climate-zones#_ENREF_8
## https://www.yieldgap.org/gygaviewer/index.html?extended=1
wheat <- read.csv("~/shared-data/Data/General/Gyga/GygaEthiopiaRainfedWheat.csv")
head(wheat)
unique(wheat$CLIMATEZONE)

wheat_agg <- Scenaio_agg(df=wheat)
head(wheat_agg)


#####################################################################################################################################
## attach the model ready field data to get the WLY: 
#####################################################################################################################################
## 1. get the CLIMATEZONE by GPS
climatezones <- terra::rast("~/shared-data/Data/General/Gyga/GYGA_ED.tif")
countryShp <- geodata::gadm("Ethiopia", level = 2, path='.')
plot(crop(climatezones, countryShp))

wheat_modelReady <- readRDS("~/shared-data/Data/Wheat/fieldData/wheat_modelReady.rds")
wheat_modelReady <- wheat_modelReady %>% dplyr::select(-c("WLY_dry", "WLY_neutral",  "WLY_wet", "CLIMATEZONE"))

wheatGPS <- wheat_modelReady %>% dplyr::select(long2, lat2) %>% dplyr::rename(x = long2, y=lat2) %>% unique()
wheat_climatezone <- raster::extract(climatezones, wheatGPS)

wheat_climatezone <- wheat_climatezone %>% 
  dplyr::mutate(lon=wheatGPS$x, lat=wheatGPS$y) %>% 
  dplyr::select(lon, lat, GYGA_ED) %>% 
  dplyr::rename(CLIMATEZONE = GYGA_ED)

wheat_modelReady <- merge(wheat_modelReady, wheat_climatezone, by.x=c("long2", "lat2"), by.y=c("lon","lat"), all.x=TRUE)
unique(wheat_modelReady$CLIMATEZONE) ## these are much more than the CLIMATEZONE with WLY that we get from Gyga

## 2. add the WLY by climate scenario using the the CLIMATEZONE
wheat_modelReady2 <- wheat_modelReady %>% inner_join(wheat_agg)
unique(wheat_modelReady2$CLIMATEZONE)
unique(wheat_modelReady$CLIMATEZONE)


wheat_modelReady_noWLY <- wheat_modelReady %>% dplyr::filter(!trial_id %in% unique(wheat_modelReady2$trial_id))
wheat_modelReady_noWLY$COUNTRY <-  unique(wheat_modelReady2$COUNTRY)

## NOTE: not all CLIMATEZONE have WLY
## options to fix this are:: assign the closest neighbor WLY or get the median value within a NAME-2
## the closest neighbor can be east and west it does not work, and the there is no always data from the same NAME_2, when tehre is we wiill use the 
## median or NAME_2, otherwise national median value for missing data

get_missing_WLY <- NULL 
for (tid in unique(wheat_modelReady_noWLY$trial_id)){
  tdata <- droplevels(wheat_modelReady_noWLY[wheat_modelReady_noWLY$trial_id ==tid, ])
  pdata <- droplevels(wheat_modelReady2[wheat_modelReady2$NAME_2 %in%unique(tdata$NAME_2), ])
  
  if(nrow(pdata)>0){
    print(unique(tdata$NAME_2))
    tdata$WLY_dry <- median(pdata$WLY_dry)
    tdata$WLY_neutral <- median(pdata$WLY_neutral)
    tdata$WLY_wet  <- median(pdata$WLY_wet)
  }else{
    tdata$WLY_dry <- median(wheat_modelReady2$WLY_dry)
    tdata$WLY_neutral <- median(wheat_modelReady2$WLY_neutral)
    tdata$WLY_wet  <- median(wheat_modelReady2$WLY_wet)
  }
  get_missing_WLY <- rbind(get_missing_WLY, tdata)
}

nrow(wheat_modelReady)
wheat_modelReady <- rbind(wheat_modelReady2, get_missing_WLY)

saveRDS(wheat_modelReady, "~/shared-data/Data/Wheat/fieldData/wheat_modelReady.rds")











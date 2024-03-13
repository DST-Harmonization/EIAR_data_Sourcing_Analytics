source("~/shared-data/Scripts/generic/Gyga_WLY/Gyga_climatezone.R")
#####################################################################################################################################
## GYGA WLY for by CLIMATEZONE, CROP, HARVESTYEAR 
## VIP: not all climate zones in Ethiopia have estimated WLY in Gyga and that poses a problem in using this data
sorghum <- read.csv("~/shared-data/Data/General/Gyga/GygaEthiopiaRainfedSorghum.csv")
sorghum_agg <- Scenaio_agg(df=sorghum)

head(sorghum_agg)
head(sorghum)
unique(sorghum$CLIMATEZONE)

#####################################################################################################################################
## attach the model ready field data to get the WLY: 
#####################################################################################################################################
## 1. get the CLIMATEZONE by GPS
climatezones <- terra::rast("~/shared-data/Data/General/Gyga/GYGA_ED.tif")
countryShp <- geodata::gadm("Ethiopia", level = 2, path='.')
plot(crop(climatezones, countryShp))

sorghum_modelReady <- readRDS("~/shared-data/Data/Sorghum/fieldData/sorghum_modelReady.rds")
sorghum_modelReady <- sorghum_modelReady %>% 
  dplyr::rename(trial_id = trail_id)

sorghumGPS <- sorghum_modelReady %>% 
  dplyr::select(Long, Lat) %>% 
  dplyr::rename(x = Long, y=Lat) %>%
  unique()

sorghum_climatezone <- raster::extract(climatezones, sorghumGPS)

sorghum_climatezone <- sorghum_climatezone %>% 
  dplyr::mutate(lon=sorghumGPS$x, lat=sorghumGPS$y) %>% 
  dplyr::select(lon, lat, GYGA_ED) %>% 
  dplyr::rename(CLIMATEZONE = GYGA_ED)

sorghum_modelReady <- merge(sorghum_modelReady, sorghum_climatezone, by.x=c("Long", "Lat"), by.y=c("lon","lat"), all.x=TRUE)
unique(sorghum_modelReady$CLIMATEZONE) ## these are much more than the CLIMATEZONE with WLY that we get from Gyga

## 2. add the WLY by climate scenario using the the CLIMATEZONE
sorghum_modelReady2 <- sorghum_modelReady %>% inner_join(sorghum_agg)
nrow(sorghum_modelReady)
nrow(sorghum_modelReady2)

sorghum_modelReady_noWLY <- sorghum_modelReady %>% dplyr::filter(!trial_id %in% unique(sorghum_modelReady2$trial_id))
sorghum_modelReady_noWLY$COUNTRY <-  unique(sorghum_modelReady2$COUNTRY)

## NOTE: not all CLIMATEZONE have WLY
## options to fix this are:: assign the closest neighbor WLY or get the median value within a NAME-2
## the closest neighbor can be east and west it does not work, and the there is no always data from the same NAME_2, when tehre is we wiill use the 
## median or NAME_2, otherwise national median value for missing data

get_missing_WLY <- NULL 
for (tid in unique(sorghum_modelReady_noWLY$trial_id)){
  tdata <- droplevels(sorghum_modelReady_noWLY[sorghum_modelReady_noWLY$trial_id ==tid, ])
  pdata <- droplevels(sorghum_modelReady2[sorghum_modelReady2$NAME_2 %in%unique(tdata$NAME_2), ])
  
  if(nrow(pdata)>0){
    print(unique(tdata$NAME_2))
    tdata$WLY_dry <- median(pdata$WLY_dry)
    tdata$WLY_neutral <- median(pdata$WLY_neutral)
    tdata$WLY_wet  <- median(pdata$WLY_wet)
  }else{
    tdata$WLY_dry <- median(sorghum_modelReady2$WLY_dry)
    tdata$WLY_neutral <- median(sorghum_modelReady2$WLY_neutral)
    tdata$WLY_wet  <- median(sorghum_modelReady2$WLY_wet)
  }
  get_missing_WLY <- rbind(get_missing_WLY, tdata)
}

nrow(sorghum_modelReady)
sorghum_modelReady <- rbind(sorghum_modelReady2, get_missing_WLY)

saveRDS(sorghum_modelReady, "~/shared-data/Data/Sorghum/fieldData/sorghum_modelReady.rds")



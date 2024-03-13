

source("~/shared-data/Scripts/generic/Gyga_WLY/Gyga_climatezone.R")
#####################################################################################################################################
## GYGA WLY for the three crops by CLIMATEZONE, CROP, HARVESTYEAR 
## VIP: not all climate zones in Ethiopia have estimated WLY in Gyga and that poses a problem in using this data
maize <- read.csv("~/shared-data/Data/General/Gyga/GygaEthiopiaRainfedMaize.csv")
head(maize)
unique(maize$CLIMATEZONE)

maize_agg <- Scenaio_agg(df=maize)
head(maize_agg)


#####################################################################################################################################
## attach the model ready field data to get the WLY: 
#####################################################################################################################################
## 1. get the CLIMATEZONE by GPS
climatezones <- terra::rast("~/shared-data/Data/General/Gyga/GYGA_ED.tif")
countryShp <- geodata::gadm("Ethiopia", level = 2, path='.')
plot(crop(climatezones, countryShp))

maize_modelReady <- readRDS("~/shared-data/Data/Maize/fieldData/maize_modelReady.rds")
maizeGPS <- maize_modelReady %>% dplyr::select(long2, lat2) %>% dplyr::rename(x = long2, y=lat2) %>% unique()
maize_climatezone <- raster::extract(climatezones, maizeGPS)

maize_climatezone <- maize_climatezone %>% 
  dplyr::mutate(lon=maizeGPS$x, lat=maizeGPS$y) %>% 
  dplyr::select(lon, lat, GYGA_ED) %>% 
  dplyr::rename(CLIMATEZONE = GYGA_ED)

maize_modelReady <- merge(maize_modelReady, maize_climatezone, by.x=c("long2", "lat2"), by.y=c("lon","lat"), all.x=TRUE)
unique(maize_modelReady$CLIMATEZONE) ## these are much more than the CLIMATEZONE with WLY that we get from Gyga

## 2. add the WLY by climate scenario using the the CLIMATEZONE
maize_modelReady2 <- maize_modelReady %>% inner_join(maize_agg)
unique(maize_modelReady$CLIMATEZONE) 

maize_modelReady_noWLY <- maize_modelReady %>% dplyr::filter(!trial_id %in% unique(maize_modelReady2$trial_id))
maize_modelReady_noWLY$COUNTRY <-  unique(maize_modelReady2$COUNTRY)

## NOTE: not all CLIMATEZONE have WLY
## options to fix this are:: assign the closest neighbor WLY or get the median value within a NAME-2
## the closest neighbor can be east and west it does not work, and the there is no always data from the same NAME_2, when tehre is we wiill use the 
## median or NAME_2, otherwise national median value for missing data

get_missing_WLY <- NULL 
for (tid in unique(maize_modelReady_noWLY$trial_id)){
  tdata <- droplevels(maize_modelReady_noWLY[maize_modelReady_noWLY$trial_id ==tid, ])
  pdata <- droplevels(maize_modelReady2[maize_modelReady2$NAME_2 %in%unique(tdata$NAME_2), ])
  
  if(nrow(pdata)>0){
    print(unique(tdata$NAME_2))
    tdata$WLY_dry <- median(pdata$WLY_dry)
    tdata$WLY_neutral <- median(pdata$WLY_neutral)
    tdata$WLY_wet  <- median(pdata$WLY_wet)
  }else{
    tdata$WLY_dry <- median(maize_modelReady2$WLY_dry)
    tdata$WLY_neutral <- median(maize_modelReady2$WLY_neutral)
    tdata$WLY_wet  <- median(maize_modelReady2$WLY_wet)
  }
  get_missing_WLY <- rbind(get_missing_WLY, tdata)
}

nrow(maize_modelReady)
maize_modelReady <- rbind(maize_modelReady2, get_missing_WLY)

saveRDS(maize_modelReady, "~/shared-data/Data/Maize/fieldData/maize_modelReady.rds")




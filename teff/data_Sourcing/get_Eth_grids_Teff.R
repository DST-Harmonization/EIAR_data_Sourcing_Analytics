

## read the crop mask, the planting and growing length rasters 
tefMask <- terra::rast("~/shared-data/Data/General/cropMask/teff.tif")
tefGrowing <- terra::rast("~/shared-data/Data/General/growingLength/teff.tif")
tefPlanting <- terra::rast("~/shared-data/Data/General/sowingDate/teff.tif")


## get pldate and teh key coded as decades 
PlD_date <- as.data.frame(read_csv("~/shared-data/Data/General/sowingDate/PlDate.csv"))
head(PlD_date)
PlD_date$Zone2 <- paste(PlD_date$NAME_1, PlD_date$NAME_2, sep="_")
PlD_date <- PlD_date[!is.na(PlD_date$Zone2),]
unique(PlD_date$Zone2)
Decade_dates <- as.data.frame(read_csv("~/shared-data/Data/General/Decade_dates.csv", show_col_types = FALSE))
Decade_dates <- Decade_dates[!is.na(Decade_dates$Decad), ]

## planting date by crop and decades tnaslated to month and date
PlD_tef <- PlD_date[, c("NAME_1","Zone2", "Teff_Meher")]
PlD_tef <- droplevels(PlD_tef[!is.na(PlD_tef$Teff_Meher), ])
PlD_tef <- merge(PlD_tef, Decade_dates[, c("Decad","start", "month", "date" )], by.x="Teff_Meher", by.y="Decad")
PlD_tef <- PlD_tef %>% 
  select(!(Teff_Meher)) %>%
  dplyr::rename( Zone = Zone2, 
                 planting_month_date = start,
                 plMonth =month, plDate =date)


## get length of growing period in weeks ??
LGP <-as.data.frame(read_csv("~/shared-data/Data/General/growingLength/LGP.csv"))
LGP$Zone <- paste(LGP$NAME_1, LGP$NAME_2, sep="_")
LGP_tef <- LGP %>% 
  select(Zone, Teff_Meher) %>%
  dplyr::rename(HvWeeks = Teff_Meher)
head(LGP_tef)
PlD_tef <- merge(PlD_tef, LGP_tef, by="Zone")


## get harvest month and week, 
PlD_tef$pld <- as.Date(paste(2024, PlD_tef$planting_month_date, sep="-"))
PlD_tef$hvd <- PlD_tef$pld + PlD_tef$HvWeeks*10
PlD_tef$hvMonth <- format(as.Date(PlD_tef$hvd, format="%Y-%m-%d"),"%m")
PlD_tef$hvDate <- format(as.Date(PlD_tef$hvd, format="%Y-%m-%d"),"%d")
PlD_tef$harvest_month_date <- paste(PlD_tef$hvMonth, PlD_tef$hvDate, sep="-")


## for setting the initial soil condition the weather data prior to planting need to be available for that the data sourcing scipt takes care of that
PlD_tef$plMonth <- format(as.Date(PlD_tef$pld, format="%Y-%m-%d"),"%m")
PlD_tef$plDate <- format(as.Date(PlD_tef$pld, format="%Y-%m-%d"),"%d")
PlD_tef$planting_month_date <- paste(PlD_tef$plMonth, PlD_tef$plDate, sep="-")
head(PlD_tef)

saveRDS(PlD_tef, "~/shared-data/Data/Tef/geoSpatial/Tef_pLHv_byZone.RDS")

## define the use case parameters 
Crop  <- "Tef"
useCaseName <- "harmonizeDST"
resltn <- 0.05
country <- "Ethiopia"
countryShp <- geodata::gadm(country = "Ethiopia", level = 2, path='.')

## create regular grid, and merge
xmin <- ext(tefMask)[1]
xmax <- ext(tefMask)[2]
ymin <- ext(tefMask)[3]
ymax <- ext(tefMask)[4]

lon_coors <- unique(round(seq(xmin - 0.1, xmax + 0.1, by=resltn), digits=3))
lat_coors <- unique(round(seq(ymin - 0.1, ymax + 0.1, by=resltn), digits=3))
rect_coord <- as.data.frame(expand.grid(x = lon_coors, y = lat_coors))


if(resltn == 0.05){
  rect_coord$x <- floor(rect_coord$x*10)/10 + ifelse(rect_coord$x - (floor(rect_coord$x*10)/10) < 0.05, 0.025, 0.075)
  rect_coord$y <- floor(rect_coord$y*10)/10 + ifelse(abs(rect_coord$y)-(floor(abs(rect_coord$y)*10)/10) < 0.05, 0.025, 0.075)
}
rect_coord <- unique(rect_coord[,c("x", "y")])

State_LGA <- as.data.frame(raster::extract(countryShp, rect_coord))
tefArea <- as.data.frame(raster::extract(tefMask, rect_coord))
State_LGA$Zone <- paste(State_LGA$NAME_1, State_LGA$NAME_2, sep="_")
unique(State_LGA$Zone)


State_LGA$lon <- rect_coord$x
State_LGA$lat <- rect_coord$y
State_LGA$country <- country
State_LGA$tefregion <- tefArea$tef
State_LGA <- droplevels(State_LGA[!is.na(State_LGA$tefregion), ])
plot(State_LGA$lon, State_LGA$lat)
State_LGA <- unique(State_LGA[, c("country", "Zone", "NAME_1", "NAME_2", "lon", "lat")])
head(State_LGA)

State_LGA2 <- merge(State_LGA, PlD_tef, by="Zone")
head(State_LGA2)
### planting and harvset dates are unique by zone, so data shuld be sourced by zone

saveRDS(State_LGA2,"~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ethiopia_harmonizeDST/Tef/raw/tef_AOI.RDS")

####################################################################################################
####################################################################################################
####################################################################################################

### creating the shape file for plating dates, altitude and length of growing season
library(raster)
library(sp)




setwd("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ethiopia_harmonizeDST/Tef/raw")

listRaster_dem <- raster("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ethiopia_harmonizeDST/ETH_dem.tif")
Tef <- readRDS("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ethiopia_harmonizeDST/Tef/raw/tef_AOI.RDS")
Tef <- Tef %>%
  dplyr::select(-HvWeeks, -pld, -hvd, -planting_month_date, -harvest_month_date)
head(Tef)

rect_coord <- Tef[,c("lon", "lat")]
names(rect_coord) <- c('x', 'y')
elevation <- as.data.frame(raster::extract(listRaster_dem, rect_coord))
Tef$elevation <- elevation$`raster::extract(listRaster_dem, rect_coord)`

Tef_vect <- vect(Tef, geom = c('lon', 'lat'), crs = 'EPSG:4326') 
class(Tef_vect)

eth_zone <- geodata::gadm(country="Ethiopia", level = 2, path='.')

plot(eth_zone)
plot(Tef_vect, add = T)
head(eth_zone)
dim(eth_zone)

#joining
colnames(Tef)
dim(Tef)
names(eth_zone)

sp_df <- merge(eth_zone, Tef, all.x=TRUE, by.x=c('NAME_1', 'NAME_2'), by.y=c('NAME_1', 'NAME_2'))
class(sp_df)
head(sp_df)
plot(sp_df)

sp_df <- sp_df[!is.na(sp_df$lon)]
head(sp_df)


writeVector(sp_df, "Tef_zone_pl_hv_date.shp", "ESRI Shapefile", overwrite=TRUE)

require(rgdal)
Tef_PlHv <- readOGR(dsn = ".", layer = "Tef_zone_pl_hv_date")
plot(Tef_PlHv)
names(Tef_PlHv)

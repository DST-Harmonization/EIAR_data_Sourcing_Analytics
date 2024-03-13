
## read the crop mask, the planting and growing length rasters 
sorghumMask <- terra::rast("~/shared-data/Data/General/cropMask/sorghum.tif")
sorghumGrowing <- terra::rast("~/shared-data/Data/General/growingLength/sorghum.tif")
sorghumPlanting <- terra::rast("~/shared-data/Data/General/sowingDate/sorghum.tif")


## get pldate and the key coded as decades 
PlD_date <- as.data.frame(read_csv("~/shared-data/Data/General/sowingDate/PlDate.csv"))
head(PlD_date)
PlD_date$Zone2 <- paste(PlD_date$NAME_1, PlD_date$NAME_2, sep="_")
PlD_date <- PlD_date[!is.na(PlD_date$Zone2),]
unique(PlD_date$Zone2)
Decade_dates <- as.data.frame(read_csv("~/shared-data/Data/General/Decade_dates.csv", show_col_types = FALSE))
Decade_dates <- Decade_dates[!is.na(Decade_dates$Decad), ]

## planting date by crop and decades tnaslated to month and date
PlD_sorghum <- PlD_date[, c("NAME_1","Zone2", "Sorghum_Meher")]
PlD_sorghum <- droplevels(PlD_sorghum[!is.na(PlD_sorghum$Sorghum_Meher), ])
PlD_sorghum <- merge(PlD_sorghum, Decade_dates[, c("Decad","start", "month", "date" )], by.x="Sorghum_Meher", by.y="Decad")
PlD_sorghum <- PlD_sorghum %>% 
  select(!(Sorghum_Meher)) %>%
  dplyr::rename( Zone = Zone2, 
                 planting_month_date = start,
                 plMonth =month, plDate =date)


## get length of growing period in weeks ??
LGP <- as.data.frame(read_csv("~/shared-data/Data/General/growingLength/LGP.csv"))
LGP$Zone <- paste(LGP$NAME_1, LGP$NAME_2, sep="_")
LGP_sorghum <- LGP %>% 
  select(Zone, Sorghum_Meher) %>%
  dplyr::rename(HvWeeks = Sorghum_Meher)
head(LGP_sorghum)
PlD_sorghum <- merge(PlD_sorghum, LGP_sorghum, by="Zone")
head(PlD_sorghum)

## get harvest month and week, 
PlD_sorghum$pld <- as.Date(paste(2024, PlD_sorghum$planting_month_date, sep="-"))
PlD_sorghum$hvd <- PlD_sorghum$pld + PlD_sorghum$HvWeeks*10
PlD_sorghum$hvMonth <- format(as.Date(PlD_sorghum$hvd, format="%Y-%m-%d"),"%m")
PlD_sorghum$hvDate <- format(as.Date(PlD_sorghum$hvd, format="%Y-%m-%d"),"%d")
PlD_sorghum$harvest_month_date <- paste(PlD_sorghum$hvMonth, PlD_sorghum$hvDate, sep="-")


## for setting the initial soil condition the weather data prior to planting need to be available for that the data sourcing scipt takes care of that
PlD_sorghum$plMonth <- format(as.Date(PlD_sorghum$pld, format="%Y-%m-%d"),"%m")
PlD_sorghum$plDate <- format(as.Date(PlD_sorghum$pld, format="%Y-%m-%d"),"%d")
PlD_sorghum$planting_month_date <- paste(PlD_sorghum$plMonth, PlD_sorghum$plDate, sep="-")
head(PlD_sorghum)
saveRDS(PlD_sorghum, "~/shared-data/Data/Sorghum/geoSpatial/Sorghum_pLHv_byZone.RDS")




## define the use case parameters 
Crop  <- "Sorghum"
useCaseName <- "harmonizeDST"
resltn <- 0.05
country <- "Ethiopia"
countryShp <- geodata::gadm(country = "Ethiopia", level = 2, path='.')

## create regular grid, and merge
xmin <- ext(sorghumMask)[1]
xmax <- ext(sorghumMask)[2]
ymin <- ext(sorghumMask)[3]
ymax <- ext(sorghumMask)[4]

lon_coors <- unique(round(seq(xmin - 0.1, xmax + 0.1, by=resltn), digits=3))
lat_coors <- unique(round(seq(ymin - 0.1, ymax + 0.1, by=resltn), digits=3))
rect_coord <- as.data.frame(expand.grid(x = lon_coors, y = lat_coors))


if(resltn == 0.05){
  rect_coord$x <- floor(rect_coord$x*10)/10 + ifelse(rect_coord$x - (floor(rect_coord$x*10)/10) < 0.05, 0.025, 0.075)
  rect_coord$y <- floor(rect_coord$y*10)/10 + ifelse(abs(rect_coord$y)-(floor(abs(rect_coord$y)*10)/10) < 0.05, 0.025, 0.075)
}
rect_coord <- unique(rect_coord[,c("x", "y")])

State_LGA <- as.data.frame(raster::extract(countryShp, rect_coord))
sorghumArea <- as.data.frame(raster::extract(sorghumMask, rect_coord))
State_LGA$Zone <- paste(State_LGA$NAME_1, State_LGA$NAME_2, sep="_")
unique(State_LGA$Zone)


State_LGA$lon <- rect_coord$x
State_LGA$lat <- rect_coord$y
State_LGA$country <- country
State_LGA$sorghumregion <- sorghumArea$sorghum
State_LGA <- droplevels(State_LGA[!is.na(State_LGA$sorghumregion), ])
plot(State_LGA$lon, State_LGA$lat)
State_LGA <- unique(State_LGA[, c("country", "Zone", "NAME_1", "NAME_2", "lon", "lat")])
head(State_LGA)

State_LGA2 <- merge(State_LGA, PlD_sorghum, by="Zone")
head(State_LGA2)
### planting and harvset dates are unique by zone, so data shuld be sourced by zone

saveRDS(State_LGA2,"~/shared-data/Data/Sorghum/geoSpatial/Wheat_AOI.RDS")

####################################################################################################
####################################################################################################

### creating the shape file for plating dates, altitude and length of growing season
library(raster)
library(sp)
setwd("~/shared-data/Data/Sorghum/geoSpatial")
eth_zone <- geodata::gadm(country="Ethiopia", level = 2, path='.')
countryExt <- terra::ext(eth_zone)

# ## if the extent is not fully within a distince of 5 degrees this does not work, otherwise this would have been better script
listRaster_dem1 <-geodata::elevation_3s(lon=countryExt[1], lat=countryExt[3], path=getwd()) #xmin - ymin
listRaster_dem2 <-geodata::elevation_3s(lon=countryExt[1], lat=countryExt[4], path=getwd()) #xmin - ymax
listRaster_dem3 <-geodata::elevation_3s(lon=countryExt[2], lat=countryExt[3], path=getwd()) #xmax - ymin
listRaster_dem4 <-geodata::elevation_3s(lon=countryExt[2], lat=countryExt[4], path=getwd()) #xmax - ymax
listRaster_dem5 <-geodata::elevation_3s(lon=32, lat=7, path=getwd()) #xmax - ymax
listRaster_dem6 <-geodata::elevation_3s(lon=37, lat=2, path=getwd()) #xmax - ymax
listRaster_dem7 <-geodata::elevation_3s(lon=37, lat=7, path=getwd()) #xmax - ymax
listRaster_dem8 <-geodata::elevation_3s(lon=37, lat=12, path=getwd()) #xmax - ymax
listRaster_dem9 <-geodata::elevation_3s(lon=42, lat=2, path=getwd()) #xmax - ymax
listRaster_dem10 <-geodata::elevation_3s(lon=42, lat=7, path=getwd()) #xmax - ymax
listRaster_dem11 <-geodata::elevation_3s(lon=42, lat=12, path=getwd()) #xmax - ymax
listRaster_dem12 <-geodata::elevation_3s(lon=47, lat=7, path=getwd()) #xmax - ymax
listRaster_dem <- terra::mosaic(listRaster_dem1, listRaster_dem2, listRaster_dem3, listRaster_dem4, 
                                listRaster_dem5, listRaster_dem6, listRaster_dem7, listRaster_dem8,
                                listRaster_dem9, listRaster_dem10, listRaster_dem11, listRaster_dem12,
                                fun='mean')

plot(listRaster_dem)
#writeRaster(listRaster_dem, "ETH_dem.shp", format="ESRI Shapefile")

#writeRaster(listRaster_dem, filename = "ETHdem.tif", format = "GTiff", overwrite = TRUE)
#writeRaster(listRaster_dem, filename = "ETHdem.tif", overwrite = TRUE)

ETHdem <- raster("~/shared-data/Data/Sorghum/geoSpatial/ETH_dem.tif")



sorghum <- readRDS("~/shared-data/Data/Sorghum/geoSpatial/sorghum_AOI.RDS")
sorghum <- sorghum %>% dplyr::select(-HvWeeks, -pld, -hvd, -planting_month_date, -harvest_month_date)
head(sorghum)

sorghum_vect <- vect(sorghum, geom = c('lon', 'lat'), crs = 'EPSG:4326') 
class(sorghum_vect)

sorghum_elev <- listRaster_dem |> terra::extract(sorghum_vect)
unique(sorghum_elev[,2])


sorghum2 <- sorghum |> cbind(sorghum_elev[,-1])
colnames(sorghum2)
colnames(sorghum2)[11] <- "elevation"
head(sorghum2)


plot(eth_zone)
plot(sorghum_vect, add = T)
head(eth_zone)
dim(eth_zone)

#joining
colnames(sorghum)
dim(sorghum)
names(eth_zone)

sp_df <- merge(eth_zone, sorghum2, all.x=TRUE, by.x=c('NAME_1', 'NAME_2'), by.y=c('NAME_1', 'NAME_2'))
class(sp_df)
head(sp_df)
plot(sp_df)

sp_df <- sp_df[!is.na(sp_df$lon)]



writeVector(sp_df, "sorghum_zone_pl_hv_date.shp", "ESRI Shapefile", overwrite=TRUE)

require(rgdal)
sorghum_PlHv <- readOGR(dsn = ".", layer = "sorghum_zone_pl_hv_date")
plot(sorghum_PlHv)
names(sorghum_PlHv)


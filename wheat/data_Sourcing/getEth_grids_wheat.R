
## for setting the initial soil condition the weather data prior to planting need to be available and for that the data sourcing script takes care of that
PlD_wheat$plMonth <- format(as.Date(PlD_wheat$pld, format="%Y-%m-%d"),"%m")
PlD_wheat$plDate <- format(as.Date(PlD_wheat$pld, format="%Y-%m-%d"),"%d")
PlD_wheat$planting_month_date <- paste(PlD_wheat$plMonth, PlD_wheat$plDate, sep="-")
head(PlD_wheat)

saveRDS(PlD_wheat, "~/shared-data/Data/Wheat/geoSpatial/Wheat_pLHv_byZone.RDS")


## define the use case parameters 
Crop  <- "Wheat"
resltn <- 0.05
country <- "Ethiopia"
countryShp <- geodata::gadm(country = "Ethiopia", level = 2, path='.')

## create regular grid, and merge
xmin <- ext(wheatMask)[1]
xmax <- ext(wheatMask)[2]
ymin <- ext(wheatMask)[3]
ymax <- ext(wheatMask)[4]

lon_coors <- unique(round(seq(xmin - 0.1, xmax + 0.1, by=resltn), digits=3))
lat_coors <- unique(round(seq(ymin - 0.1, ymax + 0.1, by=resltn), digits=3))
rect_coord <- as.data.frame(expand.grid(x = lon_coors, y = lat_coors))


if(resltn == 0.05){
  rect_coord$x <- floor(rect_coord$x*10)/10 + ifelse(rect_coord$x - (floor(rect_coord$x*10)/10) < 0.05, 0.025, 0.075)
  rect_coord$y <- floor(rect_coord$y*10)/10 + ifelse(abs(rect_coord$y)-(floor(abs(rect_coord$y)*10)/10) < 0.05, 0.025, 0.075)
}
rect_coord <- unique(rect_coord[,c("x", "y")])

State_LGA <- as.data.frame(raster::extract(countryShp, rect_coord))
wheatArea <- as.data.frame(raster::extract(wheatMask, rect_coord))
State_LGA$Zone <- paste(State_LGA$NAME_1, State_LGA$NAME_2, sep="_")
unique(State_LGA$Zone)


State_LGA$lon <- rect_coord$x
State_LGA$lat <- rect_coord$y
State_LGA$country <- country
State_LGA$wheatregion <- wheatArea$wheat
State_LGA <- droplevels(State_LGA[!is.na(State_LGA$wheatregion), ])
plot(State_LGA$lon, State_LGA$lat)
State_LGA <- unique(State_LGA[, c("country", "Zone", "NAME_1", "NAME_2", "lon", "lat")])
head(State_LGA)

State_LGA2 <- merge(State_LGA, PlD_wheat, by="Zone")
head(State_LGA2)
### planting and harvset dates are unique by zone, so data should be sourced by zone

saveRDS(State_LGA2,"~/shared-data/Data/Wheat/geoSpatial/Wheat_AOI.RDS")





####################################################################################################
####################################################################################################

### creating the shape file for plating dates, altitude and length of growing season
library(raster)
library(sp)
setwd("~/shared-data/Data/Wheat/geoSpatial")

listRaster_dem <- raster("~/shared-data/Data/General/Global_GeoData/Landing/ETH_dem.tif")
wheat <- readRDS("~/shared-data/Data/Wheat/geoSpatial/Wheat_AOI.RDS")
wheat <- wheat %>%
  dplyr::select(-HvWeeks, -pld, -hvd, -planting_month_date, -harvest_month_date)
head(wheat)

rect_coord <- wheat[,c("lon", "lat")]
names(rect_coord) <- c('x', 'y')
elevation <- as.data.frame(raster::extract(listRaster_dem, rect_coord))
wheat$elevation <- elevation$`raster::extract(listRaster_dem, rect_coord)`

wheat_vect <- vect(wheat, geom = c('lon', 'lat'), crs = 'EPSG:4326') 
class(wheat_vect)

eth_zone <- geodata::gadm(country="Ethiopia", level = 2, path='.')

plot(eth_zone)
plot(wheat_vect, add = T)
head(eth_zone)
dim(eth_zone)

#joining
colnames(wheat)
dim(wheat)
names(eth_zone)

sp_df <- merge(eth_zone, wheat, all.x=TRUE, by.x=c('NAME_1', 'NAME_2'), by.y=c('NAME_1', 'NAME_2'))
class(sp_df)
head(sp_df)
plot(sp_df)

sp_df <- sp_df[!is.na(sp_df$lon)]
head(sp_df)


writeVector(sp_df, "~/shared-data/Data/Wheat/geoSpatial/wheat_zone_pl_hv_date.shp", "ESRI Shapefile", overwrite=TRUE)

require(rgdal)
wheat_PlHv <- readOGR(dsn = ".", layer = "~/shared-data/Data/Wheat/geoSpatial/wheat_zone_pl_hv_date")
plot(wheat_PlHv)
names(wheat_PlHv)



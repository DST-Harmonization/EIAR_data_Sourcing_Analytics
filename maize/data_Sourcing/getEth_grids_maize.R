
## read the crop mask, the planting and growing length rasters 
maizeMask <- terra::rast("~/shared-data/Data/General/cropMask/maize.tif")
maizeGrowing <- terra::rast("~/shared-data/Data/General/growingLength/maize.tif")
maizePlanting <- terra::rast("~/shared-data/Data/General/sowingDate/maize.tif")


## get pldate and teh key coded as decades 
PlD_date <- as.data.frame(read_csv("~/shared-data/Data/General/sowingDate/PlDate.csv"))
head(PlD_date)
PlD_date$Zone2 <- paste(PlD_date$NAME_1, PlD_date$NAME_2, sep="_")
PlD_date <- PlD_date[!is.na(PlD_date$Zone2),]
unique(PlD_date$Zone2)
Decade_dates <- as.data.frame(read_csv("~/shared-data/Data/General/Decade_dates.csv", show_col_types = FALSE))
Decade_dates <- Decade_dates[!is.na(Decade_dates$Decad), ]

## planting date by crop and decades tnaslated to month and date
PlD_Maize <- PlD_date[, c("NAME_1","Zone2", "Maize_Meher")]
PlD_Maize <- droplevels(PlD_Maize[!is.na(PlD_Maize$Maize_Meher), ])
PlD_Maize <- merge(PlD_Maize, Decade_dates[, c("Decad","start", "month", "date" )], by.x="Maize_Meher", by.y="Decad")
PlD_Maize <- PlD_Maize %>% 
  select(!(Maize_Meher)) %>%
  dplyr::rename( Zone = Zone2, 
                 planting_month_date = start,
                 plMonth =month, plDate =date)


## get length of growing period in weeks ??
LGP <- as.data.frame(read_csv("~/shared-data/Data/General/growingLength/LGP.csv"))
LGP$Zone <- paste(LGP$NAME_1, LGP$NAME_2, sep="_")
LGP_Maize <- LGP %>% 
  select(Zone, Maize_Meher) %>%
  dplyr::rename(HvWeeks = Maize_Meher)
head(LGP_Maize)
sort(unique(LGP_Maize$HvWeeks))
PlD_Maize <- merge(PlD_Maize, LGP_Maize, by="Zone")
head(PlD_Maize)

## get harvest month and week, 
PlD_Maize$pld <- as.Date(paste(2024, PlD_Maize$planting_month_date, sep="-"))
PlD_Maize$hvd <- PlD_Maize$pld + PlD_Maize$HvWeeks*10
PlD_Maize$hvMonth <- format(as.Date(PlD_Maize$hvd, format="%Y-%m-%d"),"%m")
PlD_Maize$hvDate <- format(as.Date(PlD_Maize$hvd, format="%Y-%m-%d"),"%d")
PlD_Maize$harvest_month_date <- paste(PlD_Maize$hvMonth, PlD_Maize$hvDate, sep="-")

## for setting the initial soil condition the weather data prior to planting need to be available and for that we take a month earlier
# PlD_Maize$pld <- PlD_Maize$pld - 30
PlD_Maize$plMonth <- format(as.Date(PlD_Maize$pld, format="%Y-%m-%d"),"%m")
PlD_Maize$plDate <- format(as.Date(PlD_Maize$pld, format="%Y-%m-%d"),"%d")
PlD_Maize$planting_month_date <- paste(PlD_Maize$plMonth, PlD_Maize$plDate, sep="-")
str(PlD_Maize)
saveRDS(PlD_Maize, "~/shared-data/Data/Maize/geoSpatial/Maize_pLHv_byZone.RDS")




## define the use case parameters 
Crop  <- "Maize"
useCaseName <- "harmonizeDST"
resltn <- 0.05
country <- "Ethiopia"
countryShp <- geodata::gadm(country = "Ethiopia", level = 2, path='.')

## create regular grid, and merge
xmin <- ext(maizeMask)[1]
xmax <- ext(maizeMask)[2]
ymin <- ext(maizeMask)[3]
ymax <- ext(maizeMask)[4]

lon_coors <- unique(round(seq(xmin - 0.1, xmax + 0.1, by=resltn), digits=3))
lat_coors <- unique(round(seq(ymin - 0.1, ymax + 0.1, by=resltn), digits=3))
rect_coord <- as.data.frame(expand.grid(x = lon_coors, y = lat_coors))


if(resltn == 0.05){
  rect_coord$x <- floor(rect_coord$x*10)/10 + ifelse(rect_coord$x - (floor(rect_coord$x*10)/10) < 0.05, 0.025, 0.075)
  rect_coord$y <- floor(rect_coord$y*10)/10 + ifelse(abs(rect_coord$y)-(floor(abs(rect_coord$y)*10)/10) < 0.05, 0.025, 0.075)
}
rect_coord <- unique(rect_coord[,c("x", "y")])

State_LGA <- as.data.frame(raster::extract(countryShp, rect_coord))
maizeArea <- as.data.frame(raster::extract(maizeMask, rect_coord))
State_LGA$Zone <- paste(State_LGA$NAME_1, State_LGA$NAME_2, sep="_")
unique(State_LGA$Zone )


State_LGA$lon <- rect_coord$x
State_LGA$lat <- rect_coord$y
State_LGA$country <- country
State_LGA$maizeregion <- maizeArea$maize
State_LGA <- droplevels(State_LGA[!is.na(State_LGA$maizeregion), ])
plot(State_LGA$lon, State_LGA$lat)
State_LGA <- unique(State_LGA[, c("country", "Zone", "NAME_1", "NAME_2", "lon", "lat")])
head(State_LGA)

State_LGA2 <- merge(State_LGA, PlD_Maize, by="Zone")
head(State_LGA2)
### planting and harvset dates are unique by zone, so data shuld be sourced by zone

saveRDS(State_LGA2,"~/shared-data/Data/Maize/geoSpatial/Maize_AOI.RDS")


##########################################################################################################
##########################################################################################################

### creating the shape file for plating dates, altitude and length of growing season
library(terra)
library(tidyverse)
maize <- readRDS("~/shared-data/Data/Maize/geoSpatial/Maize_AOI.RDS")
maize <- maize %>%
  dplyr::select(-HvWeeks, -pld, -hvd, -planting_month_date, -harvest_month_date)

dim(maize)
head(maize)

maize_vect <- vect(maize, geom = c('lon', 'lat'), crs = 'EPSG:4326') 
class(maize_vect)

# dem <- rast("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ethiopia_harmonizeDST/Maize/raw/srtm_43_12.tif")
dem <- rast("~/shared-data/Data/General/Global_GeoData/Landing/ETH_dem.tif")
maize_elev <- listRaster_dem |> terra::extract(maize_vect)
unique(maize_elev[,2])


maize2 <- maize |> cbind(maize_elev[,-1])
colnames(maize2)
colnames(maize2)[11] <- "elevation"
head(maize2)

eth_zone <- gadm(path = ".", country = "ETH", level = 2)
plot(eth_zone)
plot(maize_vect, add = T)
head(eth_zone)
dim(eth_zone)

#joining
colnames(maize)
dim(maize)
names(eth_zone)

sp_df <- merge(eth_zone, maize2, all.x=TRUE, by.x=c('NAME_1', 'NAME_2'), by.y=c('NAME_1', 'NAME_2'))
class(sp_df)
head(sp_df)
plot(sp_df)

sp_df <- sp_df[!is.na(sp_df$lon)]



writeVector(sp_df, "Maize_zone_pl_hv_date.shp", "ESRI Shapefile", overwrite=TRUE)

require(rgdal)
Maize_PlHv <- readOGR(dsn = ".", layer = "Maize_zone_pl_hv_date")
plot(Maize_PlHv)
names(Maize_PlHv)


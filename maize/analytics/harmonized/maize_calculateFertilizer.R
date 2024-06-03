########################################################
#required packages
########################################################
packages_required <- c("terra","tidyverse", "sf", "tidyr", "foreach", "doParallel")
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])
}
invisible(lapply(packages_required, library, character.only = TRUE))


########################################################
## define pathout and read data
########################################################
pathOut <- "~/shared-data/Data/Maize/result/geoSpatial/"

## TODO the climate scenarios need to be smoothen up to avoid pockets of totally 
#different climate scenario in neighboring pixels


#read prediction raster files
below <- readRDS("~/shared-data/Data/Maize/result/geoSpatial/final_df_275_sp_below.rds")
normal <- readRDS("~/shared-data/Data/Maize/result/geoSpatial/final_df_275_sp_normal.rds")
above <- readRDS("~/shared-data/Data/Maize/result/geoSpatial/final_df_275_sp_above.rds")

#check the dimensions
dim(below) == dim(normal)
dim(normal) == dim(above)

#read dominant probability climate scenario and extract by points of predicted scenario
domin_clim <- rast("~/shared-data/Data/General/dominantClimate/dominant.tif")
pts <- below |> dplyr::select(x, y) |> terra::vect(geom = c('x', 'y'), crs = 'epsg:4326')

#the values are 1 = below, 2 = normal, 3 = above 
domin_clim_pts <- domin_clim |> terra::extract(pts) |> as.data.frame() |> 
  dplyr::select(-ID) |> cbind(below[,c("x","y")])
colnames(domin_clim_pts)[1] <- "prob"

########################################################
# merge the dominant prob scenrio to the simulation  
########################################################
nrow(domin_clim_pts) == nrow(above)

dom_below <- merge(below, domin_clim_pts, by =c("x", "y")) |>
  dplyr::filter(prob == 1)
dom_above <- merge(above, domin_clim_pts, by =c("x", "y")) |>
  dplyr::filter(prob == 3)
dom_normal <- merge(normal, domin_clim_pts, by =c("x", "y")) |>
  dplyr::filter(prob == 2)


dom_below$location <- paste(dom_below$x, dom_below$y, sep="_")
dom_above$location <- paste(dom_above$x, dom_above$y, sep="_")
dom_normal$location <- paste(dom_normal$x, dom_normal$y, sep="_")

########################################################
## get NUE and PUE
########################################################
##below
num_cores <- 12
cl <- makeCluster(num_cores)
registerDoParallel(cl)
source("~/shared-data/Scripts/generic/nutrientResponse/NUE_PUE.R")
below_result <- foreach(i=1:length(unique(dom_below$location)), .packages = c('terra', 'plyr', 'stringr','tidyr', 'tidyverse', 'sf')) %dopar% {
  lodData <- dom_below[dom_below$location == unique(dom_below$location)[i], ]
  NUE_PUE_below <- calculate_NPUE(lodData, col1="yield.0.0", col2="yield.240.50")
}
NUE_PUE_below <- dplyr::bind_rows(below_result)
saveRDS(NUE_PUE_below, "~/shared-data/Data/Maize/result/geoSpatial/NUE_PUE_below.RDS")
stopCluster(cl)

#NUE_PUE_below <- readRDS("~/shared-data/Data/Maize/result/geoSpatial/NUE_PUE_below.RDS")
## above
above_result <- foreach(i=1:length(unique(dom_above$location)), .packages = c('terra', 'plyr', 'stringr','tidyr', 'tidyverse', 'sf')) %dopar% {
  locData <- dom_above[dom_above$location == unique(dom_above$location)[i], ]
  NUE_PUE_above1 <- calculate_NPUE(locData, col1="yield.0.0", col2="yield.240.50")
}
NUE_PUE_above <- dplyr::bind_rows(above_result)
saveRDS(NUE_PUE_above, "~/shared-data/Data/Maize/result/geoSpatial/NUE_PUE_above.RDS")

#NUE_PUE_above <- readRDS("~/shared-data/Data/Maize/result/geoSpatial/NUE_PUE_above.RDS")
## normal
normal_result <- foreach(i=1:length(unique(dom_normal$location)), .packages = c('terra', 'plyr', 'stringr','tidyr', 'tidyverse', 'sf')) %dopar% {
  locData <- dom_normal[dom_normal$location == unique(dom_normal$location)[i], ]
  NUE_PUE_normal1 <- calculate_NPUE(locData, col1="yield.0.0", col2="yield.240.50")
}
NUE_PUE_normal <- dplyr::bind_rows(normal_result)
saveRDS(NUE_PUE_normal, "~/shared-data/Data/Maize/result/geoSpatial/NUE_PUE_normal.RDS")
NUE_PUE_normal <- readRDS("~/shared-data/Data/Maize/result/geoSpatial/NUE_PUE_normal.RDS")



########################################################
NUE_PUE_below <- readRDS("~/shared-data/Data/Maize/result/geoSpatial/NUE_PUE_below.RDS")
NUE_PUE_above <- readRDS("~/shared-data/Data/Maize/result/geoSpatial/NUE_PUE_above.RDS")
NUE_PUE_normal <- readRDS("~/shared-data/Data/Maize/result/geoSpatial/NUE_PUE_normal.RDS")


NUE_PUE_below$location <- paste(NUE_PUE_below$x, NUE_PUE_below$y, sep="_")
NUE_PUE_above$location <- paste(NUE_PUE_above$x, NUE_PUE_above$y, sep="_")
NUE_PUE_normal$location <- paste(NUE_PUE_normal$x, NUE_PUE_normal$y, sep="_")

###########################################################################
# covert to urea and TSP with steps ...
#1. by location select n:p2o5 ration> 1.2 and the top 90% aNUE and aPUE and then max yield and then min N and P application (every location will have one data)
#2. rbind all data across the different scenarios (if the domain definition and the dominant climate info are accurate there will be less mixes of different scenarios in a domain )
#3. attach domain info to the data: every location will have an indication of in which domain it belongs
#4. For every domain, aggregate the locations data, try mean, median but ideally we select by the most frequent rate by domain
###########################################################################

## removing the negative pue and nue
NUE_PUE_below <- NUE_PUE_below[NUE_PUE_below$nue > 0 & NUE_PUE_below$pue > 0, ]
NUE_PUE_above <- NUE_PUE_above[NUE_PUE_above$nue > 0 & NUE_PUE_above$pue > 0, ]
NUE_PUE_normal <- NUE_PUE_normal[NUE_PUE_normal$nue > 0 & NUE_PUE_normal$pue > 0, ]

## change P to P2O5: 140.94 P2O5 has 64.94 P
NUE_PUE_below$P2O5 <- (NUE_PUE_below$p * 141.94)/ 61.94
NUE_PUE_above$P2O5 <- (NUE_PUE_above$p * 141.94)/ 61.94
NUE_PUE_normal$P2O5 <- (NUE_PUE_normal$p * 141.94)/ 61.94


NUE_PUE_below$NPratio <- NUE_PUE_below$n/NUE_PUE_below$P2O5
NUE_PUE_above$NPratio <- NUE_PUE_above$n/NUE_PUE_above$P2O5
NUE_PUE_normal$NPratio <- NUE_PUE_normal$n/NUE_PUE_normal$P2O5

quantile(NUE_PUE_below$NPratio, probs=seq(0,1,0.05)) ## 0.2 - 48


NUE_PUE_below <- NUE_PUE_below[NUE_PUE_below$NPratio >= 1.2, ]
NUE_PUE_above <- NUE_PUE_above[NUE_PUE_above$NPratio >= 1.2, ]
NUE_PUE_normal <- NUE_PUE_normal[NUE_PUE_normal$NPratio >= 1.2, ]

summary(NUE_PUE_below$NPratio)


NUE_PUE_below <- NUE_PUE_below[NUE_PUE_below$n > 50, ] ## there will be no location in the maize belt growing maize with no fertilizer
NUE_PUE_above <- NUE_PUE_above[NUE_PUE_above$n > 50, ]
NUE_PUE_normal <- NUE_PUE_normal[NUE_PUE_normal$n > 50, ]


# 
# ll <- NUE_PUE_below[NUE_PUE_below$location == "38.4114423381926_7.59845147822936", ]
# llu <- ll[ll$nue > quantile(ll$nue, probs=0.90) & ll$pue > quantile(ll$pue, probs=0.90), ]
# lluy <- llu[llu$yield == max(llu$yield), ]
# K <- lluy[lluy$n == min(lluy$n),]


Loc_below <- NUE_PUE_below %>% 
  dplyr::group_by(location) %>% 
  dplyr::filter(nue > quantile(nue, probs=0.90) & pue > quantile(pue, probs=0.90)) %>% 
  dplyr::filter(yield == max(yield)) %>% 
  dplyr::filter(n == min(n)) %>%
  dplyr::filter(p == min(p)) %>%
  as.data.frame()


Loc_above <- NUE_PUE_above %>% 
  dplyr::group_by(location) %>% 
  dplyr::filter(nue > quantile(nue, probs=0.90) & pue > quantile(pue, probs=0.90)) %>% 
  dplyr::filter(yield == max(yield)) %>% 
  dplyr::filter(n == min(n)) %>% 
  dplyr::filter(p == min(p)) %>% 
  as.data.frame()


Loc_normal <- NUE_PUE_normal %>% 
  dplyr::group_by(location) %>% 
  dplyr::filter(nue > quantile(nue, probs=0.90) & pue > quantile(pue, probs=0.90)) %>% 
  dplyr::filter(yield == max(yield)) %>% 
  dplyr::filter(n == min(n)) %>% 
  dplyr::filter(p == min(p)) %>% 
  as.data.frame()



location_all <- rbind(Loc_above, Loc_below, Loc_normal)
summary(location_all)

location_all$Urea <- round(location_all$n/0.46, digits=0)
location_all$TSP <- round(location_all$p/0.205, digits=0) ## assuming the prate in the original data is P and not P2O5


quantile(location_all$n, probs=seq(0, 1, 0.01))

saveRDS(location_all, "~/shared-data/Scripts/Maize/responseFunction/outputs/location_all_point_rate_P2O5.RDS")

location_all <- readRDS("~/shared-data/Scripts/Maize/responseFunction/outputs/location_all_point_rate_P2O5.RDS")

quantile(location_all$n, probs=seq(0,1,0.01))
quantile(location_all$p, probs=seq(0,1,0.01))


# ## check few locations
# countryShp <- geodata::gadm("Ethiopia", level = 3, path='.')
# 
dd2 <- raster::extract(countryShp, location_all[, c("x", "y")])[, c("NAME_1", "NAME_2", "NAME_3")]
location_all$NAME_1 <- dd2$NAME_1
location_all$NAME_2 <- dd2$NAME_2
location_all$NAME_3 <- dd2$NAME_3

head(location_all)

Misraq_Shewa <- location_all[location_all$NAME_2 == "Misraq Shewa",]
Adami_Tulu <- Misraq_Shewa[Misraq_Shewa$NAME_3 == "Adami Tulu Jido Kombolcha", ]
unique(Adami_Tulu$n)
Adami_Tulu[order(Adami_Tulu$x, Adami_Tulu$y), ]

Adami_Tulu[Adami_Tulu$location == "38.4114423381926_7.59845147822936", ]

# 
# #TODO

## aggregate by woreda using frequent urea and TSP rate / max yield 
#clust_maize <- terra::vect("~/shared-data/Data/Maize/geoSpatial/domain_cluster/Maize_cluster_after_process.shp") |>
# terra::project(y = "epsg:4326") 
# dim(clust_maize)
location_sp <- location_all |> terra::vect(geom = c('x', 'y'), crs = "epsg:4326") 
plot(location_sp)

maize_clust <- terra::vect("~/shared-data/Data/Maize/geoSpatial/domain_cluster/Maize_cluster_9May_lessthan200Eliminate.shp") |>
  terra::project(y = "epsg:4326")
clust_loc <- maize_clust |> terra::intersect(location_sp) |> st_as_sf()
class(clust_loc)
dim(clust_loc)
head(clust_loc)

#select fertilizer rate with the dominant n & p
#setwd("/home/jovyan/shared-data/Data/Maize/result/geoSpatial/")
max_freq_loc <- clust_loc |>
  dplyr::group_by(WOREDANAME, ClusterDom, Urea, TSP) |>
  dplyr::summarize(frequency = n()) |> 
  dplyr::group_by(WOREDANAME, ClusterDom) |>
  dplyr::filter(frequency == max(frequency)) |>
  ungroup()

dim(max_freq_loc |> na.omit())
head(max_freq_loc)
class(max_freq_loc)
#terra::writeVector(max_freq_loc, "max_freq_rate.shp", filetype = "ESRI Shapefile")

# max_freq_loc |> dplyr::select(WOREDANAME, ClusterDom, Urea, TSP, yield, frequency) |>
#   write.csv("max_frequency_rate_woreda.csv", col.names = T)

#select fertilizer rate with the maximum yield
max_yield_loc <- clust_loc |>
  dplyr::group_by(WOREDANAME, ClusterDom) |>
  dplyr::filter(yield == max(yield)) |>
  ungroup() |> dplyr::select(WOREDANAME, ClusterDom, Urea, TSP, yield)

dim(max_yield_loc)
head(max_yield_loc)
# max_yield_loc |> as.data.frame() |>
#   write.csv("max_yield_rate_woreda.csv", col.names = T)

#terra::writeVector(max_yield_loc, "max_yld_rate.shp", filetype = "ESRI Shapefile")

#merge wth the rates with the cluster polygon
# clust_maize_dominant <- maize_clust |> 
#   st_as_sf() |> st_make_valid() |> st_join(max_freq_loc)
max_freq_loc_df <- max_freq_loc |> st_drop_geometry()
clust_maize_dominant <- maize_clust |> terra::aggregate(by = c("WOREDANAME","ClusterDom")) |> 
  st_as_sf() |> st_make_valid() |>
  dplyr::left_join(max_freq_loc_df, by = c("WOREDANAME","ClusterDom"))
dim(clust_maize_dominant)
tail(clust_maize_dominant)

max_yield_loc_df <- max_yield_loc |> st_drop_geometry()
clust_maize_max_yld <-  maize_clust |> terra::aggregate(by = c("WOREDANAME","ClusterDom")) |> 
  st_as_sf() |> st_make_valid() |>
  dplyr::left_join(max_yield_loc_df, by = c("WOREDANAME","ClusterDom"))

dim(clust_maize_max_yld)
head(clust_maize_max_yld)
tail(clust_maize_max_yld)

#write the polygons
clust_maize_dominant |> terra::vect() |>
  terra::writeVector("~/shared-data/Data/Maize/result/geoSpatial/final_rate_domain/maize_dominant_rate",
                     filetype = "ESRI Shapefile")
clust_maize_max_yld |> terra::vect() |>
  terra::writeVector("~/shared-data/Data/Maize/result/geoSpatial/final_rate_domain/maize_maxyld_rate",
                     filetype = "ESRI Shapefile")

#rasterize the points
#load the validation zone raster for maize
maize_rast <- terra::rast("~/shared-data/Data/Maize/geoSpatial/validation_zone/validation_cluster_maize_gcs.tif")
plot(maize_rast)

columns <- c('P2O5', 'Urea', 'TSP', "n", "p", "yield")
location_sp <- location_all |> terra::vect(geom = c("x", "y"), crs = "epsg:4326")
plot(location_sp, add = T)

path_result <- "~/shared-data/Data/Maize/result/geoSpatial/final_rate_raster"
for(i in 1:length(columns)){
  print(columns[i])
  rast <- terra::rasterize(location_sp, maize_rast, field = columns[i], values = 8, fun = "mean")
  terra::writeRaster(rast, paste(path_result, paste0(columns[i], "_2", ".tif"), sep = "/"), 
                     overwrite = T)
}



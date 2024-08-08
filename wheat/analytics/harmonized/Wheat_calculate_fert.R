
########################################################
#required packages
########################################################
rm(list=ls())
packages_required <- c("terra","tidyverse", "sf", "tidyr", "foreach", "doParallel")
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])
}
invisible(lapply(packages_required, library, character.only = TRUE))


########################################################
## define pathout and read data
########################################################
# pathOut <- "~/shared-data/Data/Wheat/result/geoSpatial/remaining_zone/"
crop <- "Wheat"
## run after the NP rate data are added
pathOut <- "~/shared-data/Data/Wheat/result/geoSpatial/"
above <- readRDS("~/shared-data/Data/Wheat/result/geoSpatial/NPrate_predicted_df_Wheat_above.rds")
normal <- readRDS("~/shared-data/Data/Wheat/result/geoSpatial/NPrate_predicted_df_Wheat_normal.rds")
below <- readRDS("~/shared-data/Data/Wheat/result/geoSpatial/NPrate_predicted_df_Wheat_below.rds")


#check the dimensions
dim(below) == dim(normal)
dim(normal) == dim(above)

#read dominant probability climate scenario and extract by points of predicted scenario
# domin_clim <- rast("~/shared-data/Data/General/dominantClimate/dominant.tif")
domin_clim <- rast("~/shared-data/Data/General/dominantClimate/dominant.tif")

pts <- above |> dplyr::select(x, y) |> terra::vect(geom = c('x', 'y'), crs = 'epsg:4326')

#the values are 1 = below, 2 = normal, 3 = above 
domin_clim_pts <- domin_clim |> terra::extract(pts) |> as.data.frame() |> 
  dplyr::select(-ID) |> cbind(above[,c("x","y")])
colnames(domin_clim_pts)[1] <- "prob"

########################################################
# merge the dominant prob scenario to the simulation  
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
  NUE_PUE_below <- calculate_NPUE(lodData, col1= "yield.230.5", col2="yield.220.5")
}
NUE_PUE_below <- dplyr::bind_rows(below_result)
saveRDS(NUE_PUE_below, "~/shared-data/Data/Wheat/result/geoSpatial/NUE_PUE_below_NPrate_NUE.RDS")



## above
above_result <- foreach(i=1:length(unique(dom_above$location)), .packages = c('terra', 'plyr', 'stringr','tidyr', 'tidyverse', 'sf')) %dopar% {
  locData <- dom_above[dom_above$location == unique(dom_above$location)[i], ]
  NUE_PUE_above1 <- calculate_NPUE(locData, col1= "yield.230.5", col2="yield.220.5")
}
NUE_PUE_above <- dplyr::bind_rows(above_result)
saveRDS(NUE_PUE_above, "~/shared-data/Data/Wheat/result/geoSpatial/NUE_PUE_above_NPrate_NUE.RDS")



## normal
normal_result <- foreach(i=1:length(unique(dom_normal$location)), .packages = c('terra', 'plyr', 'stringr','tidyr', 'tidyverse', 'sf')) %dopar% {
  locData <- dom_normal[dom_normal$location == unique(dom_normal$location)[i], ]
  NUE_PUE_normal1 <- calculate_NPUE(locData, col1= "yield.70.15", col2="yield.60.15")
}
NUE_PUE_normal <- dplyr::bind_rows(normal_result)
saveRDS(NUE_PUE_normal, "~/shared-data/Data/Wheat/result/geoSpatial/NUE_PUE_normal_NPrate_NUE.RDS")
stopCluster(cl)



########################################################
NUE_PUE_below <- readRDS("~/shared-data/Data/Wheat/result/geoSpatial/NUE_PUE_below_NPrate_NUE.RDS")
NUE_PUE_above <- readRDS("~/shared-data/Data/Wheat/result/geoSpatial/NUE_PUE_above_NPrate_NUE.RDS")
NUE_PUE_normal <- readRDS("~/shared-data/Data/Wheat/result/geoSpatial/NUE_PUE_normal_NPrate_NUE.RDS")


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


NUE_PUE_below <- NUE_PUE_below[NUE_PUE_below$NPratio >= 1, ]
NUE_PUE_above <- NUE_PUE_above[NUE_PUE_above$NPratio >= 1, ]
NUE_PUE_normal <- NUE_PUE_normal[NUE_PUE_normal$NPratio >= 1, ]

summary(NUE_PUE_below$NPratio)

# 
# ll <- NUE_PUE_below[NUE_PUE_below$location == "38.4114423381926_7.59845147822936", ]
# llu <- ll[ll$nue > quantile(ll$nue, probs=0.90) & ll$pue > quantile(ll$pue, probs=0.90), ]
# lluy <- llu[llu$yield == max(llu$yield), ]
# K <- lluy[lluy$n == min(lluy$n),]


Loc_below <- NUE_PUE_below %>% 
  dplyr::group_by(location) %>% 
  dplyr::filter(nue > quantile(nue, probs=0.80) & pue > quantile(pue, probs=0.80)) %>% 
  dplyr::filter(yield == max(yield)) %>% 
  dplyr::filter(n == min(n)) %>%
  dplyr::filter(p == min(p)) %>%
  as.data.frame()


Loc_above <- NUE_PUE_above %>% 
  dplyr::group_by(location) %>% 
  dplyr::filter(nue > quantile(nue, probs=0.80) & pue > quantile(pue, probs=0.80)) %>% 
  dplyr::filter(yield == max(yield)) %>% 
  dplyr::filter(n == min(n)) %>% 
  dplyr::filter(p == min(p)) %>% 
  as.data.frame()


Loc_normal <- NUE_PUE_normal %>% 
  dplyr::group_by(location) %>% 
  dplyr::filter(nue > quantile(nue, probs =0.80) & pue > quantile(pue, probs=0.80)) %>% 
  dplyr::filter(yield == max(yield)) %>% 
  dplyr::filter(n == min(n)) %>% 
  dplyr::filter(p == min(p)) %>% 
  as.data.frame()



location_all <- rbind(Loc_above, Loc_below, Loc_normal)
summary(location_all)

location_all$Urea <- round(location_all$n/0.46, digits=0)
location_all$TSP <- round(location_all$p/0.205, digits=0) ## assuming the prate in the original data is P and not P2O5


quantile(location_all$n, probs=seq(0, 1, 0.01))

saveRDS(location_all, "~/shared-data/Data/Wheat/result/geoSpatial/location_all_point_NPrate.RDS")

quantile(location_all$n, probs=seq(0,1,0.01))
quantile(location_all$p, probs=seq(0,1,0.01))

#rasterize the points
#load the validation zone raster for Wheat
Wheat_rast <- terra::rast("~/shared-data/Data/Wheat/geoSpatial/validation_zone/wheat_new.tif")
plot(Wheat_rast)
plot(location_sp, add = T)

columns <- c('NPratio', 'P2O5', 'Urea', 'TSP', "n", "p", "yield")
location_sp <- location_all |> terra::vect(geom = c("x", "y"), crs = "epsg:4326")
plot(location_sp, add = T)

path_result <- "~/shared-data/Data/Wheat/result/geoSpatial/NPrate/NUE_selected"
if(!dir.exists(path_result)){
  dir.create(path_result)
}
for(i in 1:length(columns)){
  print(columns[i])
  rast <- terra::rasterize(location_sp, Wheat_rast, field = columns[i])
  terra::writeRaster(rast, paste(path_result, paste0(columns[i], ".tif"), sep = "/"), 
                     overwrite = T)
}



## testing
countryShp <- geodata::gadm("Ethiopia", level = 3, path='.')# 
abovexy <- unique(NUE_PUE_above[, c("x", "y")])
dd2 <- raster::extract(countryShp, abovexy)[, c("NAME_1", "NAME_2", "NAME_3")]
abovexy$NAME_1 <- dd2$NAME_1
abovexy$NAME_2 <- dd2$NAME_2
abovexy$NAME_3 <- dd2$NAME_3
abovexy[abovexy$NAME_3 == "Dera", ]
abovexy$location <- paste(abovexy$x, abovexy$y, sep="_")

Dera <- NUE_PUE_above[NUE_PUE_above$location %in% unique(abovexy$location), ]
unique(Dera$NPratio)
unique(Dera$n)
unique(Dera$p)

ggplot(Dera, aes(n, yield)) +
  geom_point() 
ggplot(Dera, aes(factor(n_rate2), blup)) +
  geom_boxplot() 
ggplot(Dera, aes(factor(n_rate2),  nue)) +
  geom_boxplot()

ggplot(Dera, aes(p_rate2, blup)) +
  geom_point() 
ggplot(Dera, aes(factor(p_rate2),  nue)) +
  geom_boxplot()






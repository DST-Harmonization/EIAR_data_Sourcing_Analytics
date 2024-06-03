

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
pathOut <- "~/shared-data/Data/Sorghum/Intermediate/Secondrun"

#read prediction raster files
below <- readRDS("~/shared-data/Data/Sorghum/Intermediate/Secondrun/final_df_208_sp_below.rds")
normal <- readRDS("~/shared-data/Data/Sorghum/Intermediate/Secondrun/final_df_208_sp_normal.rds")
above <- readRDS("~/shared-data/Data/Sorghum/Intermediate/Secondrun/final_df_208_sp_above.rds")

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
  locData <- dom_below[dom_below$location == unique(dom_below$location)[i], ]
  NUE_PUE_below1 <- calculate_NPUE(locData, col1 = "yield.0.0", col2 = "yield.150.60")
}
NUE_PUE_below <- dplyr::bind_rows(below_result)
saveRDS(NUE_PUE_below, "~/shared-data/Data/Sorghum/Intermediate/Secondrun/NUE_PUE_below.RDS")


## above
above_result <- foreach(i=1:length(unique(dom_above$location)), .packages = c('terra', 'plyr', 'stringr','tidyr', 'tidyverse', 'sf')) %dopar% {
  locData <- dom_above[dom_above$location == unique(dom_above$location)[i], ]
  NUE_PUE_above1 <- calculate_NPUE(locData, col1 = "yield.0.0", col2 = "yield.150.60")
}
NUE_PUE_above <- dplyr::bind_rows(above_result)
saveRDS(NUE_PUE_above, "~/shared-data/Data/Sorghum/Intermediate/Secondrun/NUE_PUE_above.RDS")


## normal
normal_result <- foreach(i=1:length(unique(dom_normal$location)), .packages = c('terra', 'plyr', 'stringr','tidyr', 'tidyverse', 'sf')) %dopar% {
  locData <- dom_normal[dom_normal$location == unique(dom_normal$location)[i], ]
  NUE_PUE_normal1 <- calculate_NPUE(locData, col1 = "yield.0.0", col2 = "yield.150.60")
}
NUE_PUE_normal <- dplyr::bind_rows(normal_result)
saveRDS(NUE_PUE_normal, "~/shared-data/Data/Sorghum/Intermediate/Secondrun/NUE_PUE_normal.RDS")

stopCluster(cl)


########################################################
## convert N&P to urea and TSP: Urea = N rate / 0.46
## TSP = P rate / 0.20

########################################################
NUE_PUE_below <- readRDS("~/shared-data/Data/Sorghum/Intermediate/Secondrun/NUE_PUE_below.RDS")
NUE_PUE_above <- readRDS("~/shared-data/Data/Sorghum/Intermediate/Secondrun/NUE_PUE_above.RDS")
NUE_PUE_normal <- readRDS("~/shared-data/Data/Sorghum/Intermediate/Secondrun/NUE_PUE_normal.RDS")


NUE_PUE_below$location <- paste(NUE_PUE_below$x, NUE_PUE_below$y, sep="_")
NUE_PUE_above$location <- paste(NUE_PUE_above$x, NUE_PUE_above$y, sep="_")
NUE_PUE_normal$location <- paste(NUE_PUE_normal$x, NUE_PUE_normal$y, sep="_")


ggplot(Q, aes(n, yield, group=location)) +
  geom_line()+
  facet_wrap(~p)+
  theme(legend.position = "none")


inputDataTrial <- readRDS("~/shared-data/Data/Sorghum/fieldData/sorghum_modelReady.rds")
ggplot(inputDataTrial, aes(x=n_round, y=blup))+
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE, formula = y ~ x) 

## the main issue is the yield response for trials with >130 kg/ha N is quite high and the model is using this information 

inputDataTrial_A100 <- inputDataTrial[inputDataTrial$n_round <= 135 & inputDataTrial$n_round > 0, ]
mean(inputDataTrial_A100$blup)## 3155 kg yield


inputDataTrial_100 <- inputDataTrial[inputDataTrial$n_round > 135, ] ## expert advice not to take N application above 135 kg/ha
mean(inputDataTrial_100$blup) ## 3075 kg

## 11% of the data has > 100 kg N/ha and this additional N is giving a 1 t/ha yield increase 


###########################################################################
# covert to urea and TSP with steps ...
#1. remove negative nue and pue, 
#2. calculate N:P2O5 ratio and exclude points where this ratio is below 1
#3. select data in the top 80% aNUE and aPUE and within this range select the one with max yield (as suggested by Gizaw an attepmt was done to select for max aNUE )
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

quantile(NUE_PUE_below$NPratio, probs=seq(0,1,0.05)) ## 0.14 - 6.54


NUE_PUE_below <- NUE_PUE_below[NUE_PUE_below$NPratio >= 1, ]
NUE_PUE_above <- NUE_PUE_above[NUE_PUE_above$NPratio >= 1, ]
NUE_PUE_normal <- NUE_PUE_normal[NUE_PUE_normal$NPratio >= 1,]

summary(NUE_PUE_below$NPratio)


### if we remove N rate >100, what would be the next rate it would select. 
NUE_PUE_below <- NUE_PUE_below[NUE_PUE_below$n < 135, ]
NUE_PUE_above <- NUE_PUE_above[NUE_PUE_above$n < 135, ]
NUE_PUE_normal <- NUE_PUE_normal[NUE_PUE_normal$n < 135,]

########################################################################################################################
## the preferred approach it selecting for max aNUE and aPUE and then max yield
########################################################################################################################
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
  dplyr::filter(nue > quantile(nue, probs=0.80) & pue > quantile(pue, probs=0.80)) %>% 
  dplyr::filter(yield == max(yield)) %>% 
  dplyr::filter(n == min(n)) %>% 
  dplyr::filter(p == min(p)) %>% 
  as.data.frame()

location_all <- rbind(Loc_above, Loc_below, Loc_normal)

location_all$Urea <- round(location_all$n/0.46, digits=0)
location_all$TSP <- round(location_all$p/0.205, digits=0) ## assuming the prate in the original data is P and not P2O5

quantile(location_all$n, probs=seq(0,1, 0.01))
quantile(location_all$p, probs=seq(0,1, 0.01))

summary(location_all)
# ## check few locations
countryShp <- geodata::gadm("Ethiopia", level = 3, path='.')

dd2 <- raster::extract(countryShp, location_all[, c("x", "y")])[, c("NAME_1", "NAME_2", "NAME_3")]
location_all$NAME_1 <- dd2$NAME_1
location_all$NAME_2 <- dd2$NAME_2
location_all$NAME_3 <- dd2$NAME_3



saveRDS(location_all, "~/shared-data/Data/Sorghum/Intermediate/Secondrun/Sorghum_point_rate.RDS")
Sorghum_point_rate <- readRDS("~/shared-data/Data/Sorghum/Intermediate/Secondrun/Sorghum_point_rate.RDS")
plot(Sorghum_point_rate$x, Sorghum_point_rate$y)


Sorghum_point_rate[Sorghum_point_rate$NAME_2 == "Konso", ]

ggplot(Sorghum_point_rate, aes(NAME_3, n, fill=NAME_2))+
  geom_boxplot()+
  facet_wrap(~NAME_2)+
  theme_bw()+
  theme(axis.text.x = element_blank(), legend.position = "none")

ggplot(Sorghum_point_rate, aes(x, y, col=prob))+
  geom_point()

##convert this to raster
sorg_rast <- rast("~/shared-data/Data/Sorghum/result/geoSpatial/validation_zones_sorghum_4km.tif")
plot(sorg_rast)

columns <- c('P2O5', 'Urea', 'TSP', "n", "p", "yield")
location_sp <- Sorghum_point_rate |> terra::vect(geom = c("x", "y"), crs = "epsg:4326")
plot(location_sp, col = "red", add = T)

path_result <- "~/shared-data/Data/Sorghum/result/geoSpatial/final_rate_raster_2ndrun"
if(!dir.exists(path_result)){
  dir.create(file.path(path_result), recursive = T)
}
for(i in 1:length(columns)){
  print(columns[i])
  rast <- terra::rasterize(location_sp, sorg_rast, field = columns[i])
  terra::writeRaster(rast, paste(path_result, paste0(columns[i], ".tif"), sep = "/"), 
                     overwrite = T)
}

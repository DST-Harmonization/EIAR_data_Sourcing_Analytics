

################################################################################################################
# 1. Sourcing required packages -------------------------------------------
#################################################################################################################
#################################################################################################################
packages_required <- c("doParallel", "foreach", "h2o", "tidyverse", "dplyr", "stringr", "ggplot2", "ggpmisc", "Metrics", "keras")


# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))


#################################################################################################################
# 2. join the soil INS with geo-spatial data for ML
#################################################################################################################

inputDataTrial <- readRDS("~/shared-data/Data/Tef/fieldData/tef_modelReady.rds")
soildata_iSDA <- readRDS("~/shared-data/Data/Tef/geoSpatial/geo_4ML_trial/SoilDEM_PointData_trial.RDS")
weatherdata <- readRDS("~/shared-data/Data/Tef/geoSpatial/geo_4ML_trial/weatherSummaries_trial.RDS")


inputDataTrial <- inputDataTrial %>% 
  dplyr::select(-c(yield_diff, treatment_id)) %>% 
  dplyr::rename(lon = long2, lat = lat2)

#inputDataTrial <- inputDataTrial %>%
#  dplyr::rename(lon = long2, lat = lat2)

soildata_iSDA <- soildata_iSDA %>% 
  dplyr::select(-c("country", "NAME_1", "NAME_2","lon","lat")) %>%
  dplyr::rename(trial_id = ID) %>% 
  unique()


## you should test the model accuracy using the soil data fro iSDA and EthiSis and use the better one
Tef_ML_data <- merge(inputDataTrial, soildata_iSDA, by="trial_id")

weatherdata <- weatherdata %>% 
  dplyr::select(-c(NAME_1, NAME_2, plantingDate, harvestDate, pyear, latitude, longitude)) %>% 
  dplyr::rename(trial_id = ID) %>% 
  unique

Tef_ML_data <- merge(Tef_ML_data, weatherdata, by= "trial_id")

Tef_ML_data
### remove variables with NA values, if we take complete case, because of the difference in growing season there are quite some data points wiht NA weatehr data for later omonths 

Tef_ML_trial <- Tef_ML_data %>% 
  dplyr::select(-c(names(which(colSums(is.na(Tef_ML_data)) > 0)))) %>% 
  dplyr::mutate_if(is.character, as.factor) %>% 
  unique()

str(Tef_ML_trial)
colnames(Tef_ML_trial)
Tef_ML_trial

##### create topography and control classes, ...
## create topo- class: ICRISAT to provide information how they are doing it, we have info from CIAT already
## for now I made it at step of 250 m altitude difference 


ds_alt <- Tef_ML_trial %>%
  dplyr::group_by(trial_id) %>%
  dplyr::summarise(altClass = median(altitude)) %>%
  mutate(altClass = cut(altClass, c(-Inf, 1500, 1750, 2000, 2250, 2500, 2700, Inf), labels = c("Altclass1", "Altclass2", "Altclass3", "Altclass4", "Altclass5", "Altclass6", "Altclass7")))%>%
  unique()
head(ds_alt)

Tef_ML_trial <- merge(Tef_ML_trial, ds_alt, by="trial_id")
print(Tef_ML_trial)

Tef_ML_trial <- Tef_ML_trial %>% dplyr::select(-c(trial_id, source, year, ref_trt, grain_yield_kgpha, lon, lat, NAME_1, NAME_2))

str(Tef_ML_trial)

is.na(Tef_ML_trial)
summary(Tef_ML_trial)
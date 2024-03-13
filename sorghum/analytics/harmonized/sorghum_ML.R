
#################################################################################################################
# 1. Sourcing required packages -------------------------------------------
#################################################################################################################
#################################################################################################################
packages_required <- c("doParallel", "foreach", "h2o", "tidyverse", "dplyr", "stringr", "ggplot2", "ggpmisc", "Metrics")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))


#################################################################################################################
# 2. join the soil INS with geo-spatial data for ML
#################################################################################################################
## First try if adding the control improves the model
## create topo- class: ICRISAT to provide information how they are doing it, we have info from CIAT already
## If not make use of the INS NPK as covariate and model yield  as response function. INS will be aggregated by altitude class and region


inputDataTrial <- readRDS("~/shared-data/Data/Wheat/fieldData/wheat_modelReady.rds")
soildata_iSDA <- readRDS("~/shared-data/Data/Wheat/geoSpatial/geo_4ML_trial/SoilDEM_PointData_trial.RDS")
## TODO soildata_EthioSIS
weatherdata <- readRDS("~/shared-data/Data/Wheat/geoSpatial/geo_4ML_trial/weatherSummaries_trial.RDS")
wheatSupply <- readRDS("~/shared-data/Data/Wheat/Intermediate/wheatSupply507.RDS")

soildata <- soildata %>% 
  dplyr::select(-c(country)) %>%
  dplyr::rename(TLID = ID) %>% 
  unique()

wheatSupply <- merge(wheatSupply, soildata, by="TLID")

wheatSupply <- wheatSupply %>%
  dplyr::select(-c(controlObs, yieldQUEFTS, conVal )) %>% 
  unique()

weatherdata <- weatherdata %>% 
  dplyr::select(-c(NAME_1, NAME_2, plantingDate, harvestDate, pyear, latitude, longitude)) %>% 
  dplyr::rename(TLID = ID) %>% 
  unique

wheat_ML_trial <- merge(wheatSupply, weatherdata, by= "TLID")

### remove variables with NA values, if we take complete case, because of the difference in growing season there are quite some data points wiht NA weatehr data for later omonths 

wheat_ML_trial <- wheat_ML_trial %>% 
  dplyr::select(-c(names(which(colSums(is.na(wheat_ML_trial)) > 0)))) %>% 
  dplyr::mutate_if(is.character, as.factor) %>% 
  unique()
str(wheat_ML_trial)


##### create topography and control classes, ...

hist(wheat_ML_trial$altitude)
## add control yield class

#select treatments with high nutrient rates (Increased NPK for RS-PFR-1, NPK_all for IFDC, NPK11 for SA-VAL-1):
ds_control <- inputDataTrial %>%
  filter(treatment_id  == "0_0_0") %>%
  dplyr::group_by(trial_id, NAME_1, NAME_2, NAME_3) %>%
  dplyr::summarise(conY = median(blup)) %>%
  mutate(conY = cut(conY, c(-Inf, 1000, 1500, 2000, 2500, 3000, Inf), labels = c("Class1", "Class2", "Class3", "Class4", "Class5", "Class6")))%>%
  unique()
str(ds_control)


#Topography and EAZ data:
tdt <- sdt %>%
  dplyr::select(c(expCode, TLID, altitude)) %>%
  mutate(alt = cut(altitude, breaks = c(-Inf, 1000, 1500, 2000, 2500, 3000, Inf)))%>%
  unique()

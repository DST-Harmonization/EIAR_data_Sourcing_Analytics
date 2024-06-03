

### Steps to follow:
# 1. merge soil, weather and yield data of training set
# 2. split data to test and training 
# 3. tune the hyper parameter
# 4. train, validate and select model
# 5. get the soil and weather data for prediction (for the top 15 parameters based on var importance)
# 6. prepare the NP matrix and add it to the prediction set
# 7. merge and split training and the prediction data set set so that the whole factor levels will be associated to both data set
# 8 use the selected model with the prediction set to get yield estimates. 
# 9 import the recommendations domains and select max agronomy efficient fertilizer rates by domain
# 10.with price data available, run price sensitivity analysis (desktop)


#################################################################################################################
# Sourcing required packages -------------------------------------------
#################################################################################################################
#################################################################################################################
rm(list=ls())
packages_required <- c("doParallel", "foreach", "h2o", "tidyverse", "dplyr", "stringr", "ggplot2", "ggpmisc", "Metrics")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))


#################################################################################################################
## 1. merge soil, weather and yield data of training set
#################################################################################################################

inputDataTrial <- readRDS("~/shared-data/Data/Sorghum/fieldData/sorghum_modelReady.rds")
weatherdata <- readRDS("~/shared-data/Data/Sorghum/geoSpatial/geo_4ML_trial/weatherSummaries_trial.RDS")
Soil_type_tr <- unique(readRDS("~/shared-data/Data/Sorghum/geoSpatial/geo_4ML_trial/sorghum_soil_type.rds"))
soildata_iSDA <- readRDS("~/shared-data/Data/Sorghum/geoSpatial/geo_4ML_trial/SoilDEM_PointData_trial.RDS")
soildata_iSDA <- unique(merge(soildata_iSDA, Soil_type_tr, by.x="ID", by.y="trial_id"))

quantile(inputDataTrial$n_round, probs=seq(0, 1, 0.01))

dim(inputDataTrial)
inputDataTrial <- inputDataTrial %>% 
  dplyr::select(-c(Data.source,Year, NAME_1, NAME_2, WLY_wet, WLY_dry,
                   Grain_yield, treatment_id, ref_trt, yield_diff, COUNTRY,variety)) %>% 
  dplyr::rename(lon = Long, lat = Lat) %>% 
  unique()
dim(inputDataTrial)

soildata_iSDA <- soildata_iSDA %>% 
  dplyr::select(-c("country", "NAME_1", "NAME_2","lon","lat")) %>%
  dplyr::rename(trial_id = ID) %>% 
  unique()


## you should test the model accuracy using the soil data fro iSDA and EthiSis and use the better one
Sorghum_ML_data <- merge(inputDataTrial, soildata_iSDA, by="trial_id", all.x=TRUE)
dim(Sorghum_ML_data)

weatherdata <- weatherdata %>% 
  dplyr::select(-c(NAME_1, NAME_2, plantingDate, harvestDate, pyear, latitude, longitude)) %>% 
  dplyr::rename(trial_id = ID) %>% 
  unique

Sorghum_ML_data <- merge(Sorghum_ML_data, weatherdata, by= "trial_id")

### remove variables with NA values, if we take complete case, because of the difference in growing season there are quite some data points wiht NA weatehr data for later omonths 

Sorghum_ML_trial <- Sorghum_ML_data %>% 
  dplyr::select(-c(names(which(colSums(is.na(Sorghum_ML_data)) > 0)))) %>% 
  dplyr::mutate_if(is.character, as.factor) %>% 
  unique()
dim(Sorghum_ML_trial)


##### create topography and control classes, ...
## create topo- class: ICRISAT to provide information how they are doing it, we have info from CIAT already
## for now I made it at step of 250 m altitude difference 

max(Sorghum_ML_trial$altitude)

ds_alt <- Sorghum_ML_trial %>%
  dplyr::group_by(trial_id) %>%
  dplyr::summarise(altClass = median(altitude)) %>%
  mutate(altClass = cut(altClass, c(-Inf, 1000, 1200, 1400, 1600, 1800, 2000, Inf), labels = c("Altclass1", "Altclass2", "Altclass3", "Altclass4", "Altclass5", "Altclass6", "Altclass7")))%>%
  unique()
head(ds_alt)

Sorghum_ML_trial <- merge(Sorghum_ML_trial, ds_alt, by="trial_id")
Sorghum_ML_trial <- Sorghum_ML_trial %>% dplyr::rename( N_rate =n_round, P_rate =p_round, K_rate = k_round)
names(Sorghum_ML_trial)
Sorghum_ML_trial <- Sorghum_ML_trial %>% dplyr::select(-c(trial_id, lon,lat,
                                                          fe_top,fe_bottom,Cu_0_30,Mn_0_30)) %>% 
  
  unique()

dim(Sorghum_ML_trial)
names(Sorghum_ML_trial)
Sorghum_ML_trial$soil_type <- as.factor(Sorghum_ML_trial$soil_type)


#################################################################################################################
## 2. split data to test and training 
#################################################################################################################
pathOut <- "~/shared-data/Data/Sorghum/Intermediate/soilGrids"
if (!dir.exists(pathOut)){
  dir.create(file.path(pathOut), recursive = TRUE)
}

ML_inputData <- Sorghum_ML_trial
# ML_inputData$CLIMATEZONE <- as.factor(ML_inputData$CLIMATEZONE)
dim(ML_inputData)
str(ML_inputData)


response <- "blup"
predictors <- ML_inputData %>% dplyr::select(-c(blup)) %>%  names()

#######################################################################################
## testing different models 
# convert our data frame into a special object that h2o can recognize
h2o.init()
ML_inputData.h2o <- as.h2o(ML_inputData)

#create a random training-test split of our data ## should be possible to do it by missing one
ML_inputData_split <- h2o.splitFrame(data = ML_inputData.h2o, ratios = 0.8, seed = 1234)
training_data <- ML_inputData_split[[1]]
test_data <- ML_inputData_split[[2]]




#################################################################################################################
## 3. tune the hyper parameter
#################################################################################################################
## Specify the hyper-parameter grid: test some values around the out put of h2o.get_best_model(auto ML)
hyperparams_gbm <- list(
  ntrees = seq(800, 1200, 100), ### 
  max_depth = seq(4, 10, 2)
)

# Train and tune the gradient boosting model
grid_gbm <- h2o.grid(
  algorithm = "gbm",
  x = predictors,
  y = response,
  #y = response2,
  grid_id = "hyperparams_gbm",
  hyper_params = hyperparams_gbm,
  training_frame = training_data,
  validation_frame = test_data,
  seed = 444
)


saveRDS(grid_gbm, "/home/jovyan/shared-data/Data/Sorghum/Intermediate/Secondrun/grid_gbm_hyperParam.rds")

#grid_gbm <- readRDS(paste(pathOut, "grid_gbm.rds", sep=""))
# Get the best hyper parameters
best_hyperParm <- h2o.getModel(grid_gbm@model_ids[[1]])
print(best_hyperParm@parameters) 

ntrees_gbm_optim <- best_hyperParm@parameters$ntrees
max_depth_gbm_optim <- best_hyperParm@parameters$max_depth



#################################################################################################################
## 4. train, validate and select model
#################################################################################################################

### fit the model with the tuned hyper parameters: 
ML_gbm <- h2o.gbm(x = predictors,
                  y = response,
                  ntrees = ntrees_gbm_optim,
                  max_depth = max_depth_gbm_optim,
                  training_frame = training_data,
                  validation_frame = test_data,
                  keep_cross_validation_predictions = TRUE,
                  nfolds = 5,
                  seed = 444)

h2o.residual_analysis_plot(ML_gbm,test_data)

h2o.saveModel(object = ML_gbm, path = "/home/jovyan/shared-data/Data/Sorghum/Intermediate/Secondrun", force = TRUE)

# load the model
saved_model <- h2o.loadModel("/home/jovyan/shared-data/Data/Sorghum/Intermediate/Secondrun/GBM_model_R_1716383638344_1")
pathOut <- "/home/jovyan/shared-data/Data/Sorghum/Intermediate/Secondrun"

my_local_model <- h2o.download_model(saved_model, path = pathOut)
# upload the model that you just downloded above to the H2O cluster
uploaded_model <- h2o.upload_model(my_local_model)


rmse_r2_gbm <- data.frame(mae = round(h2o.mae(ML_gbm, train=TRUE, valid=TRUE), 0),
                          rmse = round(h2o.rmse(ML_gbm, train=TRUE, valid=TRUE), 0),
                          R_sq = round(h2o.r2(ML_gbm, train=TRUE, valid=TRUE), 2))

rmse_r2_gbm

bestMod_tuened <- data.frame(ntrees = ntrees_gbm_optim, maxDepth=max_depth_gbm_optim, R2 = rmse_r2_gbm$R_sq, rmse = rmse_r2_gbm$rmse)

h2o.partialPlot(object = ML_gbm, newdata = test_data, cols = c( "N_rate",  "P_rate"))

GBM_valid <- test_data
GBM_valid$predResponse <- h2o.predict(object = ML_gbm, newdata = test_data)
GBM_valid <- as.data.frame(GBM_valid)
GBM_valid$Response <- GBM_valid[,which(names(GBM_valid)==response)]

ggplot(GBM_valid, aes(Response, predResponse)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  xlab("Measured Yield") + ylab("predicted yield")+
  ggtitle("Gradient Boosting: soilGrids, iSDA") +
  #xlim(0,max(ML_inputData$yield)) + ylim(0,max(ML_inputData$Yield))+ 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size=12))


#the variable importance plot
par(mar = c(1, 1, 1, 1))#Expand the plot layout pane
h2o.varimp_plot(ML_gbm)


# shap values = the direction of the relationship between our features and target
# e.g., high vlaues of total rainfall has positive contribution
h2o.shap_summary_plot(ML_gbm, test_data)

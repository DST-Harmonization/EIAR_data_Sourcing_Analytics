

#################################################################################################################
# 1. Sourcing required packages -------------------------------------------
#################################################################################################################
#################################################################################################################
rm(list = ls())

packages_required <- c("doParallel", "foreach", "h2o", "tidyverse", "dplyr", "stringr", "ggplot2", "ggpmisc", "Metrics","terra")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

#################################################################################################################
# describing model fit data
#################################################################################################################
inputDataTrial <- readRDS("~/shared-data/Data/Wheat/fieldData/wheat_modelReady_WLY_NPrateAdded.RDS")
dim(inputDataTrial) ## has 13,725 data points with treatment replications couted
length(unique(inputDataTrial$trial_id)) ## 635 unique trial IDs
inputDataTrial <- inputDataTrial %>%
  dplyr::select(-c(yield_diff, treatment_id, k_rate2, ref_trt, grain_yield_kgpha)) %>% 
  unique()
dim(inputDataTrial)## 4120 data points removing the replication after fitting linear mixed effects to estimate treatment effect for every locations

4120/635 ## in average there are about 6 to 7 treatments at every location

ggplot(inputDataTrial, aes(NAME_1, blup, fill=NAME_1))+
  geom_boxplot()

inputDataTrial %>% dplyr::select(c(NAME_1, trial_id)) %>% 
  unique() %>% 
  dplyr::group_by(NAME_1) %>%
  count(NAME_1) %>% 
  as.data.frame()



#################################################################################################################
# 2. join the soil INS with geo-spatial data for ML
#################################################################################################################
inputDataTrial <- readRDS("~/shared-data/Data/Wheat/fieldData/wheat_modelReady_WLY_NPrateAdded.RDS")

dim(inputDataTrial)
weatherdata <- readRDS("~/shared-data/Data/Wheat/geoSpatial/geo_4ML_trial/weatherSummaries_trial.RDS")

weatherdata <- weatherdata %>% 
  dplyr::select(-c(NAME_1, NAME_2, plantingDate, harvestDate, pyear, latitude, longitude)) %>% 
  dplyr::rename(trial_id = ID) %>% 
  unique
field_weather <- merge(inputDataTrial, weatherdata, by = "trial_id")
dim(field_weather)

field_weather <- field_weather %>%
  dplyr::select(-c(yield_diff, treatment_id, source, year, NAME_1, NAME_3, k_rate2, treatment_id, ref_trt, grain_yield_kgpha)) %>% 
  unique()



## get EthioSis Data for all trial locations
EC <- rast("shared-data/Data/Wheat/Intermediate/ML/EC.tif")
soil_type <- rast("shared-data/Data/Wheat/Intermediate/ML/soil_type.tif")
tri <- rast("shared-data/Data/Wheat/Intermediate/ML/tri.tif")
tpi <- rast("shared-data/Data/Wheat/Intermediate/ML/tpi.tif")
slope <- rast("shared-data/Data/Wheat/Intermediate/ML/slope.tif")
dem <- rast("shared-data/Data/Wheat/Intermediate/ML/dem.tif")
P <- rast("shared-data/Data/Wheat/Intermediate/ML/P.tif")
N <- rast("shared-data/Data/Wheat/Intermediate/ML/N.tif")
pH <- rast("shared-data/Data/Tef/Intermediate/ML/pH.tif")
CEC <- rast("shared-data/Data/Tef/Intermediate/ML/CEC.tif")
# nitrogen_0_30 <- rast("shared-data/Data/Tef/Intermediate/ML/nitrogen_0-30cm.tif")
OM <- rast("shared-data/Data/Maize/Intermediate/ML/OM.tif")



points <- vect(inputDataTrial, geom = c("long2", "lat2"), crs = "epsg:4326")
EC_points <- EC %>%  terra::extract(points)
soil_type_points <- soil_type %>%  terra::extract(points)
tri_points <- tri %>%  terra::extract(points)
tpi_points <- tpi %>%  terra::extract(points)
slope_points <- slope %>%  terra::extract(points)
dem_points <- dem %>%  terra::extract(points)
P_points <- P %>%  terra::extract(points)
N_points <- N %>%  terra::extract(points)
pH_points <- pH %>%  terra::extract(points)
CEC_points <- CEC %>%  terra::extract(points)
# N30_points <- nitrogen_0_30 %>%  terra::extract(points)
OM_points <- OM %>%  terra::extract(points)


soil_EthSIS <- inputDataTrial[, c("long2", "lat2")] %>%
  cbind(EC_points$EC)%>%
  cbind(soil_type_points$soil_type)%>%
  cbind(tri_points$TRI)%>%
  cbind(tpi_points$TPI)%>%
  cbind(slope_points$slope)%>%
  cbind(dem_points$srtm_43_12)%>%
  cbind(P_points$P_rf)%>%
  cbind(N_points$N_rf)%>%
  cbind(pH_points$pH_rf)%>%
  cbind(CEC_points$CEC_rf)%>%
  # cbind(N30_points$`nitrogen_0-30cm`)%>%
  cbind(OM_points$OM_pct_rf)

names(soil_EthSIS) <- c("long2","lat2","EC","soil_type","TRI", "TPI", "slope","Alt", "P_soil" ,"N_soil", "pH","CEC","OM" )
dim(soil_EthSIS)
soil_EthSIS <- unique(soil_EthSIS)

wheat_ML_data <- merge(field_weather, soil_EthSIS, by=c("long2","lat2"))
dim(wheat_ML_data)

wheat_ML_trial <- wheat_ML_data %>%
  dplyr::select(-c(long2, lat2, trial_id))

### remove variables with NA values, if we take complete case, because of the difference in growing season there are quite some data points wiht NA weatehr data for later omonths 
wheat_ML_trial <- wheat_ML_trial %>% 
  dplyr::select(-c(names(which(colSums(is.na(wheat_ML_trial)) > 0)))) %>% 
  dplyr::mutate_if(is.character, as.factor) %>% 
  dplyr::rename(N_fertilizer=n_rate2, P_fertilizer=p_rate2) %>% 
  unique()
wheat_ML_trial$soil_type <- as.factor(wheat_ML_trial$soil_type)
str(wheat_ML_trial)
dim(wheat_ML_trial)
colnames(wheat_ML_trial)


#################################################################################################################
# 3. train the ML
#################################################################################################################
pathOut <- "~/shared-data/Data/Wheat/Intermediate/ML/NPrate"
if (!dir.exists(pathOut)){
  dir.create(file.path(pathOut), recursive = TRUE)
}

colnames(wheat_ML_trial)
response <- "blup"
predictors <- wheat_ML_trial |> dplyr::select(-c(blup, relativeHumid_month1, relativeHumid_month2, relativeHumid_month3,
                                                 solarRad_month1, solarRad_month2, solarRad_month3,
                                                 Tmin_month1, Tmin_month2, Tmin_month3)) |> names()


# setting up a local h2o cluster
h2o.init()

# convert our data frame into a special object that h2o can recognize
ML_inputData.h2o <- as.h2o(wheat_ML_trial)

#create a random training-test split of our data ## should be possible to do it by missing one
ML_inputData_split <- h2o.splitFrame(data = ML_inputData.h2o, ratios = 0.8, seed = 1234)
training_data <- ML_inputData_split[[1]]
test_data <- ML_inputData_split[[2]]


### 2. fitting the best model: given the best model is unidentifiable without supervision, it is not easy to automate this step *****************************************************************************
## (gradient boosting machine in the example) after tuning the hyper parameters
## Specify the hyper-parameter grid: test some values around the out put of h2o.get_best_model(autoML)
hyperparams_gbm <- list(
  ntrees = seq(800, 1000, 100),
  max_depth = seq(4, 10, 2)
)

# Train and tune the gradient boosting model
grid_gbm <- h2o.grid(
  algorithm = "gbm",
  x = predictors,
  y = response,
  grid_id = "hyperparams_gbm",
  hyper_params = hyperparams_gbm,
  training_frame = training_data,
  validation_frame = test_data,
  seed = 444
)


# Get the best hyper parameters
best_hyperParm <- h2o.getModel(grid_gbm@model_ids[[1]])
print(best_hyperParm@parameters) 

ntrees_gbm_optim <- best_hyperParm@parameters$ntrees # 100
max_depth_gbm_optim <- best_hyperParm@parameters$max_depth #14

### fit the model with the tuned hyper parameters: ntrees = 100 and max_depth=14
ML_gbm <- h2o.gbm(x = predictors,
                  y = response,
                  ntrees = ntrees_gbm_optim,
                  max_depth = max_depth_gbm_optim,
                  training_frame = training_data,
                  validation_frame = test_data,
                  keep_cross_validation_predictions = TRUE,
                  nfolds = 5,
                  seed = 444)

#Save the model for prediction
h2o.saveModel(object = ML_gbm, path = "/home/jovyan/shared-data/Data/Wheat/Intermediate/ML/NPrate/", force = TRUE)
# "/home/jovyan/shared-data/Data/Wheat/Intermediate/ML/NPrate/GBM_model_R_1721321242741_1"

rmse_r2_gbm <- data.frame(mae=round(h2o.mae(ML_gbm, train=TRUE, valid=TRUE), 0),
                          rmse = round(h2o.rmse(ML_gbm, train=TRUE, valid=TRUE), 3),
                          R_sq = round(h2o.r2(ML_gbm, train=TRUE, valid=TRUE), 3))
rmse_r2_gbm

bestMod_tuened <- data.frame(ntrees = ntrees_gbm_optim, maxDepth=max_depth_gbm_optim, R2 = rmse_r2_gbm$R_sq, rmse = rmse_r2_gbm$rmse)


GBM_valid <- test_data
GBM_valid$predResponse <- h2o.predict(object = ML_gbm, newdata = test_data)
GBM_valid <- as.data.frame(GBM_valid)
GBM_valid$Response <- GBM_valid[,which(names(GBM_valid)==response)]

ggplot(GBM_valid, aes(Response, predResponse)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  xlab("Measured Response") + ylab("predicted Response")+
  ggtitle("Gradient Boosting") +
  #xlim(0,max(ML_inputData$yield)) + ylim(0,max(ML_inputData$Yield))+ 
  theme_minimal()


#the variable importance plot
par(mar = c(1, 1, 1, 1)) #Expand the plot layout pane
h2o.varimp_plot(ML_gbm,num_of_features = 20)

# residula plot
h2o.residual_analysis_plot(ML_gbm,test_data)


# shap values = the direction of the relationship between our features and target
# e.g., high vlaues of total rainfall has positive contribution
h2o.shap_summary_plot(ML_gbm, test_data)

h2o.partialPlot(object = ML_gbm, newdata = test_data, cols = c("N_fertilizer", "P_fertilizer"))





#### 3. Random Forest *******************************************************************
## for the sake of comparison, we fit random forest and ANN as well 

## Specify the hyper-parameter grid
hyper_params <- list(
  ntrees = seq(20, 200, 10),
  max_depth = seq(4, 12, 2),
  mtries = c(2, 3, 4, 5, 6)
)

# Train and tune the random forest model
grid <- h2o.grid(
  algorithm = "randomForest",
  x = predictors,
  y = response,
  grid_id = "rf_grid",
  hyper_params = hyper_params,
  training_frame = training_data,
  validation_frame = test_data,
  seed = 444
)

# Get the best model from the grid search
best_model <- h2o.getModel(grid@model_ids[[1]])

# View the hyperparameters of the best model
print(best_model@parameters) ## mtry = 6, ntrees = 80, max_Depth = 12


ML_randomForest <- h2o.randomForest(x = predictors,
                                    y = response,
                                    ntrees =  best_model@parameters$ntrees,
                                    max_depth = best_model@parameters$max_depth,
                                    mtries =   best_model@parameters$mtries,
                                    training_frame = training_data,
                                    validation_frame = test_data,
                                    keep_cross_validation_predictions = TRUE,
                                    nfolds = 5,
                                    seed = 444)

h2o.saveModel(object = ML_randomForest, path = "/home/jovyan/ shared-data/Data/Wheat/result/models/final_model_RF", force = TRUE)
# "/home/jovyan/ shared-data/Data/Wheat/result/models/final_model_RF/DRF_model_R_1723035362901_2"


rmse_r2_randomforest <- data.frame(mae=round(h2o.mae(ML_randomForest, train=TRUE, valid=TRUE), 0),
                                   rmse =h2o.rmse(ML_randomForest, train=TRUE, valid=TRUE),
                                   R_sq = c(h2o.r2(ML_randomForest, train=TRUE, valid=TRUE)))
rmse_r2_randomforest

rf_valid <- test_data
rf_valid$predResponse <- h2o.predict(object = ML_randomForest, newdata = test_data)
rf_valid <- as.data.frame(rf_valid)
rf_valid$Response <- rf_valid[,which(names(rf_valid)==response)]

ggplot(rf_valid, aes(Response, predResponse)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "blue") +
  xlab("Measured yield") + ylab("predicted yield")+
  ggtitle("Random forest") +
  #xlim(0,max(ML_inputData$Yield)) + ylim(0,max(ML_inputData$Yield))+ 
  theme_minimal()


#the variable importance plot
par(mar = c(1, 1, 1, 1))#Expand the plot layout pane
h2o.varimp_plot(ML_randomForest, num_of_features = 20)

# residual plot
h2o.residual_analysis_plot(ML_randomForest,test_data)


# shap values = the direction of the relationship between our features and target
# e.g., high vlaues of total rainfall has positive contribution
h2o.shap_summary_plot(ML_randomForest, test_data)


h2o.partialPlot(object = ML_randomForest, newdata = test_data, cols = c("N_fertilizer", "P_fertilizer"))


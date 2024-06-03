
##################################################################################################################
# 1. Sourcing required packages -------------------------------------------
#################################################################################################################
#################################################################################################################
rm(list = ls())
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
## If not make use of the INS NPK as c-ovariate and model yield  as response function. INS will be aggregated by altitude class and region
## EDA, Geo-spatila data sourcing + GYGA, ML, is the procedure. 


## inputDataTrial[inputDataTrial$source=="EIAR_NPrate",] ## get EthioSis and weather data

inputDataTrial <- readRDS("~/shared-data/Data/Maize/fieldData/maize_modelReady.rds")
soildata_iSDA <- readRDS("~/shared-data/Data/Maize/geoSpatial/geo_4ML_trial/SoilDEM_PointData_trial.RDS")
soildata_EthSIS <- readRDS("~/shared-data/Data/Maize/geoSpatial/geo_4ML_trial/ethiosis_geo_maize.rds")
weatherdata <- readRDS("~/shared-data/Data/Maize/geoSpatial/geo_4ML_trial/weatherSummaries_trial.RDS")
weatherdata <- weatherdata[, -c(grep("windSpeed", names(weatherdata)), grep("month8", names(weatherdata)))]
weatherdata2 <- readRDS("~/shared-data/Data/Maize/geoSpatial/geo_4ML_trial/2023/NP_rateWeatherSummaries_trial.RDS")
weatherdata2 <- weatherdata2[, names(weatherdata)]
weatherdata <- rbind(weatherdata, weatherdata2)

tids <- unique(soildata_EthSIS$trial_id)


dim(inputDataTrial)
inputDataTrial <- inputDataTrial %>% 
  dplyr::select(-c(source, year, NAME_1, long2, lat2, WLY_wet, WLY_dry,
                   grain_yield_kgpha, treatment_id, ref_trt, yield_diff, COUNTRY)) %>% 
  unique()
dim(inputDataTrial)
inputDataTrial[inputDataTrial$trial_id == "34.38_9.54_2009_P_calibration NSRC", ]

colnames(inputDataTrial)



soildata_iSDA <- soildata_iSDA %>% 
  dplyr::select(-c("country", "NAME_1", "NAME_2","lon","lat")) %>%
  dplyr::rename(trial_id = ID) %>% 
  unique()

topo_data <- soildata_iSDA |> dplyr::select(trial_id, altitude, TPI, TRI) |>
  distinct(trial_id, .keep_all = TRUE)
dim(topo_data)
colnames(topo_data)

soildata_EthSIS <- soildata_EthSIS %>% 
  dplyr::select(-c(source, NAME_1, NAME_2,NAME_3,long2,lat2, year, 
                   ref_trt, grain_yield_kgpha, blup, treatment_id, n_rate2, p_rate2,
                   k_rate2, yield_diff)) %>%
  distinct(trial_id, .keep_all = TRUE)
dim(soildata_EthSIS)
colnames(soildata_EthSIS)



## you should test the model accuracy using the soil data fro iSDA and EthiSis and use the better one
maize_ML_data <- merge(inputDataTrial, soildata_EthSIS, by = "trial_id", all.x=TRUE)
maize_ML_data[maize_ML_data$trial_id == "34.38_9.54_2009_P_calibration NSRC", ]
maize_ML_data <- merge(maize_ML_data, topo_data, by = "trial_id", all.x=TRUE)

dim(maize_ML_data)
colnames(maize_ML_data)

hist(soildata_iSDA$altitude)

weatherdata <- weatherdata %>% 
  dplyr::select(-c(NAME_1, NAME_2, plantingDate, harvestDate, pyear, latitude, longitude)) %>% 
  unique()
dim(weatherdata)
colnames(weatherdata)
names(weatherdata)[1] <- "trial_id"

weatherdata[weatherdata$trial_id == "34.38_9.54_2009_P_calibration NSRC", ]


maize_ML_data <- merge(maize_ML_data, weatherdata, by = "trial_id", all.x=TRUE)
maize_ML_data[maize_ML_data$trial_id == "34.38_9.54_2009_P_calibration NSRC", ]


colnames(maize_ML_data)
maize_ML_data <- unique(maize_ML_data) ## we lost a serious amount of data here
dim(maize_ML_data)
unique(maize_ML_data$K_rf)
unique(maize_ML_data[is.na(maize_ML_data$K_rf), ]$trial_id)

### remove variables with NA values, if we take complete case, because of the difference in growing season there are quite some data points with NA weather data for later omonths 
maize_ML_data <- maize_ML_data[maize_ML_data$trial_id %in% tids, ]

maize_ML_trial <- maize_ML_data %>% 
  dplyr::select(-c(names(which(colSums(is.na(maize_ML_data)) > 0)))) %>% 
  dplyr::mutate_if(is.character, as.factor) 


dim(maize_ML_trial)
colnames(maize_ML_data)
colnames(maize_ML_trial)

##### create topography and control classes, ...
## create topo- class: ICRISAT to provide information how they are doing it, we have info from CIAT already
## for now I made it at step of 250 m altitude difference 
ds_alt <- maize_ML_trial %>%
  dplyr::group_by(trial_id) %>%
  dplyr::summarise(altClass = median(altitude)) %>%
  mutate(altClass = cut(altClass, c(-Inf, 1750, 2000, 2250, 2500, 2700, 3000, Inf), labels = c("Altclass1", "Altclass2", "Altclass3", "Altclass4", "Altclass5", "Altclass6", "Altclass7")))%>%
  unique()
head(ds_alt)

maize_ML_trial <- merge(maize_ML_trial, ds_alt, by="trial_id")
maize_ML_trial <- maize_ML_trial %>% dplyr::select(-c(trial_id))
dim(maize_ML_trial)
colnames(maize_ML_trial)

#Top 15 variables 
top_vars <- c("NAME_3", "n_rate2", "p_rate2", "Rainfall_month2", "Tmin_month1",
              "Tmin_month3", "Ca_sat_rf", "P_rf", "S_rf", "relativeHumid_month2",
              "Rainfall_month3", "solarRad_month2", "Rainfall_month1", "B_rf",
              "relativeHumid_month3", "blup")
#################################################################################################################
# 3. train the ML
#################################################################################################################
pathOut <- "~/shared-data/Data/Maize/Intermediate/grainwt"
if (!dir.exists(pathOut)){
  dir.create(file.path(pathOut), recursive = TRUE)
}

ML_inputData <- maize_ML_trial[, c(top_vars)]
ML_inputData <- ML_inputData |> unique()
colnames(ML_inputData)
# ML_inputData$CLIMATEZONE <- as.factor(ML_inputData$CLIMATEZONE)
# ML_inputData <-  ML_inputData |> dplyr::select(-c(WLY_dry, WLY_wet)) %>% unique()
dim(ML_inputData)
str(ML_inputData)
colnames(ML_inputData)

response <- "blup"
predictors <- ML_inputData |> names()
predictors <- predictors[!predictors %in% c("Ca_rf", "Ca_Mg", "Mg_rf", "Mg_sat_rf", "MnAl_rf", "Zn_rf", "Cu_rf","Fe_rf", "K_Mg", "blup")]

###############################################################################
###############################################################################
## test the different ML models 

# setting up a local h2o cluster
h2o.init()

# convert our data frame into a special object that h2o can recognize
ML_inputData.h2o <- as.h2o(ML_inputData)

#create a random training-test split of our data ## should be possible to do it by missing one
ML_inputData_split <- h2o.splitFrame(data = ML_inputData.h2o, ratios = 0.8, seed = 1234)
training_data <- ML_inputData_split[[1]]
test_data <- ML_inputData_split[[2]]


### 1. fitting the best model: given the best model is unidentifiable without supervision, it is not easy to automate this step *****************************************************************************
## (gradient boosting machine in the example) after tuning the hyper parameters
## Specify the hyper-parameter grid: test some values around the out put of h2o.get_best_model(autoML)
hyperparams_gbm <- list(
  ntrees = seq(500, 1000, 100), ### is tested for diff nr trees with seq(20, 200, 20), seq(200, 500, 50) and seq(500, 1000, 100)
  max_depth = seq(4, 8, 2)
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


saveRDS(grid_gbm, paste(pathOut, "grid_gbm_15vars.rds", sep=""))

#grid_gbm <- readRDS(paste(pathOut, "grid_gbm.rds", sep=""))
# Get the best hyper parameters
best_hyperParm <- h2o.getModel(grid_gbm@model_ids[[1]])
print(best_hyperParm@parameters) 

ntrees_gbm_optim <- best_hyperParm@parameters$ntrees #1000
max_depth_gbm_optim <- best_hyperParm@parameters$max_depth #4

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


pathOut <- "~/shared-data/Data/Maize/Intermediate/grainwt"
model_path <- h2o.saveModel(object = ML_gbm, path = pathOut, force = TRUE)
print(model_path)
# load the model
saved_model <- h2o.loadModel(model_path)

# download the model built above to your local machine
my_local_model <- h2o.download_model(ML_gbm, path = pathOut)

# upload the model that you just downloded above to the H2O cluster
uploaded_model <- h2o.upload_model(my_local_model)



rmse_r2_gbm <- data.frame(mae=round(h2o.mae(ML_gbm, train=TRUE, valid=TRUE), 0),
                          rmse = round(h2o.rmse(ML_gbm, train=TRUE, valid=TRUE), 0),
                          R_sq = round(h2o.r2(ML_gbm, train=TRUE, valid=TRUE), 2))
rmse_r2_gbm
# mae rmse R_sq
# train  65   97 1.00
# valid 208  337 0.97

h2o.residual_analysis_plot(ML_gbm,test_data)

bestMod_tuened <- data.frame(ntrees = ntrees_gbm_optim, maxDepth=max_depth_gbm_optim, R2 = rmse_r2_gbm$R_sq, rmse = rmse_r2_gbm$rmse)


h2o.partialPlot(object = ML_gbm, newdata = test_data, cols = c("n_rate2", "p_rate2"))

#the variable importance plot
par(mar = c(1, 1, 1, 1))#Expand the plot layout pane
h2o_varimp <- h2o.varimp_plot(ML_gbm, num_of_features = 15)



GBM_valid <- test_data
GBM_valid$predResponse <- h2o.predict(object = ML_gbm, newdata = test_data)
GBM_valid <- as.data.frame(GBM_valid)
GBM_valid$Response <- GBM_valid[,which(names(GBM_valid)==response)]

ggplot(GBM_valid, aes(Response, predResponse)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  xlab("Measured Yield") + ylab("predicted yield")+
  ggtitle("Gradient Boosting") +
  #xlim(0,max(ML_inputData$yield)) + ylim(0,max(ML_inputData$Yield))+ 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


# shap values = the direction of the relationship between our features and target
# e.g., high vlaues of total rainfall has positive contribution
h2o.shap_summary_plot(ML_gbm, test_data)




#### 3. Random Forest *******************************************************************
## for the sake of comparison, we fit random forest and ANN as well 

## Specify the hyper-parameter grid
hyper_params <- list(
  ntrees = seq(500, 1000, 100),
  max_depth = seq(4, 8, 2), # max_depth limits the depth of the tree, which in turn limits the complexity of the tree and prevents overfitting. A smaller max_depth value restricts the growth of trees, resulting in simpler models with less chance of capturing noise in the data
  mtries = c(3, 4, 5) # the number of features considered for splitting at each node during the construction of each tree in the forest
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

saveRDS(grid, paste(pathOut, "grid_RF.rds", sep=""))

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

h2o.saveModel(object = ML_randomForest, path = pathOut, force = TRUE)
h2o.residual_analysis_plot(ML_randomForest,test_data)

rmse_r2_randomforest <- data.frame( rmse =h2o.rmse(ML_randomForest, train=TRUE, valid=TRUE),
                                    R_sq = c(h2o.r2(ML_randomForest, train=TRUE, valid=TRUE)))
rmse_r2_randomforest

rf_valid <- test_data
rf_valid$predResponse <- h2o.predict(object = ML_randomForest, newdata = test_data)
rf_valid <- as.data.frame(rf_valid)
rf_valid$Response <- rf_valid[,which(names(rf_valid)==response)]

ggplot(rf_valid, aes(Response, predResponse)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "blue") +
  xlab("Measured yield") + ylab("Predicted yield")+
  ggtitle("Random forest") +
  xlim(500,10000) + ylim(500,10000)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size=12))

#the variable importance plot
par(mar = c(1, 1, 1, 1))#Expand the plot layout pane
h2o.varimp_plot(ML_randomForest)


# shap values = the direction of the relationship between our features and target
# e.g., high vlaues of total rainfall has positive contribution
h2o.shap_summary_plot(ML_randomForest, test_data)


### 4. deep learning / ANN ********************************************************

# Specify the hyper-parameter grid
hyper_params_ANN <- list(
  hidden = list(c(100, 100)),
  activation = c("Rectifier", "Tanh", "Maxout"),
  epochs = c(4, 20, 2),
  l1 = c(0, 0.001),
  l2 = c(0, 0.001)
)



# Train and tune the deep learning model
grid_ANN <- h2o.grid(
  algorithm = "deeplearning",
  x = predictors,
  y = response,
  grid_id = "dl_grid",
  hyper_params = hyper_params_ANN,
  training_frame = training_data,
  validation_frame = test_data,
  seed = 444
)

saveRDS(grid_ANN, paste(pathOut, "hyper_params_ANN.rds", sep=""))




# Get the best model from the grid search
best_HP_ANN <- h2o.getModel(grid_ANN@model_ids[[1]])# epochs = 20, l1=l2 = 0.001, distribution = guassian, hidden = 100
print(best_HP_ANN@parameters)


# run the model with optimized hyper-parameters
ML_ANN <- h2o.deeplearning(x = predictors,
                           y = response,
                           hidden =  c(100, 100),
                           epochs =  best_HP_ANN@parameters$epochs,
                           reproducible = TRUE,
                           activation = best_HP_ANN@parameters$activation,
                           l2 = 0.001,
                           seed = 444,
                           training_frame = training_data,
                           validation_frame = test_data,
                           keep_cross_validation_predictions = TRUE,
                           nfolds = 5)


h2o.saveModel(object = ML_ANN, path = pathOut, force = TRUE)
h2o.residual_analysis_plot(ML_ANN,test_data)

rmse_r2_ANN <- data.frame( rmse = h2o.rmse(ML_ANN, train=TRUE, valid=TRUE),
                           R_sq = c(h2o.r2(ML_ANN, train=TRUE, valid=TRUE)))
rmse_r2_ANN ## random forest is better 

ANN_valid <- test_data
ANN_valid$predYield <- h2o.predict(object = ML_ANN, newdata = test_data)
ANN_valid <- as.data.frame(ANN_valid)
ggplot(ANN_valid, aes(blup, predYield)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "blue") +
  xlab("Measured yield") + ylab("Predicted yield")+
  ggtitle("Artificail Nueral Network") +
  # annotate("text", x = 1500, y = 7500, label = "r2 = 0.63")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size=12))


#the variable importance plot
par(mar = c(1, 1, 1, 1)) #Expand the plot layout pane
h2o.varimp_plot(ML_ANN)

h2o.shap_summary_plot(ML_ANN, test_data)

#####################################################################

# Train a stacked ensemble using the default metalearner algorithm
stack <- h2o.stackedEnsemble(x = predictors,
                             y = response,
                             training_frame = training_data,
                             base_models = list(ML_randomForest, ML_gbm))

h2o.auc(h2o.performance(stack, test_data))



#gbm
stack_gbm <- h2o.stackedEnsemble(x = predictors,
                                 y = response,
                                 training_frame = training_data,
                                 base_models = list(ML_randomForest, ML_gbm),
                                 metalearner_algorithm = "gbm")
h2o.auc(h2o.performance(stack_gbm, test_data))

#rf
stack_rf <- h2o.stackedEnsemble(x = predictors,
                                y = response,
                                training_frame = training_data,
                                base_models = list(ML_randomForest, ML_gbm),
                                metalearner_algorithm = "drf")
h2o.auc(h2o.performance(stack_rf, test_data))






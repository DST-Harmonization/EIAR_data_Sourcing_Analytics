
#################################################################################################################
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
inputDataTrial <- readRDS("~/shared-data/Data/Maize/fieldData/maize_modelReady.rds")
soildata_EthSISN <- readRDS("~/shared-data/Data/Maize/geoSpatial/geo_4ML_trial/ethiosis_extract_maize_new.RDS")
names(soildata_EthSISN) <- c("ID","B_rf","Ca_rf","Ca_Mg","Ca_sat_rf","CEC_rf","Cu_rf","EC_rf", "Fe_rf","K_rf",
                             "K_Mg","Mg_rf","Mg_sat_rf","MnAl_rf","N_rf","OM_pct_rf", "P_rf","pH_rf","S_rf",
                             "Zn_rf","long2","lat2","trial_id")
weatherdata <- readRDS("~/shared-data/Data/Maize/geoSpatial/geo_4ML_trial/weatherSummaries_trial.RDS")
weatherdata <- weatherdata |> dplyr::mutate(NAME_1 = replace(NAME_1, NAME_1 == "Southern Nations, Nationalities", "SNNP"))
TopoData <- readRDS("~/shared-data/Data/Maize/geoSpatial/geo_4ML_trial/TopoData.RDS")

inputDataTrial <- inputDataTrial %>% 
  dplyr::select(-c(grain_yield_kgpha, treatment_id, ref_trt, yield_diff)) %>% 
  unique()

## merge field data and weather data 
WF <- merge(inputDataTrial,weatherdata, by.x=c("NAME_1", "NAME_2","long2","lat2","year","trial_id"), by.y=c("NAME_1", "NAME_2","longitude","latitude","pyear","ID"), all.x=TRUE )
unique(WF[is.na(WF$totalRF), ]$trial_id)

## add soil EthiSIS data
SWF <- merge(WF,soildata_EthSISN, by=c("trial_id","long2","lat2"))

## add topography data
SWFT <- merge(SWF,TopoData, by.x=c("long2","lat2"), by.y=c("lon", "lat"), all.x=TRUE)

## drop Na columns
maize_ML_data <- SWFT %>% 
  select_if(~sum(!is.na(.)) > 0)


### drop weather data beyond month 4, because there are a lot of NA values there 
maize_ML_data <-maize_ML_data %>% dplyr::select(-c(names(maize_ML_data)[c(grep("_month4", names(maize_ML_data)), grep("_month5", names(maize_ML_data)), 
                                                                          grep("_month6", names(maize_ML_data)), grep("_month7", names(maize_ML_data)), grep("_month8", names(maize_ML_data)))])) %>%
  dplyr::select(-c(ID, Cu_rf, EC_rf, Fe_rf)) %>% 
  unique()

maize_ML_trial <- maize_ML_data %>% dplyr::select(-c(trial_id))
dim(maize_ML_trial)
colnames(maize_ML_trial)


top_vars <- c("NAME_3", "n_rate2", "p_rate2", "totalRF", "nrRainyDays", 
              "Rainfall_month1","Tmax_month1","Tmin_month1",
              "Rainfall_month2","Tmax_month2","Tmin_month2",
              "Rainfall_month3","Tmax_month3","Tmin_month3",
              "B_rf", "Ca_Mg", "CEC_rf","K_rf","K_Mg","Mg_rf",
              "Mg_sat_rf","MnAl_rf", "N_rf","OM_pct_rf","P_rf",
              "pH_rf","altitude","slope","TPI", "TRI", "blup") 

top15 <- c("n_rate2", "p_rate2", "totalRF", "nrRainyDays", 
           "Rainfall_month1","Tmax_month1","Tmin_month1",
           "Rainfall_month2", "Tmin_month2",
           "Rainfall_month3","Tmax_month3",
           "B_rf", "CEC_rf", 
           "MnAl_rf", "OM_pct_rf",
           "pH_rf","altitude","slope", "TRI",  "blup") 


#################################################################################################################
# 3. train the ML
#################################################################################################################
pathOut <- "/home/jovyan/shared-data/Data/Maize/Intermediate/SecondRun"
if (!dir.exists(pathOut)){
  dir.create(file.path(pathOut), recursive = TRUE)
}

ML_inputData <- maize_ML_trial[, c(top15)]
ML_inputData <- ML_inputData |> na.omit() |> unique()
colnames(ML_inputData)
dim(ML_inputData)
str(ML_inputData)
colnames(ML_inputData)

names(ML_inputData) <- c("N_fert","P_fert","totalRF","nrRainyDays","Rainfall_M1", 
                         "Tmax_M1", "Tmin_M1", "Rainfall_M2", "Tmin_M2", 
                         "Rainfall_M3", "Tmax_M3", "B_rf", "CEC_rf", "MnAl_rf",
                         "OM_pct_rf", "pH_rf", "altitude","slope","TRI", "blup")


dim(ML_inputData)
response <- "blup"
predictors <- ML_inputData |> names()
predictors <- predictors[!predictors %in% c("blup")]
predictors <- predictors[!predictors %in% c("Ca_rf", "Ca_Mg", "B_rf",  "Mg_sat_rf", "MnAl_rf", "Zn_rf", "Cu_rf","Fe_rf","blup", "NAME_3", "relativeHumid_month2","relativeHumid_month3")]

###############################################################################
###############################################################################
## test the different ML models 
# setting up a local h2o cluster
h2o.init()

# convert our data frame into a special object that h2o can recognize
ML_inputData.h2o <- as.h2o(ML_inputData)

#create a random training-test split of our data ## should be possible to do it by missing one
ML_inputData_split <- h2o.splitFrame(data = ML_inputData.h2o, ratios = 0.8, seed = 4444)
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


saveRDS(grid_gbm, paste(pathOut, "grid_gbm_allVars.rds", sep=""))

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


pathOut <- "/home/jovyan/shared-data/Data/Maize/Intermediate/grainwt"
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
#        mae rmse R_sq
# train  74  102 1.00
# valid 250  454 0.95

h2o.residual_analysis_plot(ML_gbm,test_data)

bestMod_tuened <- data.frame(ntrees = ntrees_gbm_optim, maxDepth=max_depth_gbm_optim, R2 = rmse_r2_gbm$R_sq, rmse = rmse_r2_gbm$rmse)


h2o.partialPlot(object = ML_gbm, newdata = test_data, cols = c("N_fert","P_fert"))

#the variable importance plot
par(mar = c(1, 1, 1, 1))#Expand the plot layout pane
h2o_varimp <- h2o.varimp_plot(ML_gbm, num_of_features = 20)



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

rmse_r2_randomforest <-  data.frame(mae=round(h2o.mae(ML_randomForest, train=TRUE, valid=TRUE), 0),
                                    rmse = round(h2o.rmse(ML_randomForest, train=TRUE, valid=TRUE), 0),
                                    R_sq = round(h2o.r2(ML_randomForest, train=TRUE, valid=TRUE), 2))

rmse_r2_randomforest
#       mae rmse R_sq
# train 660  865 0.82
# valid 665  856 0.82

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
h2o.varimp_plot(ML_randomForest, num_of_features = 20)

h2o.partialPlot(object = ML_randomForest, newdata = test_data, cols = c("N_fert","P_fert"))

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
h2o.partialPlot(object = ML_ANN, newdata = test_data, cols = c("N_fert","P_fert"))

rmse_r2_ANN <- data.frame(mae=round(h2o.mae(ML_ANN, train=TRUE, valid=TRUE), 0),
                          rmse = h2o.rmse(ML_ANN, train=TRUE, valid=TRUE),
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
h2o.varimp_plot(ML_ANN, num_of_features = 15)

h2o.shap_summary_plot(ML_ANN, test_data)

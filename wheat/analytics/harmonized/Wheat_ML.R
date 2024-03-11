
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

inputDataTrial <- readRDS("C:\\Users\\ATilaye\\OneDrive - CGIAR\\Desktop\\DST_Harmonization\\response_functions\\data\\input\\wheat_modelReady.rds")
soildata_iSDA <- readRDS("C:\\Users\\ATilaye\\OneDrive - CGIAR\\Desktop\\DST_Harmonization\\response_functions\\data\\input\\SoilDEM_PointData_trial.RDS")
soildata_EthSIS <- readRDS("C:\\Users\\ATilaye\\OneDrive - CGIAR\\Desktop\\DST_Harmonization\\response_functions\\data\\input\\ethiosis_geo_wheat.rds")
weatherdata <- readRDS("C:\\Users\\ATilaye\\OneDrive - CGIAR\\Desktop\\DST_Harmonization\\response_functions\\data\\input\\weatherSummaries_trial.RDS")


# inputDataTrial <- inputDataTrial %>%
#   dplyr::select(-c(yield_diff, treatment_id)) %>%
#   dplyr::rename(lon = long2, lat = lat2)

inputDataTrial <- inputDataTrial %>%
  dplyr::rename(lon = long2, lat = lat2)

soildata_iSDA <- soildata_iSDA %>% 
  dplyr::select(-c("country", "NAME_1", "NAME_2","lon","lat"   )) %>%
  dplyr::rename(trial_id = ID) %>% 
  unique()


## you should test the model accuracy using the soil data fro iSDA and EthiSis and use the better one
wheat_ML_data <- merge(inputDataTrial, soildata_iSDA, by="trial_id")
hist(soildata_iSDA$altitude)

weatherdata <- weatherdata %>% 
  dplyr::select(-c(NAME_1, NAME_2, plantingDate, harvestDate, pyear, latitude, longitude)) %>% 
  dplyr::rename(trial_id = ID) %>% 
  unique

wheat_ML_data <- merge(wheat_ML_data, weatherdata, by = "trial_id")


### remove variables with NA values, if we take complete case, because of the difference in growing season there are quite some data points wiht NA weatehr data for later omonths 

wheat_ML_trial <- wheat_ML_data %>% 
  dplyr::select(-c(names(which(colSums(is.na(wheat_ML_data)) > 0)))) %>% 
  dplyr::mutate_if(is.character, as.factor) %>% 
  unique()
str(wheat_ML_trial)
dim(wheat_ML_trial)
colnames(wheat_ML_trial)

##### create topography and control classes, ...
## create topo- class: ICRISAT to provide information how they are doing it, we have info from CIAT already
## for now I made it at step of 250 m altitude difference 
ds_alt <- wheat_ML_trial %>%
  dplyr::group_by(trial_id) %>%
  dplyr::summarise(altClass = median(altitude)) %>%
  mutate(altClass = cut(altClass, c(-Inf, 1750, 2000, 2250, 2500, 2700, 3000, Inf), labels = c("Altclass1", "Altclass2", "Altclass3", "Altclass4", "Altclass5", "Altclass6", "Altclass7")))%>%
  unique()
head(ds_alt)

wheat_ML_trial <- merge(wheat_ML_trial, ds_alt, by="trial_id")

wheat_ML_trial <- wheat_ML_trial %>% dplyr::select(-c(trial_id, source, year, ref_trt))



#################################################################################################################
# 3. train the ML
#################################################################################################################
pathOut <- "~/shared-data/Data/Wheat/Intermediate/ML"
if (!dir.exists(pathOut)){
  dir.create(file.path(pathOut), recursive = TRUE)
}

ML_inputData <- wheat_ML_trial
dim(ML_inputData)
colnames(ML_inputData)
response <- "blup"
#predictors <- ML_inputData %>% dplyr::select(-c(TLID, P_base_supply, K_base_supply, lon, lat)) %>% names()
ML_inputData <-  ML_inputData |> dplyr::select(-c(lon, lat, NAME_1, NAME_2, NAME_3, grain_yield_kgpha, yield_diff, treatment_id)) 
predictors <- ML_inputData |> select(-blup) |> names()
dim(ML_inputData)
colnames(ML_inputData)

#' @param ML_inputData the input data with response and predictor variables
#' @param predictors list of predictor variables 
#' @param response the dependent variable
#' @param nthread By default, h2o will spun all available CPU’s but you can specify a specific number of CPU’s to initialize using nthread
train_ML_H20Auto <- function(ML_inputData,  predictors=NULL, response= NULL, nthread){
  
  # setting up a local h2o cluster
  h2o.init()
  
  # convert our data frame into a special object that h2o can recognize
  ML_inputData.h2o <- as.h2o(ML_inputData)
  
  #create a random training-test split of our data ## should be possible to do it by missing one
  ML_inputData.h2o <- as.h2o(ML_inputData)
  ML_inputData_split <- h2o.splitFrame(data = ML_inputData.h2o, ratios = 0.8, seed = 1234)
  training_data <- ML_inputData_split[[1]]
  test_data <- ML_inputData_split[[2]]
  
  
  ################# Testing different models #################
  ### 1. Auto ML ****************************************************************************************
  ## A quick and raw way to look at the way different models perform on the data:
  autoML <- h2o.automl(x = predictors,
                       y = response,
                       training_frame = training_data,
                       validation_frame = test_data,
                       seed = 444,
                       include_algos = c("DRF", "GBM", "DeepLearning", "XGBoost", "StackedEnsemble"))
  
  #setwd("~/shared-data/Data/Wheat/result/")
  #saveRDS(autoML, "AutoML_5model_wheat.rds")
  #autoML <- readRDS("~/shared-data/Data/Wheat/result/models/AutoML_5model_wheat.rds")
  # Get the leaderboard with model performance to select the top models
  leaderboard <- autoML@leaderboard
  leaderboard <- as.data.frame(leaderboard)
  print(leaderboard)
  bestModID <- leaderboard$model_id[1]
  
  #It seems the gradient boosting machine followed by Stacked Ensemble all models are best performers. 
  ## Get more information on the best model by calling:
  bestMOd <- h2o.get_best_model(autoML)
  ntrees <- bestMOd@model$model_summary$number_of_trees
  minDepth <- bestMOd@model$model_summary$min_depth
  maxDepth <- bestMOd@model$model_summary$max_depth
  
  bestMod_info <- data.frame(modelID= bestModID, ntrees = ntrees, minDepth=minDepth, maxDepth=maxDepth)
  
  
  ### 2. fitting the best model: given the best model is unidentifiable without supervision, it is not easy to automate this step *****************************************************************************
  ## (gradient boosting machine in the example) after tuning the hyper parameters
  ## Specify the hyper-parameter grid: test some values around the out put of h2o.get_best_model(autoML)
  hyperparams_gbm <- list(
    ntrees = seq(20, 100, 10),
    max_depth = seq(4, 16, 2)
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
  
  #saveRDS(grid_gbm, "grid_gbm.rds")
  #grid_gbm <- readRDS("./models/grid_gbm.rds")
  # Get the best hyper parameters
  best_hyperParm <- h2o.getModel(grid_gbm@model_ids[[1]])
  print(best_hyperParm@parameters) 
  
  ntrees_gbm_optim <- best_hyperParm@parameters$ntrees # 30
  max_depth_gbm_optim <- best_hyperParm@parameters$max_depth #4
  
  ### fit the model with the tuned hyper parameters: ntrees = 40 and max_depth=8
  ML_gbm <- h2o.gbm(x = predictors,
                    y = response,
                    ntrees = ntrees_gbm_optim,
                    max_depth = max_depth_gbm_optim,
                    training_frame = training_data,
                    validation_frame = test_data,
                    keep_cross_validation_predictions = TRUE,
                    nfolds = 5,
                    seed = 444)
  
  rmse_r2_gbm <- data.frame(rmse = round(h2o.rmse(ML_gbm, train=TRUE, valid=TRUE), 3),
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
  par(mar = c(1, 1, 1, 1))#Expand the plot layout pane
  h2o.varimp_plot(ML_gbm)
  
  
  # shap values = the direction of the relationship between our features and target
  # e.g., high vlaues of total rainfall has positive contribution
  h2o.shap_summary_plot(ML_gbm, test_data)
  
  
  
  
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
    xlab("Measured yield") + ylab("predicted yield")+
    ggtitle("Random forest") +
    #xlim(0,max(ML_inputData$Yield)) + ylim(0,max(ML_inputData$Yield))+ 
    theme_minimal()
  
  
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
  
  # Get the best model from the grid search
  best_HP_ANN <- h2o.getModel(grid_ANN@model_ids[[1]])# epochs = 20, l1=l2 = 0.001, distribution = guassian, hidden = 100
  print(best_HP_ANN@parameters)
  
  
  # run the model with optimized hyper-parameters
  ML_ANN <- h2o.deeplearning(x = predictors,
                             y = response,
                             hidden =  c(100, 100),
                             epochs =  best_HP_ANN@parameters$epochs,
                             reproducible = TRUE,
                             activation = "Maxout",
                             l2 = 0.001,
                             seed = 444,
                             training_frame = training_data,
                             validation_frame = test_data,
                             keep_cross_validation_predictions = TRUE,
                             nfolds = 5)
  
  rmse_r2_ANN <- data.frame( rmse = h2o.rmse(ML_ANN, train=TRUE, valid=TRUE),
                             R_sq = c(h2o.r2(ML_ANN, train=TRUE, valid=TRUE)))
  rmse_r2_ANN ## random forest is better 
  
  ANN_valid <- test_data
  ANN_valid$predYield <- h2o.predict(object = ML_ANN, newdata = test_data)
  ANN_valid <- as.data.frame(ANN_valid)
  ggplot(ANN_valid, aes(grain_yield_kgpha, predYield)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, col = "blue") +
    xlab("Measured yield") + ylab("predicted yield")+
    ggtitle("Artificail Nueral Network") +
    annotate("text", x = 1500, y = 7500, label = "r2 = 0.63")+
    #xlim(0,max(ML_inputData$Yield)) + ylim(0,max(ML_inputData$Yield))+ 
    # add also rmse 
    theme_minimal()
  
  #the variable importance plot
  par(mar = c(1, 1, 1, 1)) #Expand the plot layout pane
  h2o.varimp_plot(ML_ANN)
  
  
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
  
  
  
  return(list(bestMod_info, ))
  
  
}


#Explanation Plotting Functions#
#There are a number of individual plotting functions that are used inside the explain() function. Some of these functions take a group of models as input and others just evaluate a single model at a time. The following functions take a list of models (including an AutoML object or an H2OFrame with model_id column, e.g., the Leaderboard) as input:
#When h2o.explain() is provided a single model, we get the following global explanations:
#Residual Analysis (regression only)
#Variable Importance
#Partial Dependence (PD) Plots
#Individual Conditional Expectation (ICE) Plots
# Methods for an AutoML object
h2o.varimp_heatmap(autoML)
h2o.model_correlation_heatmap(autoML,
                              test_data,
                              cluster_models = TRUE,
                              triangular = TRUE)
h2o.pd_multi_plot(autoML,
                  test_data,
                  column = "N")

#These functions take a single H2O model as input, here we start with gbm:
h2o.residual_analysis_plot(ML_gbm,test_data)
h2o.varimp_plot(ML_gbm)
h2o.shap_explain_row_plot(ML_gbm,test_data,row_index = 1)
h2o.shap_summary_plot(ML_gbm,test_data)
h2o.pd_plot(ML_gbm,test_data,column = "N")
h2o.ice_plot(ML_gbm,test_data,column = "N")

#Explanation with ML_randomForest

h2o.residual_analysis_plot(ML_randomForest,test_data)
h2o.varimp_plot(ML_randomForest)
h2o.shap_explain_row_plot(ML_randomForest,test_data,row_index = 1)
h2o.shap_summary_plot(ML_randomForest,test_data)
h2o.pd_plot(ML_randomForest,test_data,column = "N")
h2o.ice_plot(ML_randomForest,test_data,column = "N")








valData <- readRDS(paste("/home/jovyan/agwise/AgWise_Data/fieldData_analytics/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/", "field_geoSpatiallinked_geoSpatial_AOI.RDS", sep=""))

Nrate_pred <- c(seq(min(trData$N), (max(trData$N)+10), 10))
Prate_pred <- c(seq(min(trData$P), (max(trData$P)+10), 10))
Krate_pred <- c(seq(min(trData$K), (max(trData$K)+10), 10))


expand_grid(Nrate_pred, Prate_pred, Krate_pred)



########################################
## 1. H2O supports the following supervised algorithms:
#1- AutoML: Automatic Machine Learning
#2- Cox Proportional Hazards (CoxPH)
#3- Deep Learning (Neural Networks)
#4- Distributed Random Forest (DRF)
#5- Generalized Linear Model (GLM)
#6- Isotonic Regression
#7- ModelSelection
#8- Generalized Additive Models (GAM)
#9- ANOVA GLM
#10-Gradient Boosting Machine (GBM)
#11-Naïve Bayes Classifier
#12-RuleFit
#13-Stacked Ensembles
#14-Support Vector Machine (SVM)
#15-Distributed Uplift Random Forest (Uplift DRF)
#16-XGBoost
###let’s fit a Neural Network, using h2o.deeplearning:


## random forest hyper parameters
# The h2o.randomForest algorithm in H2O provides various hyper-parameters that you can tune to customize the behavior of the random forest model. Here are some commonly used hyperparameters for h2o.randomForest:
# ntrees: The number of trees to grow in the random forest. Typically, a higher number of trees can improve model performance but increase training time.
# max_depth: The maximum depth of each tree in the random forest. Controlling the depth helps control the complexity of the trees and can prevent overfitting.
# mtries: The number of features randomly selected at each split. It determines the number of features available for splitting at each node and affects the diversity and accuracy of the trees.


##ANN hyper parameters
# the h2o.deeplearning algorithm in H2O has several hyper-parameters that you can tune to customize the behavior of the deep learning model. Here are some commonly used hyperparameters for h2o.deeplearning:
# hidden: The number and sizes of hidden layers in the neural network. You can specify a vector of integers to define the number of neurons in each hidden layer.
# activation: The activation function to use in the hidden layers. Options include "Rectifier" (default), "Tanh", "TanhWithDropout", "RectifierWithDropout", "Maxout", "MaxoutWithDropout", "ExpRectifier", and "ExpRectifierWithDropout".
# epochs: The number of passes over the training data (epochs) during training. More epochs can lead to better model performance but may increase training time.
# l1: The L1 regularization strength. It helps to control the complexity of the model and prevent overfitting by adding an L1 penalty term to the loss function.
# l2: The L2 regularization strength. Similar to L1 regularization, it adds an L2 penalty term to the loss function to control model complexity and prevent overfitting.




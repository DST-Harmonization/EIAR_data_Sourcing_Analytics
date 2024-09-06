


source("~/shared-data/Scripts/generic/nutrientResponse/prediction.R")

scenario = c("below", "above", "normal")
crop = "Maize" 
## the fowlloing range of values are used only for training purposes otherwise the commented once below are use for actual work
nstart = 100 
nend = 250 
ninterval = 25
pstart = 0 
pend = 50
pinterval = 15 
# 
# nstart = 0 
# nend = 240 
# ninterval = 10
# pstart = 0 
# pend = 50
# pinterval = 5


model_crop = "GBM_model_R_1718103639209_2"## this should be replaced by the model a user trained when running the maize_ML.R


# zone_list <- c("Mirab Hararghe", "Misraq Harerge", "Misraq Shewa", "Mirab Shewa",
#                "Mirab Arsi", "Horo Guduru", "Ilubabor", "Mirab Welega", 
#                "Misraq Wellega", "Jimma", "Kelem Wellega", "Semen Gondar", 
#                "Misraq Gojjam", "Mirab Gojjam", "Debub Gondar", "Agew Awi",
#                "Sidama", "Gamo Gofa", "Debub Omo", "Derashe" , "Amaro", "Gedeo",
#                "Konso","Wolayita", "Gurage", "Silti", "Hadiya", "Kembata Tembaro",
#                "Alaba", "Metekel", "Asosa", "Keffa")

## for the training, select only one Woreda otherwise the whole commented list of Woredas are used for the actual work. 

zone_list <- c("Asosa")


pathModel <-  "/home/jovyan/shared-data/Data/Maize/Intermediate/grainwt" ## replace this path to where you write out your trained model
pathOut <- paste("~/shared-data/Data", crop, "result/geoSpatial", sep = "/") ## replace this with a path where you want to save the reuslt. 

for(k in 1:length(scenario)){
  print(scenario[k])
  predict_NPKY(scenario[k], crop=crop, nstart=nstart, nend=nend, ninterval=ninterval,
               pstart=pstart, pend=pend, pinterval=pinterval, model_crop=model_crop,
               zone_list=zone_list, pathModel=pathModel, pathOut=pathOut)
}

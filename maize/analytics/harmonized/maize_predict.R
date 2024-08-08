


source("~/shared-data/Scripts/generic/nutrientResponse/prediction.R")

scenario = c("below", "above", "normal")
crop = "Maize" 
nstart = 0 
nend = 240 
ninterval = 10
pstart = 0 
pend = 50
pinterval = 5 

model_crop = "GBM_model_R_1718103639209_2"## model with EthioSIS data with all NP rates added


zone_list <- c("Mirab Hararghe", "Misraq Harerge", "Misraq Shewa", "Mirab Shewa",
               "Mirab Arsi", "Horo Guduru", "Ilubabor", "Mirab Welega", 
               "Misraq Wellega", "Jimma", "Kelem Wellega", "Semen Gondar", 
               "Misraq Gojjam", "Mirab Gojjam", "Debub Gondar", "Agew Awi",
               "Sidama", "Gamo Gofa", "Debub Omo", "Derashe" , "Amaro", "Gedeo",
               "Konso","Wolayita", "Gurage", "Silti", "Hadiya", "Kembata Tembaro",
               "Alaba", "Metekel", "Asosa", "Keffa")


pathModel <-  "/home/jovyan/shared-data/Data/Maize/Intermediate/grainwt"
pathOut <- paste("~/shared-data/Data", crop, "result/geoSpatial", sep = "/")

for(k in 1:length(scenario)){
  print(scenario[k])
  predict_NPKY(scenario[k], crop=crop, nstart=nstart, nend=nend, ninterval=ninterval,
               pstart=pstart, pend=pend, pinterval=pinterval, model_crop=model_crop,
               zone_list=zone_list, pathModel=pathModel, pathOut=pathOut)
}

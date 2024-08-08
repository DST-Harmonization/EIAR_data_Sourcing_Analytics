
source("~/shared-data/Scripts/generic/nutrientResponse/prediction.R")

scenario = c("below", "above", "normal")
crop = "Wheat" 
nstart = 0 
nend = 260 
ninterval = 10
pstart = 0 
pend = 60
pinterval = 5 
model_crop = "GBM_model_R_1721321242741_1"## model with EthioSIS data with all NP rates added

zone_list <- c("Arsi", "Bale", "Misraq Shewa", "Misraq Wellega",  "Misraq Harerge", "Guji",
               "Horo Guduru", "Kelem Wellega", "North Shewa", "Addis Abeba", "Mirab Arsi",
               "Mirab Hararghe", "Mirab Shewa", "Mirab Shewa", "Mirab Gojjam",
               "Agew Awi", "Semen Gondar", "Debub Gondar", "Semen Wello", "Debub Wollo",
               "Wag Himra", "Sidama", "Gamo Gofa", "Gedeo", "Wolayita", "Gurage", "Hadiya",
               "Alaba", "Kembata Tembaro", "Silti", "Keffa", "Debub Omo", "Misraq Gojjam", 
               "Ilubabor", "Misraq Shewa","Mirab Welega", "East Gojjam ")

pathModel <- paste("/home/jovyan/shared-data/Data", crop, "Intermediate/ML/NPrate", sep = "/")
pathOut <- paste("~/shared-data/Data", crop, "result/geoSpatial/NPrate", sep = "/")

for(k in 1:length(scenario)){
  print(scenario[k])
  predict_NPKY(scenario[k], crop=crop, nstart=nstart, nend=nend, ninterval=ninterval,
               pstart=pstart, pend=pend, pinterval=pinterval, model_crop=model_crop,
               zone_list=zone_list, pathModel=pathModel, pathOut=pathOut)
}



#################################################################################################################
## sourcing required packages 
#################################################################################################################
# "geosphere",
packages_required <- c("terra", "sf", "rgl", "sp", "geodata", "tidyverse",  "countrycode", "lubridate", "plyr", 
                       "parallel", "foreach", "ggplot2", "tidyr")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))


#################################################################################################################
## get soil INS: Running reverse QUEFTS
#################################################################################################################

source("~/shared-data/Scripts/generic/nutrientResponse/QUEFTS_functions.R")
ModlingData <- readRDS("~/shared-data/Data/Wheat/fieldData/wheat_modelReady.rds")
head(ModlingData)
dim(ModlingData)
length(unique(ModlingData$trial_id))## 507 trials


ModlingData <- ModlingData %>% 
  dplyr::rename(N = n_rate2, P = p_rate2, K = k_rate2, refTreat = ref_trt , TLID = trial_id, treat =treatment_id ) %>% 
  dplyr::select(-c(grain_yield_kgpha )) %>% 
  unique()
length(unique(ModlingData$TLID))


## get soil INS
Crop <- "Wheat"
wheatSupplyWLY <- NULL
for(i in 1:length(unique(ModlingData$TLID))){
  tid <- unique(ModlingData$TLID)[i]
  print(i)
  dsi <- ModlingData[ModlingData$TLID == tid, ]
  names(dsi)[names(dsi) == "blup"] <- "Y" 
  dsi <- dsi %>% unique()
  
  #with using the WLY, the attainable yield is set to 70% of the WLY (neutral year), otherwise 30% more of the max yield obtained within the trial
  yy <- dsi[dsi$Y == max(dsi$Y), ]
  Yai <- mean(yy$Y) * 1.3 ### attainable yield is set at 30% more than the max yield
  
  Yai <- unique(dsi$WLY_neutral)*700 ## converting it to kg/ha and taking 70%
  
  #at least 2 rows of data are needed + attainable yield:
  if(length(unique(dsi$treat)) > 1 & !is.na(Yai)){
    
    ## if possible keep control treatment for validation and indicate the trial accordingly
    conData <- NULL
    if(nrow(dsi[dsi$treat == "0_0_0",]) >= 1 & length(unique(dsi$treat)) > 3){
      conData <- dsi[dsi$treat == "0_0_0",]
      dsi <- dsi[!dsi$treat == "0_0_0",]
    }
    si <- revQUEFTS(ds = dsi,
                    Ya = Yai,
                    crop = Crop)
    
    ss <- data.frame(TLID = tid, Ya = Yai,
                     N_base_supply = si[1],
                     P_base_supply = si[2],
                     K_base_supply = si[3])
    
    conY <- runQUEFTS(nut_rates = data.frame(N=0, P=0, K=0),
                      supply = si,
                      crop = Crop,
                      Ya = Yai,
                      SeasonLength = 150)
    
    if(!is.null(conData)){
      ss$controlObs <- conData$Y
      ss$yieldQUEFTS <- conY
      ss$conVal <- TRUE
    }else{
      ss$controlObs <- "NA"
      ss$yieldQUEFTS <- conY
      ss$conVal <- FALSE
    }
    wheatSupplyWLY <-  rbind(wheatSupplyWLY, ss)
  }
}



saveRDS(wheatSupply507, "~/shared-data/Data/Wheat/Intermediate/wheatSupply507.RDS") ## with at least 2 treatments/trial and max yield+30%
saveRDS(wheatSupplyWLY, "~/shared-data/Data/Wheat/Intermediate/wheatSupplyWLY.RDS")## with WLY

wheatSupply <- readRDS("~/shared-data/Data/Wheat/Intermediate/wheatSupply507.RDS")
wheatSupplyWLYNeutral <- readRDS("~/shared-data/Data/Wheat/Intermediate/wheatSupplyWLY.RDS")

wheatSupply <- wheatSupply %>%
  dplyr::mutate_if(is.character, as.numeric) %>%
  dplyr::mutate(controlObs = round(controlObs, digits = 0),
                yieldQUEFTS = round(yieldQUEFTS, digits = 0))
head(wheatSupply)


### from 507 trial IDs, only 384 have > 2 treatments 

ggQV <- wheatSupply %>% dplyr::filter(conVal = TRUE) %>% 
  ggplot(aes(controlObs, yieldQUEFTS)) +
  geom_point()+
  stat_smooth(method = "lm") +
  geom_abline(slope=1, intercept = 0)+
  xlab("Observed control yield [kg/ha]")+
  ylab("Predicited control yield [kg/ha]")+
  xlim(0,5000) + ylim(0,5000)+
  ggtitle("QUEFTS INS validation")+
  theme_bw()+
  theme(axis.title = element_text(size=12), plot.title = element_text(size=12, hjust = 0.5, face = "bold"))

ggQV

ggsave("~/shared-data/Data/Wheat/Intermediate/QUEFTS_INS_Val.pdf", ggQV, width = 6, height = 6)


ModlingData <- readRDS("~/shared-data/Data/Wheat/fieldData/yield_diff_blup.rds")
ModlingData2 <- ModlingData %>% 
  dplyr::select(c("trial_id","source", "year", "NAME_1","NAME_2","NAME_3", "long2", "lat2")) %>% 
  dplyr::rename(TLID = trial_id, long = long2, lat = lat2) %>% 
  unique()

wheatSupply <- wheatSupply %>% inner_join(ModlingData2)

wheatSupply %>% filter(yieldQUEFTS > 2500 & controlObs < 2000)

ModlingData %>% filter(trial_id %in% c("39.2_7.59_2002_P_calibration NSRC"))

#### Aggregate WLY and IINS by SLU


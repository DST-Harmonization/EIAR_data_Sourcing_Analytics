#################################################################################################################
## sourcing required packages 
#################################################################################################################
packages_required <- c("terra", "sf", "rgl", "sp", "geodata", "tidyverse","lubridate", "plyr", "geosphere")
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){install.packages(packages_required[!installed_packages])}
invisible(lapply(packages_required, library, character.only = TRUE))

#####################################################################################################################################

## for every CLIMATEZONE , create climate scenarios and aggregate WLY across these scenarios
#' @param df is the excel file downloaded from Gyga for a crop and country
#' @return a data frame with c(COUNTRY, CLIMATEZONE, WLY_dry, WLY_neutral,  WLY_wet)
#' @example Scenario_agg(df=read.csv("~/shared-data/Data/General/Gyga/GygaEthiopiaRainfedMaize.csv"))
#' @author MeklitC
Scenario_agg <- function(df){
  df <- df %>% dplyr::filter(!is.na(YW)) %>% dplyr::select(COUNTRY,CLIMATEZONE,HARVESTYEAR,YW) %>% unique()
  yyVar <- ddply(df, .(CLIMATEZONE), summarize, Q1=quantile(YW, probs=0.25), Q3=quantile(YW, probs=0.75))
  
  ClimateScenario <- NULL
  for(i in unique(df$CLIMATEZONE)){
    czData <-  df %>% dplyr::filter(CLIMATEZONE ==i)
    czyyVar <- yyVar %>% dplyr::filter(CLIMATEZONE ==i)
    czData <- czData %>% inner_join(czyyVar) %>% 
      dplyr::mutate(Climate_scenario = ifelse(YW <= Q1, "WLY_dry", ifelse(YW>Q1 & YW<=Q3, "WLY_neutral", "WLY_wet"))) %>% 
      dplyr::group_by(CLIMATEZONE, Climate_scenario) %>% 
      dplyr::mutate(WLY = median(YW)) %>% 
      dplyr::select(c(COUNTRY, CLIMATEZONE, WLY)) %>% unique()
    
    ClimateScenario <- rbind(ClimateScenario, czData)
  }
  
  ClimateScenario <- ClimateScenario[order(ClimateScenario$CLIMATEZONE, ClimateScenario$Climate_scenario),]
  ClimateScenario <- ClimateScenario[, c("COUNTRY", "CLIMATEZONE", "Climate_scenario",  "WLY")]
  ClimateScenario <- tidyr::spread(ClimateScenario, Climate_scenario, WLY)
  
  return(as.data.frame(ClimateScenario))
}

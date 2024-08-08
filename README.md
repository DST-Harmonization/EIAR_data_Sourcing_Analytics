The implementation of modeling for every is done using script under respective folders in the git link.
An example using maize crop …
1. conduct Exploratory data analysis using the RMD file in 
  DST-Harmonization/EIAR_data_Sourcing/maize/analytics/EDA/
  
Once you have model ready data using the scripts with in EIAR_data_Sourcing/maize/analytics/harmonized 
1. Train the ML model using maize_ML.R
2. Use trained model at scale using maize_predict.R
3. Calculate fertilizer rates using maize_calculate_Fertilizer.R
4. Extract rates for validation using maize_extract_Fertilizer.R

All data for this analysis are currently in the EIAR Lab server. To test the scripts,one need to have permission to access from EIAR.  


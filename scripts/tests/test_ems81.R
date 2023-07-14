#### TEST EMS81 ###################################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")


### Fetch raw data ####

sf.df <- fetchEMS81(folderIn = "C:/Users/psanche2/Desktop/EMSData/",
                          fileOut = "data_processed/sf_05_2023.csv")
tail(sf.df)


plotTimeSeries(data = sf.df,
                    xVar = timestamp,
                    yVar = sap_flux_kg_h,
                    xLab = "time", 
                    yLab = "sap flow (kg/h)", 
                    lineOrPoint = "line", 
                    colorVar = ID_species)



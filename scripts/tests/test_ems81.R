#### TEST EMS81 ###################################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")
source("scripts/functions/functions.R")


### Fetch raw data ####

sf.df <- fetchEMS81(folderIn = "input_data_example/caxuana_sapflow/",
                          fileOut = "data_raw/sap_flow/raw_sap_flow_2023_07_13.csv")
tail(sf.df)


plotTimeSeries(data = sf.df,
                    xVar = timestamp,
                    yVar = sap_flux_kg_h,
                    xLab = "time", 
                    yLab = "sap flow (kg/h)", 
                    lineOrPoint = "line", 
                    colorVar = ID_species)



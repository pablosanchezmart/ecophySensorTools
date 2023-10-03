#### FUNCTIONS SENSOR DATA PROCESSING #####################################

## Pablo Sanchez Martinez
## 05/2023

source("initialization.R")


### FLORAPULSE ----------------------------------------------------------------- ####

### Correct wrong times using the number record and previous data

correctWrongTime <- function(data){
  
  # detect rows with problems in the time column
  rowsWithProblems <- 0
  rowsWithProblems <- which(data$timestamp > "2024-01-01")
  
  if(length(rowsWithProblems) > 0){
    
    warning("correcting wrong time using record number")
    
    # Identify time using the record column
    
    for(row in rowsWithProblems){
      data[row, 1] <- data[row-1, 1] + 15*60
    }
  }
  return(data)
}

### Read florapulse water potential raw data ####

fetchFlorapulse <- function(folderIn = NULL,
                            fileOut = NULL){
  require(readr)
  require(stringr)
  require(dplyr)
  
  if(is.null(folderIn)){
    stop("specify folderIn path where the input data is located")
  }
  
  # function to read individual files and change from wide to long format
  readFpCsv <- function(file){
    wide.df <- as.data.frame(
      read_csv(file, 
                skip = 1)
      )[-c(1, 2), ]
    
    wp_cols <- names(wide.df)[-c(1,2)]
    
    long.df <- data.frame("timestamp" = character(), 
                        "record" = numeric(), 
                        "value" = numeric(), 
                        "label" = character())
    
    for(wp_col in wp_cols){
      
      ind_long.df <- data.frame(
        "timestamp" = lubridate::ymd_hms(wide.df[, "TIMESTAMP"]),
        "record" = wide.df[, "RECORD"],
        "value" = wide.df[, wp_col],
        "label" = wp_col
      )
      
      long.df <- rbind(long.df,
                       ind_long.df)
      
    }
    return(long.df)
  }
  
  # Read all files into one
  
  file.list <- list.files(folderIn, 
                          pattern = ".dat", 
                          full.names = T)
  
  raw_data.list <- lapply(file.list, 
                          readFpCsv)
  
  raw_data.df <- do.call(rbind, raw_data.list)
  
  raw_data.df$value[raw_data.df$value == -7999] <- NA
  
  # correct time if needed
  raw_data.df <- correctWrongTime(data = raw_data.df)
  
  # save
  if(!is.null(fileOut)){
    write_csv(raw_data.df, fileOut)
    print(paste0("saving raw data in ", fileOut))
  }
  
  return(raw_data.df)
}

## Process stem water potential data

processFlorapulse <- function(rawDataFile = NULL,
                              rawData = NULL,
                              offsetMultiplierFile = NULL,
                              offsetMultiplier = NULL,
                              fileOut = NULL){
  
  require(readr)
  require(stringr)
  
  # Read raw data
  
  if(is.null(rawData) && is.null(rawDataFile)){
    stop("Specify rawData object or rawDataFile path")
  } 
  
  if(is.null(rawData) && !is.null(rawDataFile)){
    rawData <- read_csv(rawDataFile) 
    print(paste0("reading raw data from ", rawDataFile))
  }
  
  # Read multiplier and offset info
  
  if(is.null(offsetMultiplier) && is.null(offsetMultiplierFile)){
    stop("Specify offsetMultiplier object or offsetMultiplierFile path")
  } 
  if(is.null(offsetMultiplier) && !is.null(offsetMultiplierFile)){
    offsetMultiplier <- read_csv(offsetMultiplierFile) 
    print(paste0("reading raw data from ", offsetMultiplierFile))
  }
  
  # sensor identificator
  
  processed.df <- rawData %>%
    mutate("flora_pulse_sensor" = str_remove(label, "_AVG"))

  # Check whether all sensor names in the field have a offset and multiplier
  
  sensorsWithoutOM <- processed.df %>%
    filter(!flora_pulse_sensor %in% offsetMultiplier$flora_pulse_sensor) %>%
    pull(flora_pulse_sensor) %>%
    unique()
  
  if(length(sensorsWithoutOM > 0)){
    warning(paste0("Sensors without Offset multiplier: ", paste0(sensorsWithoutOM, collapse = ", ")))
  }
  
  # process data
  
  processed_om.df <- merge(processed.df, offsetMultiplier, 
                           by = "flora_pulse_sensor", 
                           all.x = T) %>%
    filter(!is.na(flora_pulse_sensor)) %>%
    mutate(wp_bar = (flora_pulse_offset + (flora_pulse_multiplier * value)))

  
  # Select and store results

  processed.df <- processed_om.df %>%
    select(timestamp, ID, plot, species, size_class, flora_pulse_sensor, wp_bar)
  
  rslts <- list("processing_table" = processed_om.df,
                "processed_data" = processed.df)
  
  if(!is.null(fileOut)){
    write_csv(processed.df, fileOut)
    print(paste0("saving processed data in ", fileOut))
  }

  return(rslts)
}


### TEROS 12 ------------------------------------------------------------------- ####

### Read Teros 12 water cotent data ####

fetchTeros12 <- function(folderIn = NULL,
                         fileOut = NULL){
  
  require(readr)
  require(stringr)
  require(dplyr)
  require(readxl)
  
  if(is.null(folderIn)){
    stop("specify folderIn path where the input data is located")
  }
  
  # function to read individual files and change from wide to long format
  readWcXlsx <- function(file){
    
    # extract logger metadata and name (serial number) from file
    
    metadataLogger <- as.data.frame(t(read_excel(file, sheet = "Metadata", col_names = F)))
    colnames(metadataLogger) <- metadataLogger[2, ]
    metadataLogger <- metadataLogger[3, ]
    loggerName <- metadataLogger[, "Serial Number"]
    
    # identify sheets with data
    processed_data_sheets <- which(str_detect(excel_sheets(file), "Processed"))
    
    # retrieve data for each of the sheets with data
    
    all_sheets_long.df <- data.frame("timestamp" = character(), 
                                     "label" = character(),
                                     "water_content_m3.m3" = numeric(),
                                     "soil_temperature_C" = numeric(),
                                     "bulk_EC_mS.cm" = numeric())
    
    for(dataSheet in processed_data_sheets){
      
      # extract column names from file
      Names <- as.data.frame(read_excel(file, sheet = dataSheet, col_names = F))[1:3, -1]
      
      colNames <- c("timestamp")
      for(i in 1:length(Names)){
        colName <- paste0(Names[1, i], ".", Names[3, i])
        
        colNames <- c(colNames, colName)
      } 
      
      colNames <- str_replace_all(colNames, " ", "_")
      colNames <- str_remove(colNames, "°")
      colNames <- str_replace_all(colNames, "³", "3")
      
      # Wide data
      
      wide.df <- as.data.frame(
        read_excel(file, 
                   skip = 3 , col_names = colNames, sheet = dataSheet)
      ) %>%
        select(timestamp, contains(paste0("Port_", 1:6)))
      
      long.df <- data.frame("timestamp" = character(), 
                            "label" = character(),
                            "water_content_m3.m3" = numeric(),
                            "soil_temperature_C" = numeric(),
                            "bulk_EC_mS.cm" = numeric())
      
      # Identify ports with WC data
      
      ports <- unlist(Names[1, ])
      ports <- unique(str_replace_all(ports[which(ports %in% paste0("Port ", 1:6))], " ", "_"))
      
      # Long data
      
      for(port in ports){
        
        if(paste0(port, ".m3/m3_Water_Content") %in% names(wide.df)){  # to make sure sensor has been colecting data
          ind_long.df <- data.frame(
            "timestamp" = wide.df[, "timestamp"],
            "label" = paste0(loggerName, "_", port),
            "water_content_m3.m3" = wide.df[, paste0(port, ".m3/m3_Water_Content")],
            "soil_temperature_C" = wide.df[, paste0(port, ".C_Soil_Temperature")],
            "bulk_EC_mS.cm" = wide.df[, paste0(port, ".mS/cm_Bulk_EC")]
          )
          
          long.df <- rbind(long.df,
                           ind_long.df)
        } else{
          warning(paste0("no data for sensor ", paste0(loggerName, "_", port)))
        }
        

        
      }
      
      all_sheets_long.df <- rbind(all_sheets_long.df, long.df)
      
    }
    return(all_sheets_long.df)
  }
  
  file.list <- list.files(folderIn, 
                          pattern = ".xlsx", 
                          full.names = T)
  
  raw_data.list <- lapply(file.list, 
                          readWcXlsx)
  
  raw_data.df <- do.call(rbind, raw_data.list)
  
  if(!is.null(fileOut)){
    write_csv(raw_data.df, fileOut)
    print(paste0("saving raw data in ", fileOut))
  }
  
  return(raw_data.df)
}


processTeros12 <- function(rawDataFile = NULL,
                              rawData = NULL,
                              labelToIDFile = NULL,
                              labelToID = labelToID.data,
                              fileOut = NULL,
                              offset = -0.08189,
                              multiplier = 1.83658){
  require(readr)
  require(stringr)
  require(lubridate)
  
  # Read raw data

  if(is.null(rawData) && is.null(rawDataFile)){
    stop("Specify rawData object or rawDataFile path")
  }  
    
  if(is.null(rawData) && !is.null(rawDataFile)){
    rawData <- read_csv(rawDataFile) 
    print(paste0("reading raw data from ", rawDataFile))
  }
  
  # process data
  
  processedData <- merge(rawData, labelToID, 
                           by = "label", 
                           all.x = T) %>%
    mutate(date = as_date(as_datetime(timestamp)),
           calibrated_water_content_m3.m3 = offset + (water_content_m3.m3 * multiplier)) %>%
    select(timestamp, date, ID, plot, species, size_class, teros12_sensor = label, everything())
  
  rslts <- list("processing_table" = labelToID,
                "processed_data" = processedData)
  
  if(!is.null(fileOut)){
    write_csv(processedData, fileOut)
    print(paste0("saving processed data in ", fileOut))
  }
  
  return(rslts)
}

### EMS------------------------------------------------------------------------- ####
### Read EMS sap flow raw data ####

fetchEMS81 <- function(folderIn = NULL,
                       fileOut = NULL){
  
  require(readr)
  require(stringr)
  require(dplyr)
  require(readxl)
  require(quantreg)
  
  # function to read individual files and change from wide to long format
  readEmsXlsx <- function(file){
    ems.df <- as.data.frame(read_excel(file))
    
    print(file)
    
    id_species <- str_replace(names(ems.df)[2], 
                              "^\\S* ", "")
    
    id <- unlist(str_split(id_species, "_"))
    
    numCol <- length(names(ems.df))
    
    timestamp <- ems.df[, 1]
    species <- paste0(id[3], " ", id[4])
    plot <- ifelse(id[1] == "TFE", "TFE", "Control")
    id <- paste0(plot, "_", id[2])
    
    sapflow <- rep(NA, length(timestamp))
    increment <- rep(NA, length(timestamp))
    resistance <- rep(NA, length(timestamp))
    left_electrode <- rep(NA, length(timestamp))
    central_electrode <- rep(NA, length(timestamp))
    right_electrode <- rep(NA, length(timestamp))
    status <- rep(NA, length(timestamp))
    voltage <- rep(NA, length(timestamp))
    
    if(numCol == 10){
      sapflow <- ems.df[, 2]
      increment <- ems.df[, 3]
      resistance <- ems.df[, 4]
      left_electrode <- ems.df[, 5]
      central_electrode <- ems.df[, 6]
      right_electrode <- ems.df[, 7]
      status <- ems.df[, 8]
      voltage <- ems.df[, 9]
    } else{
      if(numCol == 4){
        sapflow <- ems.df[, 2]
        increment <- ems.df[, 3]
        resistance <- ems.df[, 4]
      } else{
        stop("Not recognized number of columns")
      }
    }
    
    clean_ems.df <- data.frame("timestamp" = timestamp, 
                               "ID" = id,
                               "species" = species,
                               "plot" = plot,
                          "sap_flux_kg_h" = sapflow, 
                          "increment_mm" = increment, 
                          "resistance_KOhm" = resistance,
                          "dT_left_electrode_K" = left_electrode,
                          "dT_central_electrode_K" = central_electrode,
                          "dT_right_electrode_K" = right_electrode,
                          "operating_status" = status,
                          "supply_voltage_V" = voltage)

    return(clean_ems.df)
  }
  
  # Read all files into one
  
  file.list <- list.files(folderIn, 
                          pattern = ".xlsx", 
                          full.names = T)
  
  raw_data.list <- lapply(file.list, 
                          readEmsXlsx)
  
  raw_data.df <- do.call(rbind, raw_data.list)
  
  
  ### Baseline (Lion) ####
  
  # using a 0.1 quantile regression
  
  unique_id <- unique(raw_data.df$ID)
  raw_data.df$bl_sap_flux_Kg_h <- NA
  
  # Loop through each unique id.
  for (id in unique_id) {
    # Step 2a: Subset the dataset for the specific id.
    id_subset <- raw_data.df[raw_data.df$ID == id, ]
    if(all(is.na(id_subset$sap_flux_kg_h))){
      next()
    }
    
    # Step 2b: Perform quantile regression for the specific id.
    quant_reg <- rq(sap_flux_kg_h ~ timestamp, data = id_subset, tau = 0.1)
    
    # Step 2c: Predict baseline using the quantile regression.
    baseline <- predict(quant_reg, newdata = id_subset)
    
    # Step 2d: Apply the shift to the sapflux values for the specific id.
    raw_data.df$bl_sap_flux_Kg_h[raw_data.df$ID == id] <- id_subset$sap_flux_kg_h - baseline
  }
  
  
  if(!is.null(fileOut)){
    write_csv(raw_data.df, fileOut)
  }
  
  return(raw_data.df)
  
}


### METEOROLOGY ---------------------------------------------------------------- ####
### Determine which plot is the data coming from and which dates is data covering, save in folder ####

dataIdentificator <- function(folderIn = NULL,
                     folderOutA = NULL,
                     folderOutB = NULL){
  require(readr)
  require(stringr)
  require(dplyr)
  require(lubridate)
  
  # to save the names of the files that cannot be automatically identified
  
  filesNotIdentified <- c()
  
  if(is.null(folderIn)){
    stop("specify folderIn path where the input data is located")
  }
  
  if(is.null(plot)){
    stop("specify plot")
  }
  
  # List of files to classify into control or TFE
  
  file.list <- list.files(folderIn, 
                          pattern = ".dat", 
                          full.names = T)
  
  for(file in file.list){
    
    logger.data <- as.data.frame(
      read_csv(file, 
               # skip = 1,
               na = "7999", col_names = F)
    )[1, ]
    
    dates <- as.data.frame(
      read_csv(file, 
               skip = 1,
               na = "7999")
    )[-c(1, 2), ]
    
    if(isFALSE("TIMESTAMP" %in% names(dates))){
      next(paste0(file, "does not contain data."))
    }
    
    dates <- dates[c(1, length(dates[, 1])), ] %>%
      mutate(date = as_date(TIMESTAMP)) %>%
      pull(date)
    
    # raw data new name
    filename <- paste0(paste0(dates, collapse = "_"), "_", str_replace(logger.data[, 2], " ", "_"), ".dat")
    
    # save raw data in plot folder
    
    if(str_detect(logger.data[, 2], "A") | str_detect(file, "PA")){
      
      # Create directory if it does't exist
      dir.create(folderOutA, showWarnings = F)
      # Copy file into directory to separate plot A and B data
      file.copy(file, paste0(folderOutA, filename))
      print(paste0(filename, " saved"))
    } else{
      if(str_detect(logger.data[, 2], "PB") | str_detect(file, "PB") | str_detect(file, "TORRE B")){
        # Create directory if it does't exist
        dir.create(folderOutB, showWarnings = F)
        # Copy file into directory to separate plot A and B data
        
        file.copy(file, paste0(folderOutB, filename))
        print(paste0(filename, " saved"))
      } else{
        
        warning(paste0(file, "plot identity info not red. Try finding it manually"))
        filesNotIdentified <- c(filesNotIdentified, file)
      } 
    }
  }
  return(filesNotIdentified)
}

### For soil data. Determine which plot is the data coming, whether it is water content or potential and from and which dates is data covering, save in folder ####

soilDataIdentificator <- function(folderIn = NULL,
                              folderOutA = NULL,
                              folderOutB = NULL){
  require(readr)
  require(stringr)
  require(dplyr)
  require(lubridate)
  
  # to save the names of the files that cannot be automatically identified
  
  filesNotIdentified <- c()
  
  if(is.null(folderIn)){
    stop("specify folderIn path where the input data is located")
  }
  
  if(is.null(plot)){
    stop("specify plot")
  }
  
  # List of files to classify into control or TFE
  
  file.list <- list.files(folderIn, 
                          pattern = ".dat", 
                          full.names = T)
  
  for(file in file.list){
    
    logger.data <- as.data.frame(
      read_csv(file, 
               # skip = 1,
               na = "7999", col_names = F)
    )[1, ]
    
    dates <- as.data.frame(
      read_csv(file, 
               skip = 1,
               na = "7999")
    )[-c(1, 2), ]
    
    if(isFALSE("TIMESTAMP" %in% names(dates))){
      next(paste0(file, "does not contain data."))
    }
    
    if(any(str_detect(names(dates), "VW"))){
      label <- "vw"
    } else{
      if(any(str_detect(names(dates), "SWP"))){
        label <- "swp"
      } else{
        warning("Not vw or swp info")
        next()
      }
    }
    dates <- dates[c(1, length(dates[, 1])), ] %>%
      mutate(date = as_date(TIMESTAMP)) %>%
      pull(date)
    
    # raw data new name
    filename <- paste0(label, "_", paste0(dates, collapse = "_"), "_", str_replace(logger.data[, 2], " ", "_"), ".dat")
    
    # save raw data in plot folder
    
    if(str_detect(logger.data[, 2], "A") | str_detect(file, "PA")){
      
      # Create directory if it does't exist
      dir.create(folderOutA, showWarnings = F)
      # Copy file into directory to separate plot A and B data
      file.copy(file, paste0(folderOutA, filename))
      print(paste0(filename, " saved"))
    } else{
      if(str_detect(logger.data[, 2], "PB") | str_detect(file, "PB") | str_detect(file, "TORRE B")){
        # Create directory if it does't exist
        dir.create(folderOutB, showWarnings = F)
        # Copy file into directory to separate plot A and B data
        
        file.copy(file, paste0(folderOutB, filename))
        print(paste0(filename, " saved"))
      } else{
        
        warning(paste0(file, "plot identity info not red. Try finding it manually"))
        filesNotIdentified <- c(filesNotIdentified, file)
      } 
    }
  }
  return(filesNotIdentified)
}


### Read meteorological raw data ####

fetchMet <- function(file = NULL,
                     fileOut = NULL,
                     plot = NULL){
  require(readr)
  require(stringr)
  require(dplyr)
  require(lubridate)
  
  if(is.null(file)){
    stop("specify file path where the input data is located")
  }
  
  if(is.null(plot)){
    stop("specify plot")
  }
  
  # function to read individual files and change from wide to long format
  readMetCsv <- function(file){
    df <- as.data.frame(
      read_csv(file, 
               skip = 1,
               na = "7999")
    )[-c(1, 2), ]
    
    for(i in 2:length(df)){
      df[, i] <- as.numeric(df[, i])
    }

    names(df) <- tolower(names(df))
    
    return(df)
  }
  
  # Read all files into one
  # 
  # file.list <- list.files(folderIn, 
  #                         pattern = ".dat", 
  #                         full.names = T)
  
  # raw_data.list <- lapply(file.list, 
  #                         readMetCsv)
  
  # raw_data.df <- do.call(bind_rows, raw_data.list)
  
  raw_data.df <- readMetCsv(file)
  
  raw_data.df <- raw_data.df %>%
    mutate(timestamp = as_datetime(timestamp)) %>%
    arrange(timestamp)

  
  # save
  if(!is.null(fileOut)){
    write_csv(raw_data.df, fileOut)
    print(paste0("saving raw data in ", fileOut))
  }
  
  return(raw_data.df)
}


#### ADDITIONAL FUNCTIONS ------------------------------------------------------ #####
### Mean, mode or date ####

### Mean or mode ####

meanOrMode <- function (x){
  require(lubridate)
  
  # all NAs?
  if(all(is.na(x))){
    return(NA)
  } else{
    
    # Mean for numbers
    
    if (is.numeric(x) | is.double(x) | is.integer(x)) {
      meanX <- mean(x, na.rm = T)
      return(meanX)
    } else{
      
      # mode for characters
      if (is.character(x) | is.factor(x)| lubridate::is.Date(x)) {
        tble <- as.data.frame(table(x))
        modeX <- tble %>% dplyr::filter(.data$Freq == max(.data$Freq)) %>% 
          dplyr::pull(.data$x) %>%
          as.character()
        return(modeX)
      }
    }
  }
}

###  Combine datasets ####

combineData <- function(data, variablesToCombine){
  
  for(var in variablesToCombine){
    var.x <- paste(var, ".x", sep = "")
    var.y <- paste(var, ".y", sep = "")
    
    var.class <- class(data[, var.x])
    
    # choose default value from 'x' in the case that both '.x' and '.y' have values
    data[!is.na(data[, var.x]) & !is.na(data[, var.y]), var] <- data[!is.na(data[, var.x]) & !is.na(data[, var.y]), var.x]
    
    # replace NA values in 'x' with values from 'y' and vice-versa
    data[is.na(data[, var.x]), var] <- data[is.na(data[, var.x]), var.y]
    data[is.na(data[, var.y]), var] <- data[is.na(data[, var.y]), var.x]
    
    # Delete ".x" and ".y" columns
    data[, var.x] <- NULL
    data[, var.y] <- NULL
    
    if(var.class %in% c("numeric", "integer", "double")){
      data[, var] <- as.numeric(data[, var])
    } else{
      data[, var] <- as.character(data[, var])
    }
  }
  return(data)
}

### Calculate quantile for a given variable ####

getQuantile <- function(x, prob = 0.05){
  res <- as.numeric(quantile(x, probs = prob, na.rm = T))
  return(res)
}


### Plot time series (points or lines) ####

plotTimeSeries <- function(data, xVar, yVar , colorVar, xLab = "x", yLab = "y", lineOrPoint = "line"){
  
  arg <- match.call()
  
  plot <- ggplot(data, aes(x = eval(arg$xVar), y = eval(arg$yVar), color = eval(arg$colorVar)))
  
  if(lineOrPoint == "line"){
    plot <- plot + 
      geom_line() + 
      theme_minimal() +
      theme(legend.title = element_blank(), legend.position = "bottom") +
      xlab(xLab) + ylab(yLab)
  }
  
  if(lineOrPoint == "point"){
    plot <- plot +
      geom_point(alpha = 0.1) + 
      geom_smooth(method = "gam") + 
      theme_minimal() +
      theme(legend.title = element_blank(), legend.position = "bottom") +
      xlab(xLab) + ylab(yLab)
  }
  
  return(plot)
}


### Retrieve linear models and generalized linear models results ####

linearModeler <- function(Formula = "growth.cm.day.gf ~ control_HSMs_50cm_MPa", Data =  species.data, Family = "gaussian"){
  
  results <- list()
  
  if(Family == "gaussian"){
    results$linearModel <- lm(as.formula(Formula), data = Data)
    results$Summary <- summary(results$linearModel)
  } else{
    results$linearModel <- glm(as.formula(Formula), data = Data, family = Family)
    results$Summary <- summary(results$linearModel)
    results$Summary$r.squared <- NA
    results$Summary$adj.r.squared <- NA
  }
  
  results$plot <- ggplot(data = species.data, aes(x = control_HSMs_50cm_MPa, y = growth.cm.day.gf)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = F) +
    theme_minimal()
  
  return(results)
}


### Estimate and extract linear model or generalized linear model coefficients and R2 for a list of responses and predictors ####


linearModelExtractor <- function(responses = "growth.cm.day.gf", predictors = "control_HSMs_50cm_MPa", data = species.data, modelType = "gaussian"){
  
  results <- list()
  
  coefficients.df <- data.frame("Response" =  character(), 
                                "Predictor" = character(), 
                                "Coefficient" = numeric(), 
                                "Significance" = numeric(), 
                                "R2" = numeric(),
                                "Model type" = character())
  
  lm.list <- list()
  
  for(resp in responses){
    
    for(pred in predictors){
      
      frmla <- paste0(resp,  " ~ ", pred)
      
      print(frmla)
      
      lm <- linearModeler(Formula = frmla, Data = data, Family = modelType)
      lm$Summary
      coefficient.df <- data.frame("Response" =  resp,
                                   "Predictor" = pred, 
                                   "Coefficient" = round(lm$Summary$coefficients[pred, 1], 3), 
                                   "Significance" = round(lm$Summary$coefficients[pred, 4], 3), 
                                   "R2" = round(lm$Summary$r.squared, 3),
                                   "Adjusted R2" = round(lm$Summary$adj.r.squared, 3),
                                   "Model type" = modelType)
      
      coefficients.df <- rbind(coefficients.df, coefficient.df)
      lm.list[[frmla]] <- lm
    }
  }
  
  results$coefficients <- coefficients.df
  results$models <- lm.list
  
  return(results)
}

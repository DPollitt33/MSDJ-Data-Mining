# TEAM MSDJs
# Data mining
# Final Project

setwd("C:/Users/John/Documents/DataMining/FinalProject/MSDJ-Data-Mining")
getwd()

rm(list = ls(all = TRUE))

# Load data
data <- read.csv('DfTRoadSafety_Accidents_2012.csv', header=TRUE)

# DATA PREPROCESSING

  # Rename verbose fields
  names(data)[names(data) == 'Latitude'] <- "Lat"
  names(data)[names(data) == 'Longitude'] <- "Long"
  names(data)[names(data) == 'Accident_Severity'] <- "Severity"
  names(data)[names(data) == 'X1st_Road_Class'] <- "First_road_class"
  names(data)[names(data) == 'X1st_Road_Number'] <- "First_road_number"
  names(data)[names(data) == 'X2nd_Road_Class'] <- "Second_road_class"
  names(data)[names(data) == 'X2nd_Road_Number'] <- "Second_road_number"
  names(data)[names(data) == 'Pedestrian_Crossing.Human_Control'] <- "Ped_xing_human"
  names(data)[names(data) == 'Pedestrian_Crossing.Physical_Facilities'] <- "Ped_xing_physical"
  names(data)[names(data) == 'Light_Conditions'] <- "Light"
  names(data)[names(data) == 'Weather_Conditions'] <- "Weather"
  names(data)[names(data) == 'Road_Surface_Conditions'] <- "Surface"
  names(data)[names(data) == 'Special_Conditions_at_Site'] <- "Special"
  names(data)[names(data) == 'Did_Police_Officer_Attend_Scene_of_Accident'] <- "Police_attendance"
  names(data)[names(data) == 'Accident_Index'] <- "Index"
  names(data)[names(data) == 'Location_Easting_OSGR'] <- "E_OSGR"
  names(data)[names(data) == 'Location_Northing_OSGR'] <- "N_OSGR"
  names(data)[names(data) == 'Local_Authority_.District.'] <- "LA_District"
  names(data)[names(data) == 'Local_Authority_.Highway.'] <- "LA_Highway"
  names(data)[names(data) == 'Day_of_Week'] <- "Day"
  names(data)[names(data) == 'LSOA_of_Accident_Location'] <- "LSOA"
  

  # Factorize fields
  data$Police_Force <- factor(data$Police_Force)
  data$Severity <- factor(data$Severity)
  data$Day <- factor(data$Day)
  data$LA_District <- factor(data$LA_District)
  data$First_road_class <- factor(data$First_road_class)
  data$Road_Type <- factor(data$Road_Type)
  data$Junction_Detail <- factor(data$Junction_Detail)
  data$Junction_Control <- factor(data$Junction_Control)
  data$Second_road_class <- factor(data$Second_road_class)
  data$Ped_xing_human <- factor(data$Ped_xing_human)
  data$Ped_xing_human <- factor(data$Ped_xing_human)
  data$Light <- factor(data$Light)
  data$Weather <- factor(data$Weather)
  data$Surface <- factor(data$Surface)
  data$Special <- factor(data$Special)
  data$Carriageway_Hazards <- factor(data$Carriageway_Hazards)
  data$Urban_or_Rural_Area <- factor(data$Urban_or_Rural_Area)
  data$Police_attendance <- factor(data$Police_attendance)
  
  # Clear Missing values
  cleanData <- data[!(is.na(data$LSOA) | data$LSOA==""),]
  
  # Clear data outside range
  cleanData <- subset(cleanData, Junction_Control != -1)
  cleanData <- subset(cleanData, Second_road_class != -1)
  cleanData <- subset(cleanData, Surface != -1)

  # Remove identifier
  cleanData$Index <- NULL


# VARIABLE SELECTION

  selectedData <- cleanData

  # Remove unnecessary variables

  # Check correlation of numeric location data
  numericLocData <- data.frame(EOSGR=selectedData$E_OSGR, NOSGR=selectedData$N_OSGR,
                               Lat=selectedData$Lat, Long=selectedData$Long)
  library(corrplot)
  corrplot(cor(numericLocData), method="number", type="upper")

  # The tested location Data is highly correlated
  selectedData$E_OSGR <- NULL
  selectedData$N_OSGR <- NULL
  selectedData$Lat <- NULL
  selectedData$Long <- NULL
  # Delete other extra location data
  selectedData$LA_District <- NULL
  selectedData$LA_Highway <- NULL
  selectedData$LSOA <- NULL

  # Data is currently imbalanced in regard to severity
  summary(cleanData$Severity)
  # Use Stratified Sampling to get balanced representation
  # Undersample data with 2 and 3 Severity, but use all of 1 Severity
  severity1Stratum <- subset(selectedData, Severity == 1)
  severity2Stratum <- subset(selectedData, Severity == 2)
  severity3Stratum <- subset(selectedData, Severity == 3)

  splSeverity2 <- sample(1:nrow(severity2Stratum), 578)
  splSeverity3 <- sample(1:nrow(severity3Stratum), 578)

  sampleSev1 <- severity1Stratum
  sampleSev2 <- severity2Stratum[splSeverity2, ]
  sampleSev3 <- severity3Stratum[splSeverity3, ]

  # Combine into final sample
  sampleData <- rbind(sampleSev1, sampleSev2, sampleSev3)
  
  # Use Random Forest to observe information gains
  library(randomForest)
  # Some variables have too many categories so they are taken out
  rf.fit <- randomForest(Severity ~ . -Time -Date
                         , data=sampleData,ntree=1000,proximity=TRUE)
  # Check the importance of the variables
  importance(rf.fit)
  varImpPlot(rf.fit)
  

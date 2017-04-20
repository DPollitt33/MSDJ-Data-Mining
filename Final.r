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

  # Default to clean data so we can remove variables without affecting base data
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

  # Not defined in guide
  selectedData$First_road_number <- NULL
  selectedData$Second_road_number <- NULL

# HELPER FUNCTIONS
  
  # FUNCTION: balanceDataBySeverity
  # Function to balance data using ROSE synthetic data library
  # @param inputData - data to balance (train data)
  # @param size - size of each severity stratum after balancing
  # @return balancedTrainData - new balanced data set
  balanceDataBySeverity <- function(inputData, size) {
    # Subset the data by severity
    severity1Stratum <- subset(inputData, Severity == 1)
    severity2Stratum <- subset(inputData, Severity == 2)
    severity3Stratum <- subset(inputData, Severity == 3)
  
    # Use ROSE to balance the input data set
    # ROSE can only act on binary responses so use it on severity 1 & 2
    # and severity 2 & 3 separately
    sev1.2.data <- rbind(severity1Stratum, severity2Stratum)
    balanced.sev1.2.data <- ROSE(Severity~., data=sev1.2.data, N=size*2)$data
  
    sev2.3.data <- rbind(severity2Stratum, severity3Stratum)
    balanced.sev2.3.data <- ROSE(Severity~., data=sev2.3.data, N=size*2)$data
  
    # Combine all 3 data groupings
    balancedTrainData <- rbind(subset(balanced.sev1.2.data, Severity == 1), balanced.sev2.3.data)
  
    return(balancedTrainData)
  }

# USING BOOSTING TO PREDICT SEVERITY
  library(adabag)
  spl <- sample(1:nrow(selectedData), round(0.8*nrow(selectedData)))
  trainData <- selectedData[spl,]
  testData <- selectedData[-spl,]

  balancedTrainData <- balanceDataBySeverity(trainData, 1000)

  # Create ensemble through boosting
  boost.fit <- boosting(Severity ~ Light+Weather+Road_Type+Surface+Number_of_Casualties
                      +Speed_limit+Number_of_Vehicles+Day+Junction_Detail+Junction_Control
                      +Police_attendance+Carriageway_Hazards+Ped_xing_human+Urban_or_Rural_Area
                      +First_road_class+Second_road_class
                      , data=balancedTrainData, mfinal=10)

  # Predict on test data
  predboosting<- predict.boosting(boost.fit, newdata=testData)

  predboosting

  importanceplot(boost.fit)














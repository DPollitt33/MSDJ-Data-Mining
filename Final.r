# TEAM MSDJs
# Data mining
# Final Project

# Select your working directory
setwd("C:/Users/John/Documents/DataMining/FinalProject/MSDJ-Data-Mining")
setwd('C:\\Users\\Dakota\\Documents\\Real Documents\\School\\Data Mining\\Final Project\\Project1')

getwd()

rm(list = ls(all = TRUE))

# DAKOTA POLLITT

# LOAD DATA
data <- read.csv('DfTRoadSafety_Accidents_2012.csv', header=TRUE)

# DATA PREPROCESSING

  # Rename verbose fields
  names(data)[names(data) == 'Accident_Index'] <- 'Index'
  names(data)[names(data) == 'Location_Easting_OSGR'] <- 'E_OSGR'
  names(data)[names(data) == 'Location_Northing_OSGR'] <- 'N_OSGR'
  names(data)[names(data) == 'Longitude'] <- 'Long'
  names(data)[names(data) == 'Latitude'] <- 'Lat'
  names(data)[names(data) == 'Accident_Severity'] <- 'Severity'
  names(data)[names(data) == 'Number_of_Vehicles'] <- 'Vehicles'
  names(data)[names(data) == 'Number_of_Casualties'] <- 'Casualties'
  names(data)[names(data) == 'Day_of_Week'] <- 'Day'
  names(data)[names(data) == 'Local_Authority_.District.'] <- 'LA_district'
  names(data)[names(data) == 'Local_Authority_.Highway.'] <- 'LA_highway'
  names(data)[names(data) == 'X1st_Road_Class'] <- 'First_road_class'
  names(data)[names(data) == 'X1st_Road_Number'] <- 'First_road_number'
  names(data)[names(data) == 'X2nd_Road_Class'] <- 'Second_road_class'
  names(data)[names(data) == 'X2nd_Road_Number'] <- 'Second_road_number'
  names(data)[names(data) == 'Pedestrian_Crossing.Human_Control'] <- 'Ped_xing_human'
  names(data)[names(data) == 'Pedestrian_Crossing.Physical_Facilities'] <- 'Ped_xing_physical'
  names(data)[names(data) == 'Light_Conditions'] <- 'Light'
  names(data)[names(data) == 'Weather_Conditions'] <- 'Weather'
  names(data)[names(data) == 'Road_Surface_Conditions'] <- 'Surface'
  names(data)[names(data) == 'Special_Conditions_at_Site'] <- 'Special'
  names(data)[names(data) == 'Carriageway_Hazards'] <- 'Hazards'
  names(data)[names(data) == 'Urban_or_Rural_Area'] <- 'Urban_or_Rural'
  names(data)[names(data) == 'Did_Police_Officer_Attend_Scene_of_Accident'] <- 'Police_attendance'
  names(data)[names(data) == 'LSOA_of_Accident_Location'] <- 'LSOA'
  
  
  # Factorize fields
  data$Police_Force <- factor(data$Police_Force)
  data$Severity <- factor(data$Severity)
  data$Day <- factor(data$Day)
  data$LA_district <- factor(data$LA_district)
  data$LA_highway <- factor(data$LA_highway)
  data$First_road_class <- factor(data$First_road_class)
  data$First_road_number <- factor(data$First_road_number)
  data$Road_Type <- factor(data$Road_Type)
  data$Speed_limit <- factor(data$Speed_limit)
  data$Junction_Detail <- factor(data$Junction_Detail)
  data$Junction_Control <- factor(data$Junction_Control)
  data$Second_road_class <- factor(data$Second_road_class)
  data$Second_road_number <- factor(data$Second_road_number)
  data$Ped_xing_human <- factor(data$Ped_xing_human)
  data$Ped_xing_physical <- factor(data$Ped_xing_physical)
  data$Light <- factor(data$Light)
  data$Weather <- factor(data$Weather)
  data$Surface <- factor(data$Surface)
  data$Special <- factor(data$Special)
  data$Hazards <- factor(data$Hazards)
  data$Urban_or_Rural <- factor(data$Urban_or_Rural)
  data$Police_attendance <- factor(data$Police_attendance)
  
  
  # Revalue factor levels
  levels(data$Severity) <- list('Fatal'=1,
                                'Serious'=2,
                                'Slight'=3)
  levels(data$Day) <- list('Su'=1,
                           'M'=2,
                           'T'=3,
                           'W'=4,
                           'Th'=5,
                           'F'=6,
                           'Sa'=7)
  levels(data$First_road_class) <-list('Motorway'=1,
                                       'A(M)'=2,
                                       'A'=3,
                                       'B'=4,
                                       'C'=5,
                                       'Unclassified'=6,
                                       NA)
  levels(data$Road_Type) <-list('Roundabout'=1,
                                'One way'=2,
                                'Dual cway'=3,
                                'Single cway'=6,
                                'Slip road'=7,
                                'Unknown'=9,
                                'One way/Slip road'=12,
                                NA)
  levels(data$Junction_Detail) <-list('Not at junction'=0,
                                      'Roundabout'=1,
                                      'Mini-roundabout'=2,
                                      'T/staggered'=3,
                                      'Slip road'=5,
                                      'Crossroads'=6,
                                      '4+ arms'=7,
                                      'Private drive'=8,
                                      'Other'=9,
                                      NA)
  levels(data$Junction_Control) <-list('Not at junction'=0,
                                       'Authorised person'=1,
                                       'Signal'=2,
                                       'Stop sign'=3,
                                       'Yield/uncontrolled'=4,
                                       NA)
  levels(data$Second_road_class) <-list('Not at junction'=0,
                                        'Motorway'=1,
                                        'A(M)'=2,
                                        'A'=3,
                                        'B'=4,
                                        'C'=5,
                                        'Unclassified'=6,
                                        NA)
  levels(data$Ped_xing_human) <-list('None'=0,
                                     'Crossing guard'=1,
                                     'Other person'=2,
                                     NA)
  levels(data$Ped_xing_physical) <-list('None'=0,
                                        'Zebra'=1,
                                        'Non-junction ped light crossing'=4,
                                        'Ped phase'=5,
                                        'Footbridge or subway'=7,
                                        'Central refuge'=8,
                                        NA)
  levels(data$Light) <-list('Daylight'=1,
                            'Dark - lights lit'=4,
                            'Dark - lights unlit'=5,
                            'Dark - no lighting'=6,
                            'Dark - light unknown'=7,
                            NA)
  levels(data$Weather) <-list('Fine'=1,
                              'Rain'=2,
                              'Snowing'=3,
                              'Fine/Windy'=4,
                              'Raining/Windy'=5,
                              'Snowing/Windy'=6,
                              'Fog/Mist'=7,
                              'Other'=8,
                              'Unknown'=9,
                              NA)
  levels(data$Surface) <-list('Dry'=1,
                              'Wet/Damp'=2,
                              'Snow'=3,
                              'Frost/Ice'=4,
                              'Flood over 3cm. deep'=5,
                              'Oil/Diesel'=6,
                              'Mud'=7,
                              NA)
  levels(data$Special) <-list('None'=0,
                              'T Signal out'=1,
                              'T Signal defective'=2,
                              'Sign/markng defective/obscured'=3,
                              'Roadworks'=4,
                              'Surface defective'=5,
                              'Oil/Diesel'=6,
                              'Mud'=7,
                              NA)
  levels(data$Hazards) <-list('None'=0,
                              'Load'=1,
                              'Object'=2,
                              'Previous accident'=3, 
                              'Dog'=4,
                              'Other animal'=5,
                              'Ped in carriageway uninjured'=6,
                              'Animal in carriageway not horse'=7,
                              NA)
  levels(data$Urban_or_Rural) <-list('Urban'=1,
                                     'Rural'=2,
                                     'Unallocated'=3) 
  levels(data$Police_attendance) <- list('Yes'=1,
                                         'No'=2,
                                         'No - self form'=3)
  
  # Convert Time
  data$Time <- as.character(data$Time)
  data$Time <- sapply(strsplit(data$Time, ':'),
                               function(x) {
                                 x <- as.numeric(x)
                                 x[1]*60+x[2]
                               }
                     )
  
  # Create Time Period
  period <- c(ifelse(data$Time < 240,
                     1,
                     ifelse(data$Time < 480,
                            2,
                            ifelse(data$Time < 720,
                                   3,
                                   ifelse(data$Time < 960,
                                          4,
                                          ifelse(data$Time < 1200,
                                                 5,
                                                 ifelse(data$Time < 1440,
                                                        6,
                                                        NA)))))))
  
  data[,'Time Period'] <- factor(period, levels=c(1:6), labels=c('Overnight',
                                                                 'Early Morning',
                                                                 'Morning',
                                                                 'Afternoon',
                                                                 'Evening',
                                                                 'Night'))
  
  # Convert Date to US format
  data$Date <- format(as.Date(data$Date, format='%d/%m/%Y'), format='%m/%d')
  
  # Create isHoliday (UK Bank Days for 2012)
  holidays <- c('01/01', '01/02', '03/17', '04/06', '04/09',
                '05/07', '06/04', '06/05', '07/12', '08/06',
                '08/27', '11/30', '12/25', '12/26')
  isHoliday <- c(ifelse(data$Date %in% holidays, TRUE, FALSE))
  
  data[,'isHoliday'] <- isHoliday
  
  
  # Create isWeekend
  isWeekend <- c(ifelse(data$Day %in% c('F', 'Sa', 'Su'), TRUE, FALSE))
  data[,'isWeekend'] <- isWeekend
  
  # Make isWeekend a Factor to cooperate with glm
  data$isWeekend <- factor(data$isWeekend)
  
  # Create Month
  months <- format(as.Date(data$Date, format='%m/%d'), format='%m')
  months <- c(ifelse(months == '01',
                     1,
                     ifelse(months == '02',
                            2,
                            ifelse(months == '03',
                                   3,
                                   ifelse(months == '04',
                                          4,
                                          ifelse(months == '05',
                                                 5,
                                                 ifelse(months == '06',
                                                        6,
                                                        ifelse(months == '07',
                                                               7,
                                                               ifelse(months == '08',
                                                                      8,
                                                                      ifelse(months == '09',
                                                                             9,
                                                                             ifelse(months == '10',
                                                                                    10,
                                                                                    ifelse(months == '11',
                                                                                           11,
                                                                                           ifelse(months == '12',
                                                                                                  12,
                                                                                                  NA)))))))))))))
  
  data[,'Month'] <- months
  data$Month <- factor(data$Month, levels=c(1:12), labels=c('JAN',
                                                            'FEB',
                                                            'MAR',
                                                            'APR',
                                                            'MAY',
                                                            'JUN',
                                                            'JUL',
                                                            'AUG',
                                                            'SEP',
                                                            'OCT',
                                                            'NOV',
                                                            'DEC'))
  
  
  # Remove intuitively useless columns
  drop <- c('Index',
            'E_OSGR',
            'N_OSGR',
            'Date',
            'Time',
            'LA_highway',
            'Police_attendance',
            'LSOA')
  
  data = data[,!names(data) %in% drop]
  
  # Clear data of NA
  cleanData <- subset(data, Junction_Control != -1)
  cleanData <- subset(cleanData, Second_road_class != -1)
  cleanData <- subset(cleanData, Surface != -1)
  
  # Prep cleaner datasets
  perfectData <- cleanData
  
  # Reclassifying levels for noLSOAData
  levels(perfectData$First_road_class) <-list('Motorway'=1,
                                              'A(M)'=2,
                                              'A'=3,
                                              'B'=4,
                                              'C'=5,
                                              NA)
  levels(perfectData$Road_Type) <-list('Roundabout'=1,
                                       'One way'=2,
                                       'Dual cway'=3,
                                       'Single cway'=6,
                                       'Slip road'=7,
                                       'One way/Slip road'=12,
                                       NA)
  levels(perfectData$Second_road_class) <-list('Not at junction'=0,
                                               'Motorway'=1,
                                               'A(M)'=2,
                                               'A'=3,
                                               'B'=4,
                                               'C'=5,
                                               NA)
  levels(perfectData$Light) <-list('Daylight'=1,
                                   'Dark - lights lit'=4,
                                   'Dark - lights unlit'=5,
                                   'Dark - no lighting'=6,
                                   NA)
  levels(perfectData$Weather) <-list('Fine'=1,
                                     'Rain'=2,
                                     'Snowing'=3,
                                     'Fine/Windy'=4,
                                     'Raining/Windy'=5,
                                     'Snowing/Windy'=6,
                                     'Fog/Mist'=7,
                                     'Other'=8,
                                     NA)
  
  
  # Remove all unknowns and unclassifieds
  perfectData <- subset(perfectData, !is.na(First_road_class))
  perfectData <- subset(perfectData, !is.na(Second_road_class))
  perfectData <- subset(perfectData, !is.na(Road_Type))
  perfectData <- subset(perfectData, !is.na(Light))
  perfectData <- subset(perfectData, !is.na(Weather))
  
  
  # Prepping data without Junction_Control, First_road_class, Second_road_class
  
  # Remove columns with high impact loss
  drop2 <- c('Junction_Control',
             'First_road_class',
             'Second_road_class')
  
  bigData <- data[,!names(data) %in% drop2]
  
  # Clean big data
  cleanBigData <- subset(bigData, Surface != -1)
  
  levels(cleanBigData$Road_Type) <-list('Roundabout'=1,
                                        'One way'=2,
                                        'Dual cway'=3,
                                        'Single cway'=6,
                                        'Slip road'=7,
                                        'One way/Slip road'=12,
                                        NA)
  levels(cleanBigData$Light) <-list('Daylight'=1,
                                    'Dark - lights lit'=4,
                                    'Dark - lights unlit'=5,
                                    'Dark - no lighting'=6,
                                    NA)
  levels(cleanBigData$Weather) <-list('Fine'=1,
                                      'Rain'=2,
                                      'Snowing'=3,
                                      'Fine/Windy'=4,
                                      'Raining/Windy'=5,
                                      'Snowing/Windy'=6,
                                      'Fog/Mist'=7,
                                      'Other'=8,
                                      NA)
  cleanBigData <- subset(cleanBigData, !is.na(Road_Type))
  cleanBigData <- subset(cleanBigData, !is.na(Light))
  cleanBigData <- subset(cleanBigData, !is.na(Weather))
  
  
  # Prepping data that only contains significant variables for glm()
  sigData <- data
  keep <- c('Lat',
            'Police_Force',
            'Vehicles',
            'Casualties',
            'Road_Type',
            'Speed_limit',
            'Ped_xing_physical',
            'Light',
            'Weather',
            'Surface')
  
  sigData <- sigData[, names(sigData) %in% keep]
  
  sigData <- subset(sigData, Surface != -1)
  
  levels(sigData$Road_Type) <-list('Roundabout'=1,
                                   'One way'=2,
                                   'Dual cway'=3,
                                   'Single cway'=6,
                                   'Slip road'=7,
                                   'One way/Slip road'=12,
                                   NA)
  levels(sigData$Light) <-list('Daylight'=1,
                               'Dark - lights lit'=4,
                               'Dark - lights unlit'=5,
                               'Dark - no lighting'=6,
                               NA)
  levels(sigData$Weather) <-list('Fine'=1,
                                 'Rain'=2,
                                 'Snowing'=3,
                                 'Fine/Windy'=4,
                                 'Raining/Windy'=5,
                                 'Snowing/Windy'=6,
                                 'Fog/Mist'=7,
                                 'Other'=8,
                                 NA)
  sigData <- subset(sigData, !is.na(Road_Type))
  sigData <- subset(sigData, !is.na(Light))
  sigData <- subset(sigData, !is.na(Weather))
  
  # Remove temporary vectors
  rm(drop, drop2, keep, holidays, isHoliday, isWeekend, months, period)
  
  ##########################
  # DATA SETS
  #
  # data - 145571 obs. of 28 variables
  #
  # cleanData - 87586 obs. of 28 variables
  #
  # perfectData - 25281 obs. of 28 variables
  #
  # bigData - 145571 obs. of 25 variables
  #
  # cleanBigData - 140057 obs. of 25 variables
  #
  # sigData - 140057 obs. of 10 variables
  #
  ##########################
  
  
  
  # JOHN CENTRITTO
  
  # VARIABLE SELECTION
  
  # Default to clean data so we can remove variables without affecting base data
  selectedData <- perfectData
  
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
  
  library(ROSE)
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
  boost.fit <- boosting(Severity ~ Light+Weather+Road_Type+Surface+Casualties
                        +Speed_limit+Vehicles+Day+Junction_Detail+Junction_Control
                        +Hazards+Ped_xing_human+Urban_or_Rural
                        +First_road_class+Second_road_class
                        , data=balancedTrainData, mfinal=10)
  
  # Predict on test data
  predboosting<- predict.boosting(boost.fit, newdata=testData)
  
  predboosting
  
  importanceplot(boost.fit)

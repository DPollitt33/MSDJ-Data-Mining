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
  
  data[,'Time_Period'] <- factor(period, levels=c(1:6), labels=c('Overnight',
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
  
  # Make isHoliday a Factor to cooperate with glm
  data$isHoliday <- factor(data$isHoliday)
  
  
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
  
  # Perform corrplot
  library(corrplot)
  numericLocData <- data.frame(EOSGR=data$E_OSGR, NOSGR=data$N_OSGR,
                               Lat=data$Lat, Long=data$Long)
  corrplot(cor(numericLocData), method='number', type='upper')
  
  
  # Remove intuitively useless columns
  drop <- c('Index',
            'E_OSGR',
            'N_OSGR',
            'Date',
            'Time',
            'LA_highway',
            'Police_attendance',
            'LSOA')
  
  # Remove columns with too many levels, giving R trouble
  drop <- c(drop,
            'First_road_number',
            'Second_road_number',
            'LA_district')
  
  data = data[,!names(data) %in% drop]
  
  # Remove Speed_limit outlier
  data = subset(data, as.numeric(Speed_limit) != 1)
  
  # Clear data of NA
  cleanData <- subset(data, Junction_Control != -1)
  cleanData <- subset(cleanData, Second_road_class != -1)
  cleanData <- subset(cleanData, Surface != -1)
  
  # Prep cleaner datasets
  perfectData <- cleanData
  
  # Reclassifying levels for perfectData
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
  
  
  # Splitting data by significant police forces
  pf <- c('Police_Force')
  lancashireData <- data[which(data$Police_Force=='4'),]
  northumbriaData <- data[which(data$Police_Force=='10'),]
  northYorkshireData <- data[which(data$Police_Force=='12'),]
  southYorkshireData <- data[which(data$Police_Force=='14'),]
  humbersideData <- data[which(data$Police_Force=='16'),]
  lincolnshireData <- data[which(data$Police_Force=='32'),]
  gloucestershireData <- data[which(data$Police_Force=='53'),]
  grampianData <- data[which(data$Police_Force=='92'),]
  fifeData <- data[which(data$Police_Force=='94'),]
  lothianData <- data[which(data$Police_Force=='95'),]
  strathclydeData <- data[which(data$Police_Force=='97'),]
  
  lancashireData <- lancashireData[, !names(lancashireData) %in% 'Police_Force']
  northumbriaData <- northumbriaData[, !names(northumbriaData) %in% 'Police_Force']
  northYorkshireData <- northYorkshireData[, !names(northYorkshireData) %in% 'Police_Force']
  southYorkshireData <- southYorkshireData[, !names(southYorkshireData) %in% 'Police_Force']
  humbersideData <- humbersideData[, !names(humbersideData) %in% 'Police_Force']
  lincolnshireData <- lincolnshireData[, !names(lincolnshireData) %in% 'Police_Force']
  gloucestershireData <- gloucestershireData[, !names(gloucestershireData) %in% 'Police_Force']
  grampianData <- grampianData[, !names(grampianData) %in% 'Police_Force']
  fifeData <- fifeData[, !names(fifeData) %in% 'Police_Force']
  lothianData <- lothianData[, !names(lothianData) %in% 'Police_Force']
  strathclydeData <- strathclydeData[, !names(strathclydeData) %in% 'Police_Force']
  
  # Remove temporary vectors
  rm(drop, drop2, keep, holidays, isHoliday, isWeekend, months, period, pf)
  
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
  # data sets for each significant police_force by glm of full data
  #
  ##########################
  
  
  
  # JOHN CENTRITTO


# HELPER FUNCTIONS

library(ROSE)
# FUNCTION: balanceDataBySeverity
# Function to balance data using ROSE synthetic data library
# @param inputData - data to balance (train data) Can only handle continous and
# categorical variables due to ROSE API
# @param size - size of each severity stratum after balancing
# @return balancedTrainData - new balanced data set
balanceDataBySeverity <- function(inputData, size) {
  # Subset the data by severity
  severity1Stratum <- subset(inputData, Severity == "Fatal")
  severity2Stratum <- subset(inputData, Severity == "Serious")
  severity3Stratum <- subset(inputData, Severity == "Slight")
  
  # Use ROSE to balance the input data set
  # ROSE can only act on binary responses so use it on severity 1 & 2
  # and severity 2 & 3 separately
  sev1.2.data <- rbind(severity1Stratum, severity2Stratum)
  balanced.sev1.2.data <- ROSE(Severity~., data=sev1.2.data, N=size*2)$data
  
  sev2.3.data <- rbind(severity2Stratum, severity3Stratum)
  balanced.sev2.3.data <- ROSE(Severity~., data=sev2.3.data, N=size*2)$data
  
  # Combine all 3 data groupings
  balancedTrainData <- rbind(subset(balanced.sev1.2.data, Severity == "Fatal"), balanced.sev2.3.data)
  
  return(balancedTrainData)
}

# USING A SIMPLE DECISION TREE TO PREDICT SEVERITY

# Sample 80% of the data for training
set.seed(1)
spl <- sample(1:nrow(cleanBigData), round(0.8*nrow(cleanBigData)))
trainData <- cleanBigData[spl,]
testData <- cleanBigData[-spl,]

library(tree)
# Train decision tree model
tree.fit <- tree(Severity ~ .  -Police_Force
                 , data=trainData)

summary(tree.fit)

# View the tree
plot(tree.fit)  
text(tree.fit, pretty=0)   

# Use the decision tree to predict on the test data
tree.pred <- predict(tree.fit, testData, type="class") 
(confMatrix <- table(tree.pred, testData$Severity))
# Find the accuracy of the predictions
(accuracy <- (confMatrix[1] + confMatrix[5] +confMatrix[9])/nrow(testData))  


# USING BOOSTING TO PREDICT SEVERITY
library(adabag)
set.seed(3)
spl <- sample(1:nrow(cleanBigData), round(0.3*nrow(cleanBigData)))
smallerSet <- cleanBigData[spl,] #Boosting is computationally expensive. Use less data
# Now create train and test set
spl <- sample(1:nrow(smallerSet), round(0.8*nrow(smallerSet)))
trainData <- smallerSet[spl,]
testData <- smallerSet[-spl,]

# Create ensemble through boosting
boost.fit <- boosting(Severity ~ Light+Weather+Road_Type+Surface+Casualties
                      +Speed_limit+Vehicles+Ped_xing_human+Urban_or_Rural
                      , data=trainData, mfinal=10,control = rpart.control(cp = -1))

# Predict on test data
predboosting<- predict.boosting(boost.fit, newdata=testData)
predboosting

# Balance data with respect to severity and retry boosting
# Get train and test suite on cleanBigData set
set.seed(4)
spl <- sample(1:nrow(cleanBigData), round(0.9*nrow(cleanBigData)))
trainData <- cleanBigData[spl,]
testData <- cleanBigData[-spl,]

# Call balance function
balancedTrainData <- balanceDataBySeverity(trainData, 1000)

# Create ensemble with balanced data
boost.fit <- boosting(Severity ~ Light+Weather+Road_Type+Surface+Casualties
                      +Speed_limit+Vehicles+Day
                      +Hazards+Ped_xing_human+Urban_or_Rural
                      , data=balancedTrainData, mfinal=10)

# Predict on test data
predboosting<- predict.boosting(boost.fit, newdata=testData)
predboosting

#Plot variable importance
importanceplot(boost.fit)



# USING LOGISTIC REGRESSION TO PREDICT FATAL ACCIDENTS

# Change data to either fatal (1) or non fatal (2,3)
binarySelectedData <- cleanBigData
binarySelectedData$Severity <- ifelse(binarySelectedData$Severity != "Fatal","NonFatal","Fatal")
binarySelectedData$Severity <- as.factor(binarySelectedData$Severity)

# Create test/train set from this binary response value data
set.seed(3)
spl <- sample(1:nrow(binarySelectedData), round(0.8*nrow(binarySelectedData)))
trainData <- binarySelectedData[spl,]
testData <- binarySelectedData[-spl,]

# Fit a logistic model
glm.fit <- glm(Severity ~ . -Lat -Long 
               , data=trainData, family = "binomial")

summary(glm.fit)

# Predict on test data with logistic model
fatal.probs <- predict(glm.fit, newdata=testData, type = "response")
# Check results of probabilites
fatal.pred <- rep("Non-Fatal", length(fatal.probs))
fatal.pred[fatal.probs < 0.99] <- "FATAL"
# Measure model's performance
( confTable <- table(fatal.pred, testData$Severity) )
( accuracy <- (confTable[1] + confTable[4])/(nrow(testData)) )
( fatalRecall <- confTable[1] / (confTable[1]+confTable[2]) )
( fatalPrecision <- confTable[1] / (confTable[1]+confTable[3]) )


# Remove variables that are insignificant and train model again
glm.fit <- glm(Severity ~ . -isWeekend -isHoliday-Ped_xing_human -Month
                -Lat -Long -Police_Force
               , data=trainData, family = "binomial")

summary(glm.fit)

# Predict on test data with logistic model
fatal.probs <- predict(glm.fit, newdata=testData, type = "response")
# Check results of probabilites
fatal.pred <- rep("Non-Fatal", length(fatal.probs))
fatal.pred[fatal.probs < 0.99] <- "FATAL"
# Measure model's performance
( confTable <- table(fatal.pred, testData$Severity) )
( accuracy <- (confTable[1] + confTable[4])/(nrow(testData)) )
( fatalRecall <- confTable[1] / (confTable[1]+confTable[2]) )
( fatalPrecision <- confTable[1] / (confTable[1]+confTable[3]) )



#MATT KAPLAN
#plot causalty tree
#plot linear regression
spl <- sample(1:nrow(cleanBigData), round(0.9*nrow(cleanBigData)))
trainData <- cleanBigData[spl,]
testData <- cleanBigData[-spl,]

lm.fit <- lm(Casualties ~. , data=cleanBigData)
summary(lm.fit)

train.lm.fit <- lm(Casualties ~ Vehicles +Speed_limit , data=trainData)
test.lm.fit <- lm(Casualties~ Vehicles +Speed_limit , data=testData)

prediction1 <- predict(train.lm.fit, newdata=testData) 
prediction2 <- predict(test.lm.fit, newdata=testData) 

summary(train.lm.fit)
summary(test.lm.fit)
testData$Casualties

summary(prediction1)
difference1 <- abs(prediction1 - testData$Casualties)
difference1
mean(prediction1)
mse <- mean(difference1^2)
mse



# Train decision tree model
cas.tree.fit <- tree(Casualties ~ Speed_limit , data=trainData)

summary(cas.tree.fit)
plot(cas.tree.fit)  
text(cas.tree.fit, pretty=0)   


plot(as.factor(gloucestershireData$Casualties), main="GLOUCESTERSHIRE CASUALTIES")
plot(as.factor(gloucestershireData$Road_Type), main="GLOUCESTERSHIRE ROAD TYPE")
plot(as.factor(gloucestershireData$Speed_limit), main="GLOUCESTERSHIRE SPEED LIMIT")
plot(as.factor(gloucestershireData$Surface), main="GLOUCESTERSHIRE WEATHER")
plot(as.factor(gloucestershireData$Light), main="GLOUCESTERSHIRE LIGHT")

as.data.frame(table(gloucestershireData$Severity))
sum(as.numeric(gloucestershireData$Casualties))


plot(as.factor(lancashireData$Casualties), main="LANCASHIRE CASUALTIES")
plot(as.factor(lancashireData$Road_Type), main="LANCASHIRE ROAD TYPE")
plot(as.factor(lancashireData$Speed_limit), main="LANCASHIRE SPEED LIMIT")
plot(as.factor(lancashireData$Surface), main="LANCASHIRE WEATHER")
plot(as.factor(lancashireData$Light), main="LANCASHIRE LIGHT")

as.data.frame(table(lancashireData$Severity))
sum(as.numeric(lancashireData$Casualties))


plot(as.factor(northumbriaData$Casualties), main="NORTH UMBRIA CASUALTIES")
plot(as.factor(northumbriaData$Road_Type), main="NORTH UMBRIA ROAD TYPE")
plot(as.factor(northumbriaData$Speed_limit), main="NORTH UMBRIA SPEED LIMIT")
plot(as.factor(northumbriaData$Surface), main="NORTH UMBRIA WEATHER")
plot(as.factor(northumbriaData$Light), main="NORTH UMBRIA LIGHT")
as.data.frame(table(northumbriaData$Severity))
sum(as.numeric(northumbriaData$Casualties))


plot(as.factor(southYorkshireData$Casualties), main="SOUTH YORKSHIRE CASUALTIES")
plot(as.factor(southYorkshireData$Road_Type), main="SOUTH YORKSHIRE ROAD TYPE")
plot(as.factor(southYorkshireData$Speed_limit), main="SOUTH YORKSHIRE SPEED LIMIT")
plot(as.factor(southYorkshireData$Surface), main="SOUTH YORKSHIRE WEATHER")
plot(as.factor(southYorkshireData$Light), main="SOUTH YORKSHIRE LIGHT")

as.data.frame(table(southYorkshireData$Severity))
sum(as.numeric(southYorkshireData$Casualties))

plot(as.factor(humbersideData$Casualties) , main="HUMBERSIDE CASUALTIES")
plot(as.factor(humbersideData$Road_Type), main="HUMBERSIDE ROAD TYPE")
plot(as.factor(humbersideData$Speed_limit), main="HUMBERSIDE SPEED LIMIT")
plot(as.factor(humbersideData$Surface), main="HUMBERSIDE WEATHER")
plot(as.factor(humbersideData$Light), main="HUMBERSIDE LIGHT")


as.data.frame(table(humbersideData$Severity))
sum(as.numeric(humbersideData$Casualties))


plot(as.factor(lincolnshireData$Casualties), main="LINCOLNSHIRE CASUALTIES")
plot(as.factor(lincolnshireData$Road_Type), main="LINCOLNSHIRE ROAD TYPE")
plot(as.factor(lincolnshireData$Speed_limit), main="LINCOLNSHIRE SPEED LIMIT")  
plot(as.factor(lincolnshireData$Surface), main="LINCOLNSHIRE WEATHER")
plot(as.factor(lincolnshireData$Light), main="LINCOLNSHIRE LIGHT")

as.data.frame(table(lincolnshireData$Severity))
sum(as.numeric(lincolnshireData$Casualties))


plot(as.factor(fifeData$Casualties), main="FIFE CASUALTIES")
plot(as.factor(fifeData$Road_Type), main="FIFE ROAD TYPE")
plot(as.factor(fifeData$Speed_limit), main="FIFE SPEED LIMIT")   
plot(as.factor(fifeData$Surface), main="FIFE WEATHER")
plot(as.factor(fifeData$Light), main="FIFE CLYDE LIGHT")



as.data.frame(table(fifeData$Severity))
sum(as.numeric(fifeData$Casualties))


plot(as.factor(grampianData$Casualties), main="GRAMPIAN CASUALTIES")
plot(as.factor(grampianData$Road_Type), main="GRAMPIAN ROAD TYPE")
plot(as.factor(grampianData$Speed_limit), main="GRAMPIAN SPEED LIMIT")
plot(as.factor(grampianData$Surface), main="GRAMPIAN WEATHER")
plot(as.factor(grampianData$Light), main="GRAMPIAN CLYDE LIGHT")

as.data.frame(table(grampianData$Severity))
sum(as.numeric(grampianData$Casualties))


plot(as.factor(lothianData$Casualties), main="LOTHIAN CASUALTIES")
plot(as.factor(lothianData$Road_Type), main="LOTHIAN ROAD TYPE")
plot(as.factor(lothianData$Speed_limit), main="LOTHIAN SPEED LIMIT")
plot(as.factor(lothianData$Surface), main="LOTHIAN WEATHER")
plot(as.factor(lothianData$Light), main="LOTHIAN CLYDE LIGHT")

as.data.frame(table(lothianData$Severity))
sum(as.numeric(lothianData$Casualties))



plot(as.factor(strathclydeData$Casualties), main="STRATH CLYDE CASUALTIES")
plot(as.factor(strathclydeData$Road_Type), main="STRATH CLYDE ROAD TYPE")
plot(as.factor(strathclydeData$Speed_limit), main="STRATH CLYDE SPEED LIMIT")
plot(as.factor(strathclydeData$Surface), main="STRATH CLYDE WEATHER")
plot(as.factor(strathclydeData$Light), main="STRATH CLYDE LIGHT")

as.data.frame(table(strathclydeData$Severity))
sum(as.numeric(strathclydeData$Casualties))
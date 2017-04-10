# TEAM MSDJs
# Data mining
# Final Project

setwd("C:\\Users\\Dakota\\Documents\\Real Documents\\School\\Data Mining\\Final Project\\Project1")
getwd()

rm(list = ls(all = TRUE))

# Load data
data <- read.csv('DfTRoadSafety_Accidents_2012.csv', header=TRUE)

# DATA PROCESSING

  # Factorize fields
  data$Police_Force <- factor(data$Police_Force)
  data$Accident_Severity <- factor(data$Accident_Severity)
  data$Day_of_Week <- factor(data$Day_of_Week)
  data$Local_Authority_.District. <- factor(data$Local_Authority_.District.)
  data$X1st_Road_Class <- factor(data$X1st_Road_Class)
  data$Road_Type <- factor(data$Road_Type)
  data$Junction_Detail <- factor(data$Junction_Detail)
  data$Junction_Control <- factor(data$Junction_Control)
  data$X2nd_Road_Class <- factor(data$X2nd_Road_Class)
  data$Pedestrian_Crossing.Human_Control <- factor(data$Pedestrian_Crossing.Human_Control)
  data$Pedestrian_Crossing.Physical_Facilities <- factor(data$Pedestrian_Crossing.Physical_Facilities)
  data$Light_Conditions <- factor(data$Light_Conditions)
  data$Weather_Conditions <- factor(data$Weather_Conditions)
  data$Road_Surface_Conditions <- factor(data$Road_Surface_Conditions)
  data$Special_Conditions_at_Site <- factor(data$Special_Conditions_at_Site)
  data$Carriageway_Hazards <- factor(data$Carriageway_Hazards)
  data$Urban_or_Rural_Area <- factor(data$Urban_or_Rural_Area)
  data$Did_Police_Officer_Attend_Scene_of_Accident <- factor(data$Did_Police_Officer_Attend_Scene_of_Accident)
  
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
  
  # Factors with missing values, as given by the guide
  # 1st Road 6-Unclassified?
  # Road Type -1
  # Junction Detail -1
  # Junction Control -1
  # 2nd Road 6-Unclassifed?
  # Ped Cross Human -1
  # Ped Cross Physical -1
  # Light Conditions -1
  # Weather -1
  # Road Surface -1
  # Special Conditions -1
  # Carriageway Hazards -1
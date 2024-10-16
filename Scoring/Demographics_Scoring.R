# This will produce demographic information that can be merged to other scored data

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(lubridate)


# Read in the file
IDTracker <- read_excel(paste0(DataLocation,"Final_DS/Screener/Matched_Siblings/Final_ID_Tracker.xlsx"))

# Keep the variables of interest
IDTracker2 <- select(IDTracker, Child_ID = ID, Sex = Child_Gender, Epilepsy, DOB = Child_Date_of_Birth, Screened_Age = Child_age)

# Load forward three tasks
Atlantis_Raw <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Children/Atlantis_Raw.xlsx"))
ZAT_Raw <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Children/ZAT_Raw.xlsx")) 
RecepVocab_Raw <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Children/RecepVocab_Raw.xlsx"))

# Keep only the IDs and the date of evaluation
Atlantis_Raw2 <- select(Atlantis_Raw, Child_ID = Child_s_Study_ID, DOE1 = Date_of_Evaluation)
ZAT_Raw2 <- select(ZAT_Raw, Child_ID, DOE2 = Date_of_Evaluation)
RecepVocab_Raw2 <- select(RecepVocab_Raw, Child_ID = Child_Study_ID, DOE3 = Date_of_Evaluation)

# Merge these datasets into one
DOE <- Atlantis_Raw2 %>%
  full_join(ZAT_Raw2, by = "Child_ID") %>%
  full_join(RecepVocab_Raw2, by = "Child_ID")
  
#Some data cleaning, keep the dates and not the time
DOE$DOE1 <- substr(DOE$DOE1, 1, 10)
DOE$DOE2 <- substr(DOE$DOE2, 1, 10)
DOE$DOE3 <- substr(DOE$DOE3, 1, 10)

# Introduce the DOB variable into DOE
DOE <- DOE %>%
  left_join(select(IDTracker2, Child_ID, DOB), by = "Child_ID")

# Transforming all dates into dates (right now they are characters)
DOE <- DOE %>%
  mutate(DOE1 = ymd(DOE1),
         DOE2 = ymd(DOE2),
         DOE3 = ymd(DOE3),
         DOB = ymd(DOB))

# Calculating Age for each DOE
DOE <- DOE %>%
  mutate(Age1 = as.numeric(difftime(DOE1, DOB, units = "weeks"))/52,
         Age2 = as.numeric(difftime(DOE2, DOB, units = "weeks"))/52,
         Age3 = as.numeric(difftime(DOE3, DOB, units = "weeks"))/52)

# Create an Age variance metric
DOE$AgeMax <- round(apply(select(DOE, Age1:Age3), 1, function(x) max(x, na.rm = TRUE)),2)
DOE$AgeMin <- round(apply(select(DOE, Age1:Age3), 1, function(x) min(x, na.rm = TRUE)),2)
DOE$AgeRange <- round(DOE$AgeMax - DOE$AgeMin,2)

# Calculate the mean Age
DOE$Age <- round(apply(select(DOE, Age1:Age3), 1, function(x) mean(x, na.rm = TRUE)),2)

# Keep the variable of interest that will make it back into the 
DOE2 <- select(DOE, Child_ID, Age, AgeRange)

# Introduce this information into the variable for demographics
IDTracker3 <- IDTracker2 %>%
  left_join(DOE2, by = "Child_ID")

# Create a save pathway
save.pathway <- paste(DataLocation,"FINAL_DS/Demographics/",
                      "Demographics.xlsx", sep="")

# Save the scored data
write.xlsx(x= IDTracker3, file = save.pathway)


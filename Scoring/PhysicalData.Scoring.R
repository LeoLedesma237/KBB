# This is the script for scoring Physical Data


# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)

# Load in the data
Physical.data <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Children/PhysicalData_Raw.xlsx"))


# Check the IDs for errors
source("Scoring/scoring_functions/IDError_FUNCTION.R")
PD_Notes <-check_id_errors("Physical Data",
                           Physical.data$Child_ID)


# Select Vars of Interest
Front <- Physical.data %>%
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation)

Eyes.Items <- Physical.data %>%
  select(`_07_Close_Vision_Chart_Left_eye`:`_08_Close_Vision_Chart_Right_eye`)

Hearing.Items <- Physical.data %>%
  select(`_10`:`_60_001`, # hearing left
         `_10_004`:`_60_003`)

# Rename Items
names(Eyes.Items) <- c("Left.eye", "Right.eye")

# Renaming Hearing (more intense)
Left.Hearing.name <- paste("L",rep(seq(from = -10, to = 60, by = 5), each = 2),"dB", sep="") 
Right.Hearing.name <- paste("R",rep(seq(from = -10, to = 60, by = 5), each = 2),"dB", sep="")

for(ii in 1:length(Left.Hearing.name)) {
  
  if(ii %% 2 == 0) {
    
    Left.Hearing.name[ii] <-  paste(Left.Hearing.name[ii],"_2",sep="")
    Right.Hearing.name[ii] <- paste(Right.Hearing.name[ii],"_2",sep="")
  }
  
}

names(Hearing.Items) <- c(Left.Hearing.name, Right.Hearing.name)

# Data cleaning
Eyes.Items$Left.eye <- gsub(pattern = " ", replacement = "", Eyes.Items$Left.eye )
Eyes.Items$Right.eye <- gsub(pattern = " ", replacement = "", Eyes.Items$Right.eye)

# All reported eye measurements
sort(unique(c(Eyes.Items$Left.eye, Eyes.Items$Right.eye)))

# Acceptable eye measurements
acceptable.eye <- c("20/32", "20/25", "20/40", "20/20", "20/16", "20/50", "20/125", "20/100")

# Create a new variable for acceptable eye measurement
Eyes.Items <- Eyes.Items %>%
  mutate(Acceptable.Eye = case_when(
    is.na(Right.eye) | is.na(Left.eye) ~ "Missing eye data",
    Right.eye %in% acceptable.eye & Left.eye %in% acceptable.eye ~ "Yes",
    TRUE ~ "Needs further inspection"
    
  ))


# Score eye vision (total)
Eyes.Items <- Eyes.Items %>%
  mutate(Both.Eyes = ifelse(Acceptable.Eye == "Yes",
                            paste("20/",ceiling((as.numeric(gsub(".*/","",Eyes.Items$Left.eye))
                                                + as.numeric(gsub(".*/","",Eyes.Items$Right.eye)))/2),sep=""),
                            "Unable to Score"))

# Scoring Hearing
Hearing.Scoring.List <- list()

for(ii in 1:nrow(Hearing.Items)) {

  # Extract one row at a time
  current.row <- Hearing.Items[ii,]
  
  score.4000hz <- names(current.row)[which(current.row == 4000)]
  score.2000hz <- names(current.row)[which(current.row == 2000)]
  score.1000hz <- names(current.row)[which(current.row == 1000)]
  
  # Extracting the decible
  score.4000hz <- gsub(pattern ="_2|L|R", replacement = "", x = score.4000hz)
  score.2000hz <- gsub(pattern ="_2|L|R", replacement = "", x = score.2000hz)
  score.1000hz <- gsub(pattern ="_2|L|R", replacement = "", x = score.1000hz)
  
  if(length(score.4000hz) == 2 & length(score.2000hz) == 2 & length(score.1000hz) == 2) {
  # Create a dataframe with decibels heared for each Hz
  scoring.df <- tibble(Ear = c("Left", "Right"),
                        `1000Hz` = score.4000hz,
                        `2000Hz` = score.2000hz,
                        `4000Hz` = score.1000hz)
  
  # Convert it to wide format
  scoring.df.wide <- scoring.df %>%
    pivot_wider(names_from = Ear, values_from = c(`1000Hz`, `2000Hz`, `4000Hz`))
  
  # Save it into a list 
  Hearing.Scoring.List[[ii]] <- scoring.df.wide
  
  } else {
    
    # If there is suspect data collection error, return all NA's
    Hearing.Scoring.List[[ii]] <-  tibble(`1000Hz_Left` = NA,
                                          `1000Hz_Right` = NA,
                                          `2000Hz_Left` = NA,
                                          `2000Hz_Right` = NA, 
                                          `4000Hz_Left` = NA,
                                          `4000Hz_Right` = NA)
                                          
}

}

# Reintroduce the scores back into the dataset
PhysicalData2 <- cbind(Front, Eyes.Items, Hearing.Items,do.call(rbind,Hearing.Scoring.List))

# Create a save pathway
save.pathway_PD <- paste(DataLocation,"FINAL_DS/Behavioral/Children/",
                         "PhysicalData.xlsx", sep="")

# Save the scored data
write.xlsx(x= PhysicalData2, file = save.pathway_PD)


# Create a save pathway for Notes
save.pathway.notes <- paste(DataLocation,"REPORTS/Individual/",
                            "PhysicalData.csv", sep="")

# Save the Notes as a CSV
write_csv(x = PD_Notes, save.pathway.notes)


# Make a note that the data was saved successfully
cat("Saving processed Physical Data\n")

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation"))])


# Set a pause time for 1 second
Sys.sleep(1)

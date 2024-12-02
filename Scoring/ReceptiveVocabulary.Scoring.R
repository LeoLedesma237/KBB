# This is the script for scoring Receptive Vocabulary


# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr) # str_count()


# Read in the file
RecepVocab <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Children/RecepVocab_Raw.xlsx"))

# Load in Answer Key
answer_key <- read_excel(paste0(DataLocation,"RAW_DATA/AnswerKeys/RecepVocab_AnswerKey.xlsx"))

# Create a save pathway
save.pathway_RV <- paste(DataLocation,"FINAL_DS/Behavioral/Children/RecepVocab.xlsx", sep="")

# Create a save pathway for Notes
save.pathway.notes <- paste(DataLocation,"REPORTS/Individual/RecepVocab.csv", sep="")


###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############

# Rename Child_Study_ID var
RecepVocab <- rename(RecepVocab, Child_ID = Child_Study_ID)

# Check the IDs for errors
source("Scoring/scoring_functions/IDError_FUNCTION.R")
RV_Notes <-check_id_errors("Receptive Vocabulary",
                RecepVocab$Child_ID)


# Select Vars of Interest
Front <- RecepVocab %>%
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation)

Items <- RecepVocab %>%
  select(PPV1:PPV30)

# Rename the items
names(Items) <- paste0("RV_",1:length(Items),"_Raw")

# Call the function to score each item in Receptive Vocabulary
source("Scoring/scoring_functions/Scoring_FUNCTION0.R")
source("Scoring/scoring_functions/Scoring_FUNCTION1.R")

# Score each Item
Items2 <- scoring_function0(Items, answer_key)

# Rename Items the scored Items
names(Items2) <- paste0("RV_",1:length(Items2))

# Get the score for the dataset
Items3 <- scoring_function1(Items2, 30)

# Save the raw and scored data together
RecepVocab <- cbind(Front, Items, Items3)

# Save the scored data
write.xlsx(x= RecepVocab, file = save.pathway_RV)


# Save the Notes as a CSV
write_csv(x = RV_Notes, save.pathway.notes)

# Make a note that the data was saved successfully
cat("Saving processed Receptive Vocabulary\n")

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation"))])

# Set a pause time for 1 second
Sys.sleep(1)

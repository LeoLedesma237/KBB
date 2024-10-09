# This is the script for scoring Receptive Vocabulary


# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr) # str_count()


# Read in the file
RecepVocab <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Children/RecepVocab_Raw.xlsx"))

# Rename Child_Study_ID var
RecepVocab <- rename(RecepVocab, Child_ID = Child_Study_ID)

# Check the IDs for errors
source("Scoring/IDError_FUNCTION.R")
RV_Notes <-check_id_errors("Receptive Vocabulary",
                RecepVocab$Child_ID)


# Select Vars of Interest
Front <- RecepVocab %>%
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation)

Items <- RecepVocab %>%
  select(PPV1:PPV30)

# Rename Items (Part 1)
names(Items) <- 1:length(Items)

# Cbind them
RecepVocab2 <- tibble(cbind(Front,Items))

# Count the number of 777 (CDK), 888 (NRG), NAs, items given, and total number of items
RecepVocab2$CDK <- rowSums(mutate(Items,across(everything(), ~ str_count(., "777"))), na.rm = T)
RecepVocab2$NRG <- rowSums(mutate(Items,across(everything(), ~ str_count(., "888"))), na.rm = T)
RecepVocab2$NA.Num <- rowSums(is.na(Items))
RecepVocab2$Items.Given <- rowSums(!(is.na(Items)))
RecepVocab2$Total.Items <- length(Items)

# Load in Answer Key
answer_key <- read_excel(paste0(DataLocation,"RAW_DATA/AnswerKeys/RecepVocab_AnswerKey.xlsx"))
answer_key.vec <- do.call(c, c(answer_key))

# Create a function that takes the sum of correct responses
Scoring.fun <- function(vector) {
  
  scores <- sum(vector == answer_key.vec)
  return(scores)
}

# Score Performance
RecepVocab2$Performance <- apply(Items, 1, Scoring.fun)

# Rename the variables
names(RecepVocab2) <- c(names(RecepVocab2)[1:3], paste("RV_",names(RecepVocab2)[4:39], sep=""))

# Create a save pathway
save.pathway_RV <- paste(DataLocation,"FINAL_DS/Behavioral/Children/",
                         "RecepVocab.xlsx", sep="")

# Save the scored data
write.xlsx(x= RecepVocab2, file = save.pathway_RV)

# Create a save pathway for Notes
save.pathway.notes <- paste(DataLocation,"REPORTS/Individual/",
                            "RecepVocab.csv", sep="")


# Save the Notes as a CSV
write_csv(x = RV_Notes, save.pathway.notes)

# Make a note that the data was saved successfully
cat("Saving processed Receptive Vocabulary\n")

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation"))])

# Set a pause time for 1 second
Sys.sleep(1)

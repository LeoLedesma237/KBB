# This is the script for scoring Pediactric Symptom Checklist
# We are starting from the Root Directory /KBB
Root.Folder <- "C:/Users/lledesma.TIMES/Documents/KBB"
setwd(Root.Folder)

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr) # str_count()

# Read in the file
PSC <- read_excel("2_behavioral_assessments/Adults/Raw/PSC_Raw.xlsx")

# Rename Child_Study_ID var
PSC <- rename(PSC, Child_ID = Child_Study_ID)

# Check the IDs for errors
source("2_behavioral_assessments/id_error_function.R")
PSC_Notes <-check_id_errors("Pediatric Symptom Checklist",
                            PSC$Child_ID)


# Select Vars of Interest
Front <- PSC %>%
  select(Child_ID,
         Evaluator_ID = Evalutator_ID,
         Date_of_Evaluation)

Items <- PSC %>%
  select(`_1_Utongauka_kuciswa`:`_40_Ulalyaaba_kugwas_kakwiina_akwaambilwa`)

# Convert Items to numeric
Items <- data.frame(sapply(Items, function(x) as.numeric(x)))

# Rename Items (Part 1)
names(Items) <- 1:length(Items)

# Cbind them
PSC2 <- tibble(cbind(Front,Items))

# Count the number NAs, items given, and total number of items
PSC2$NA.Num <- rowSums(is.na(Items))
PSC2$Items.Given <- rowSums(!(is.na(Items)))
PSC2$Total.Items <- length(Items)

# Scoring Performance
PSC2$Performance = rowSums(Items)
  
# Rename the variables
names(PSC2) <- c(names(PSC2)[1:3], paste("PSC_",names(PSC2)[4:47], sep=""))

# Create a save pathway
save.pathway_PSC <- paste(Root.Folder,"/",
                         "2_behavioral_assessments/Adults/Processed", "/",
                         "PSC.xlsx", sep="")

# Save the scored data
write.xlsx(x= PSC2, file = save.pathway_PSC)
  

# Create a save pathway for Notes
save.pathway.notes <- paste(Root.Folder,"/",
                            "2_behavioral_assessments/Adults/Processed_Notes", "/",
                            "PSC.csv", sep="")

# Save the Notes as a CSV
write_csv(x = PSC_Notes, save.pathway.notes)

# Make a note that the data was saved successfully
cat("Saving processed Pediatric Symptom Checklist\n")

# Remove all global environment objects to declutter
rm(list=ls())

# Set the working directory to the scoring scripts
setwd("~/GitHub/LeoWebsite/KBB.Scripts/Scoring")

# Set a pause time for 1 second
Sys.sleep(1)
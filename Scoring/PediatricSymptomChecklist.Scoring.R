# This is the script for scoring Pediactric Symptom Checklist

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr) # str_count()

# Read in the file
PSC <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Adults/PSC_Raw.xlsx"))

# Create a save pathway
save.pathway_PSC <- paste(DataLocation,"FINAL_DS/Behavioral/Children/PSC.xlsx", sep="")

# Create a save pathway for Notes
save.pathway.notes <-  paste(DataLocation,"REPORTS/Individual/PSC.csv", sep="")


###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############


# Rename Child_Study_ID var
PSC <- rename(PSC, Child_ID = Child_Study_ID)

# Check the IDs for errors
source("Scoring/scoring_functions/IDError_FUNCTION.R")
PSC_Notes <-check_id_errors("Pediatric Symptom Checklist",
                            PSC$Child_ID)


# Select Vars of Interest
Front <- PSC %>%
  select(Child_ID,
         Evaluator_ID = Evalutator_ID,
         Date_of_Evaluation)

# Only items to be scored (1:40)
Items <- PSC %>%
  select(`_1_Utongauka_kuciswa`:`_40_Ulalyaaba_kugwas_kakwiina_akwaambilwa`)

# Scores items plus an additional few variables (not total score)
All_Items <- PSC %>%
  select(`_1_Utongauka_kuciswa`:`Sena_mwana_wenu_kuli_dika_kuti_agwasyigwe`) %>%
  select(-Mweelwe_wajanwa_total_Score_)

# Convert Items to numeric
Items <- data.frame(sapply(Items, function(x) as.numeric(x)))

# Rename Items
names(Items) <- paste0("PS_",1:length(Items))
names(All_Items) <- c(paste0("PS_",1:length(Items)), names(All_Items)[length(Items)+1:length(All_Items)])

# Call the function to score each item in Receptive Vocabulary
source("Scoring/scoring_functions/Scoring_FUNCTION1.R")

# Get the score for the dataset
Items <- scoring_function1(Items, 40)

# Introduce the scored metrics into the dataset
PSC <- cbind(Front, All_Items, select(Items, StopRule_Num:Performance))

# Save the scored data
write.xlsx(x= PSC, file = save.pathway_PSC)
  
# Save the Notes as a CSV
write_csv(x = PSC_Notes, save.pathway.notes)

# Make a note that the data was saved successfully
cat("Saving processed Pediatric Symptom Checklist\n")

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation"))])

# Set a pause time for 1 second
Sys.sleep(1)

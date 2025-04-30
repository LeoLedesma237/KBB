# Instructions:
# Run the first 23 lines of code
# After data have been imported and scored, run the first 20 lines of code
# Run the remaining lines of code, manually score, then rerun the remaining again 


# Set the location for your working directory (Where your scripts are saved)
WorkingDirectory <- "C:/Users/lledesma.TIMES/Documents/GitHub/KBB/"

# Set the location for where your data is saved
DataLocation <- "C:/Users/lledesma.TIMES/Documents/KBB/Data/"

# Set the pathway for the final screener data
FinalData_PW <- paste0(DataLocation,"FINAL_DS/Screener/")

# Set the pathway to the final matched sibling data
MatchedSibling_PW <- paste0(DataLocation,"FINAL_DS/Screener/Matched_Siblings/")

# Set the working directory
setwd(WorkingDirectory)

# Run the main script that uses API to download the data and score it
source("MainScripts/MainScript_ImportingAllDatafromKoboToolBoxandScoringIt.R")

#########################                          ############################
######################                                 ########################
###################### REST OF THE SCRIPT IS AUTOMATIC ########################
######################                                 ########################
##########################                        #############################


# Remove all global environment objects to declutter
#rm(list=ls())

# Load in each dataset (All Recently Scored)
Incorrect.Screeners <- read_excel(paste0(FinalData_PW, "1) Incorrect Screeners (level 1).xlsx"))
Excluded.Children <- read_excel(paste0(FinalData_PW, "2) Excluded Children (level 1).xlsx"))
HOH.No.Matches.unnested <- read_excel(paste0(FinalData_PW, "3) HOH No Matches (level 1).xlsx"))
HOH.Potential.Matches.unnested <- read_excel(paste0(FinalData_PW, "4) HOH Potential Matches (level 1).xlsx"))


# Load in the IDs that have been categoriazed in Final_ID_Tracker.xlsx 
Siblings <- read_excel(paste0(MatchedSibling_PW,"Final_ID_Tracker.xlsx"), sheet= "Siblings")
Half_Siblings <- read_excel(paste0(MatchedSibling_PW,"Final_ID_Tracker.xlsx"), sheet= "Half-Siblings")
Other <- read_excel(paste0(MatchedSibling_PW,"Final_ID_Tracker.xlsx"), sheet= "Other")

# Combine these ID's into accounted for IDs
accounted.for.IDs <- c(Siblings$Child_ID, Half_Siblings$Child_ID, Other$Child_ID)

# Remove rows from these datasets if the Child_ID is present in the Final_ID_Dataset
Incorrect.Screeners.final <- Incorrect.Screeners %>%
  filter(!(Child_ID %in% accounted.for.IDs))

Excluded.Children.final <- Excluded.Children %>%
  filter(!(Child_ID %in% accounted.for.IDs))
  
HOH.No.Matches.unnested.final <- HOH.No.Matches.unnested %>%
  filter(!(Child_ID %in% accounted.for.IDs))  

HOH.Potential.Matches.unnested.final <- HOH.Potential.Matches.unnested %>%
  filter(!(Child_ID %in% accounted.for.IDs))  


# Save the partialled out HOH Potential Matches
HOH.Potential.Matches.unnested.final.shorted <- HOH.Potential.Matches.unnested.final %>%
  select(HOH_ID, Name_of_the_Village, Date_of_Evaluation, HOH_First_Name, HOH_Last_Name, Respondant_First_Name, Respondant_Last_Name, Respondant_relationship, BF, BM, Child_First_Name, Child_Last_Name, Child_Date_of_Birth, Child_age, Child_Gender, Epilepsy, KBB_DD_status, Child_ID, Overall.Summary,
         EEG_Group:EEG_Exc_Rea) %>%
  arrange(HOH_ID)

# Minor data cleaning (reading data creates a time for date variables)
HOH.Potential.Matches.unnested.final.shorted$Date_of_Evaluation <- as.character(HOH.Potential.Matches.unnested.final.shorted$Date_of_Evaluation)
HOH.Potential.Matches.unnested.final.shorted$Child_Date_of_Birth <- as.character(HOH.Potential.Matches.unnested.final.shorted$Child_Date_of_Birth)

# Save the dataset
write.xlsx(list(data = HOH.Potential.Matches.unnested.final.shorted), file =  paste0(MatchedSibling_PW,"Subjects_that_need_an_ID.xlsx"))



# Creating a final dataset
Manual.Groupings <- rbind(Siblings,
                    Half_Siblings,
                    Other)

# Bind the mutually exclusive datasets back into one
Reconstructed.data.final <- rbind(Incorrect.Screeners.final,
                                  Excluded.Children.final,
                                  HOH.No.Matches.unnested.final,
                                  HOH.Potential.Matches.unnested.final)

# Give it an empty variable that represents ID
Reconstructed.data.final$ID <- NA

# Convert the DOE into character
Reconstructed.data.final$Date_of_Evaluation <- as.character(Reconstructed.data.final$Date_of_Evaluation)

# Keep the same variables in the same order as ID. Tracker
Reconstructed.data.final <- Reconstructed.data.final[, c(names(Manual.Groupings))]

# Merge everything into one Final Dataset
Final.Data <- rbind(Manual.Groupings,
                    Reconstructed.data.final)

# Set the order of the overall summary
Final.Data <- Final.Data %>%
  mutate(Overall.Summary = factor(Overall.Summary,
                                  levels = c("Incorrect Screener",
                                             "Excluded Children",
                                             "No Matches Within HOH",
                                             "Recruited With Incorrect Screener",
                                             "Control for Incorrect Screener",
                                             "Should have been Excluded",
                                             "Control for Excluded Child",
                                             "Manual: Unable to Match",
                                             "Manual: Matched Half-Siblings",
                                             "Manual: Matched Siblings")))


# Get an overview of the dataset
data.frame(Frequency = cbind(table(Final.Data$Overall.Summary)))

# Save the data
write.xlsx(list(data = Final.Data), file =  paste0(FinalData_PW, "Comprehensive Screener Scoring.xlsx"))




# Quick modification
Siblings.DOE.arranged <- read_excel(paste0(MatchedSibling_PW, "Final_ID_Tracker.xlsx"), sheet= "Siblings") %>%
  arrange(Date_of_Evaluation)

# Add  a Medical Record Row
Siblings.DOE.arranged <- Siblings.DOE.arranged %>%
  mutate(MedicalRecord = ifelse(KBB_DD_status == "Yes", "Yes","No"))

# Save the data
write.xlsx(list(data = Siblings.DOE.arranged), file =  paste0(MatchedSibling_PW, "Final_ID_Tracker_send_to_A.xlsx"))



## Quality Control
sum(duplicated(Final.Data$Child_ID))
Final.Data$Child_ID[duplicated(Final.Data$Child_ID)]



# Load in data
Binded.data <- read_excel(paste0(FinalData_PW, "All Children.xlsx"))

# Compare ID's (should equal 0)
setdiff(Final.Data$Child_ID, Binded.data$Child_ID)
setdiff(Binded.data$Child_ID, Final.Data$Child_ID)

# Same size
length(Binded.data$Child_ID) == length(Final.Data$Child_ID)

# If not the same size check for duplicates in both dataset
Binded.data[duplicated(Binded.data$Child_ID),]


# Does every HOH Siblings have a DD and no DD pair?
Siblings %>%
  group_by(HOH_ID) %>%
  transmute(n = n_distinct(KBB_DD_status)) %>%
  filter(n <2)

# Quality checker- a mismatch between DD and no DD
Siblings %>%
  group_by(HOH_ID) %>%
  count(HOH_ID) %>%
  filter(n %% 2 != 0)

# Quality checker- checks for 4 children HOH where one is DD and three are not (vice versa)
Siblings %>%
  group_by(HOH_ID) %>%
  count(KBB_DD_status) %>%
  pivot_wider(names_from = KBB_DD_status, values_from = n) %>%
  mutate(mistmach = ifelse(No == Yes, "No", "Yes")) %>%
  filter(mistmach == "Yes")


## Quality Control
sum(duplicated(Siblings$ID))
Siblings$ID[duplicated(Siblings$ID)]

# Compare ID's (should equal 0)
setdiff(1:length(Siblings$Child_ID), Siblings$ID)

# Any overall summary missing?
sum(is.na(Final.Data$Overall.Summary))

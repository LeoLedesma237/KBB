# Instructions:
# Run the first 23 lines of code
# After the data have been imported and scored, run the first 20 lines of code
# Run the remaining lines of code, manually score, then rerun the remaining again 
# https://github.com/LeoLedesma237/KBB/wiki/KBB-Matching-Children-Protocol

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


####
####### Part 1: Loading in all of the data
####

# Load in each dataset (All Recently Scored- each is mutually exclusive)
Incorrect.Screeners <- read_excel(paste0(FinalData_PW, "1) Incorrect Screeners (level 1).xlsx"))
Excluded.Children <- read_excel(paste0(FinalData_PW, "2) Excluded Children (level 1).xlsx"))
HOH.No.Matches.unnested <- read_excel(paste0(FinalData_PW, "3) HOH No Matches (level 1).xlsx"),  guess_max = 4000)
HOH.Potential.Matches.unnested <- read_excel(paste0(FinalData_PW, "4) HOH Potential Matches (level 1).xlsx"))


# Load in the IDs that have been categoriazed in Final_ID_Tracker.xlsx 
Siblings <- read_excel(paste0(MatchedSibling_PW,"Final_ID_Tracker.xlsx"), sheet= "Siblings",  guess_max = 4000)
Half_Siblings <- read_excel(paste0(MatchedSibling_PW,"Final_ID_Tracker.xlsx"), sheet= "Half-Siblings",  guess_max = 4000)
Cousins <- read_excel(paste0(MatchedSibling_PW,"Final_ID_Tracker.xlsx"), sheet= "Cousins",  guess_max = 4000)
Other <- read_excel(paste0(MatchedSibling_PW,"Final_ID_Tracker.xlsx"), sheet= "Other",  guess_max = 4000)

# Load in the ChildrenIDs that were given an actual eligibility to participate ID
# This will be used for the QS section
MatchedIDs <- read_excel(paste0(MatchedSibling_PW,"Final_ID_Tracker.xlsx"), sheet= "MatchedIDs",  guess_max = 4000)

####
####### Part 2: Create Relatedness Status for  'Subjects_that_need_an_ID.xlsx' saved in Copy_relatedness_ID_to_Final_ID_Tracker.xlsx
#### 

# Create a function that gives a relatedness_ID
relatedness_ID_fun <- function(KBB_DD_status, relatedness_status){
  
  if(relatedness_status == "Sibling") {
    ifelse(KBB_DD_status == "Yes", 1, 2)
    
    
  } else if(relatedness_status == "Half-Sibling") {
    ifelse(KBB_DD_status == "Yes", 1, 3)
    
  } else if (relatedness_status == "Cousin") {
    ifelse(KBB_DD_status == "Yes", 1, 4)
    
  }
}

# Run this function on the Sibling, Half-Sibling, and Cousin data 

Sibling_relatedness_ID <- mapply(relatedness_ID_fun, 
                                 KBB_DD_status = Siblings$KBB_DD_status, 
                                 relatedness_status = "Sibling")

Half_Sibling_relatedness_ID <- mapply(relatedness_ID_fun, 
                                      KBB_DD_status = Half_Siblings$KBB_DD_status, 
                                      relatedness_status = "Half-Sibling")

Cousin_relatedness_ID <- mapply(relatedness_ID_fun, 
                                KBB_DD_status = Cousins$KBB_DD_status, 
                                relatedness_status = "Cousin")

# Save this information into an excel file
write.xlsx(list(Sibling = Sibling_relatedness_ID,
                Half_Sibling = Half_Sibling_relatedness_ID,
                Cousin = Cousin_relatedness_ID),
           file =  paste0(MatchedSibling_PW,"Copy_relatedness_ID_to_Final_ID_Tracker.xlsx"))


####
####### Part 3: Identify the Households whose children might be matches
#### We need to partial out all children that have already been given an ID or categorized as having no match


# Combine the children IDs that have already been categorized in the Final_ID_Tracker.xlsx
accounted.for.IDs <- c(Siblings$Child_ID, Half_Siblings$Child_ID, Cousins$Child_ID, Other$Child_ID)

# Remove rows from these datasets if the Child_ID is present in the Final_ID_Dataset
# We are mostly interested here in HOH.Potential.Matches.unnested; the remaining IDs may be the ones who will be matched
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
  select(HOH_ID, Name_of_the_Village, Date_of_Evaluation, HOH_First_Name, HOH_Last_Name, Respondant_First_Name, Respondant_Last_Name, Respondant_relationship, BF, BM, dad_sib_group, mom_sib_group, Child_First_Name, Child_Last_Name, Child_Date_of_Birth, Child_age, Child_Gender, Epilepsy, KBB_DD_status, Child_ID, Overall.Summary,
         EEG_Group:EEG_Exc_Rea) %>%
  arrange(HOH_ID)

# Minor data cleaning (reading data creates a time for date variables)
HOH.Potential.Matches.unnested.final.shorted$Date_of_Evaluation <- as.character(HOH.Potential.Matches.unnested.final.shorted$Date_of_Evaluation)
HOH.Potential.Matches.unnested.final.shorted$Child_Date_of_Birth <- as.character(HOH.Potential.Matches.unnested.final.shorted$Child_Date_of_Birth)

# Load the relatedness function
source("Scoring/scoring_functions/related_fun.R")
HOH.Potential.Matches.unnested.final.shorted$relatedness <- related_fun(HOH.Potential.Matches.unnested.final.shorted)
HOH.Potential.Matches.unnested.final.shorted <- select(HOH.Potential.Matches.unnested.final.shorted, HOH_ID:Child_Last_Name,relatedness, everything())

# Save the dataset
write.xlsx(list(data = HOH.Potential.Matches.unnested.final.shorted), file =  paste0(MatchedSibling_PW,"Subjects_that_need_an_ID.xlsx"))




####
###### Part 5: Save all data into one mega dataset (everything is remerged)- this will be used for QC (duplicates)
####



# Creating a final dataset
Manual.Groupings <- rbind(Siblings,
                    Half_Siblings,
                    Cousins,
                    Other)

# Bind the mutually exclusive/partialled outIDs into one
Reconstructed.data.final <- rbind(Incorrect.Screeners.final,
                                  Excluded.Children.final,
                                  HOH.No.Matches.unnested.final,
                                  HOH.Potential.Matches.unnested.final)


# Give it an empty variable that represents ID
Reconstructed.data.final$ID <- NA
Reconstructed.data.final$Relatedness <- NA
Reconstructed.data.final$Relatedness_ID <- NA
Reconstructed.data.final$Date_Manually_Matched <- NA

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
                                             "Manual: Matched Half-Siblings (Old)",
                                             "Manual: Matched Siblings",
                                             "Manual: Matched Cousins")))


# Get an overview of the dataset
data.frame(Frequency = cbind(table(Final.Data$Overall.Summary)))

# Save the data
write.xlsx(list(data = Final.Data), file =  paste0(FinalData_PW, "Comprehensive Screener Scoring.xlsx"))

####
###### Part 6: Create a version to send to Ackim (Arranged by DOE)
####


# Quick modification
Siblings.DOE.arranged <- read_excel(paste0(MatchedSibling_PW, "Final_ID_Tracker.xlsx"), sheet= "MatchedIDs",
                                    guess_max = 4000) %>%
  arrange(Date_of_Evaluation)

# Add  a Medical Record Row
Siblings.DOE.arranged <- Siblings.DOE.arranged %>%
  mutate(MedicalRecord = ifelse(KBB_DD_status == "Yes", "Yes","No"))

# Save the data
write.xlsx(list(data = Siblings.DOE.arranged), file =  paste0(MatchedSibling_PW, "Final_ID_Tracker_send_to_A.xlsx"))


####
###### Part 7: Several Quality Control Measures
####

# Load in All children (before scoring)
Binded.data <- read_excel(paste0(FinalData_PW, "All Children.xlsx"))

# Compare ID's (both should equal 0-if not it means child not categorized correctly)
setdiff(Final.Data$Child_ID, Binded.data$Child_ID)
setdiff(Binded.data$Child_ID, Final.Data$Child_ID)

# Same size (must be TRUE)
length(Binded.data$Child_ID) == length(Final.Data$Child_ID)

# Check for duplicate Child_ID
(duplicated_child_IDs <- Binded.data$Child_ID[duplicated(Binded.data$Child_ID)])
Binded.data %>% filter(Child_ID %in% duplicated_child_IDs) 


# Check for duplicates in the Final_ID_Tracker.xlsx (must be 0)
sum(duplicated(MatchedIDs$Child_ID))
sum(duplicated(MatchedIDs$ID))

# Making sure no ID number was skipped when given them
setdiff(1:length(MatchedIDs$Child_ID), MatchedIDs$ID)

# Does every HOH Siblings have a DD and no DD pair?
MatchedIDs %>%
  group_by(HOH_ID) %>%
  transmute(n = n_distinct(KBB_DD_status)) %>%
  filter(n <2)

# Quality checker- a mismatch between DD and no DD
MatchedIDs %>%
  group_by(HOH_ID) %>%
  count(HOH_ID) %>%
  filter(n %% 2 != 0)

# Quality checker- checks for 4 children HOH where one is DD and three are not (vice versa)
MatchedIDs %>%
  group_by(HOH_ID) %>%
  count(KBB_DD_status) %>%
  pivot_wider(names_from = KBB_DD_status, values_from = n) %>%
  mutate(mistmach = ifelse(No == Yes, "No", "Yes")) %>%
  filter(mistmach == "Yes")


# Any overall summary missing?
sum(is.na(Final.Data$Overall.Summary))

# Are there any Children_ID in the Final_ID_Tracker.xlsx tabs and NOT in the MatchedID tab (Must be 0)
# Remove the (old) half sibling matches from the Half-Sibling data
Half_Siblings <- Half_Siblings %>%
  filter(Overall.Summary != "Manual: Matched Half-Siblings (Old)")

setdiff(Siblings$Child_ID, MatchedIDs$Child_ID)
setdiff(Half_Siblings$Child_ID, MatchedIDs$Child_ID)
setdiff(Cousins$Child_ID, MatchedIDs$Child_ID)

# Check if someone given an ID was too young
cbind(MatchedIDs$Date_of_Evaluation , MatchedIDs$Child_ID, MatchedIDs$ID, MatchedIDs$Child_age, "Given ID but they are less than 3!")[MatchedIDs$Child_age < 3,]

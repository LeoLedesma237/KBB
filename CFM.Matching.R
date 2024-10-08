# Run Previous Scripts
setwd("~/GitHub/LeoWebsite/KBB.Scripts")

source("CFM2_4.Scoring.R")
source("CFM5_17.Scoring.R")
source("CFM.Before.Matching.R")

# Remove all global environment objects to declutter
rm(list=ls())


# Set the working directory
setwd("~/KBB_new_2/1_screener/final_data")

# Load in each dataset
Incorrect.Screeners <- read_excel("1) Incorrect Screeners (level 1).xlsx")
Excluded.Children <- read_excel("2) Excluded Children (level 1).xlsx")
HOH.No.Matches.unnested <- read_excel("3) HOH No Matches (level 1).xlsx")
HOH.Potential.Matches.unnested <- read_excel("4) HOH Potential Matches (level 1).xlsx")



# Set the working directory
setwd("~/KBB_new_2/matched_siblings")

# Load in the ID Tracker (each tab) 
Siblings <- read_excel("Final ID Tracker.xlsx", sheet= "Siblings")
Half_Siblings <- read_excel("Final ID Tracker.xlsx", sheet= "Half-Siblings")
Other <- read_excel("Final ID Tracker.xlsx", sheet= "Other")

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



# Set working directory
setwd("~/KBB_new_2/matched_siblings")

# Save the partialled out HOH Potential Matches
HOH.Potential.Matches.unnested.final.shorted <- HOH.Potential.Matches.unnested.final %>%
  select(HOH_ID, Name_of_the_Village, Date_of_Evaluation, HOH_First_Name, HOH_Last_Name, Respondant_First_Name, Respondant_Last_Name, Respondant_relationship, BF, BM, Child_First_Name, Child_Last_Name, Child_Date_of_Birth, Child_age, Child_Gender, Epilepsy, KBB_DD_status, Child_ID, Overall.Summary) %>%
  arrange(HOH_ID)

# Save the dataset
write.xlsx(list(data = HOH.Potential.Matches.unnested.final.shorted), file =  "Subjects that need an ID.xlsx")




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

# Set the working directory
setwd("~/KBB_new_2")

# Save the data
write.xlsx(list(data = Final.Data), file =  "Comprehensive Screener Scoring.xlsx")




# Quick modification
setwd("~/KBB_new_2/matched_siblings")
Siblings.DOE.arranged <- read_excel("Final ID Tracker.xlsx", sheet= "Siblings") %>%
  arrange(Date_of_Evaluation)

# Save the data
write.xlsx(list(data = Siblings.DOE.arranged), file =  "Final ID Tracker send to A.xlsx")






## Quality Control
sum(duplicated(Final.Data$Child_ID))
Final.Data$Child_ID[duplicated(Final.Data$Child_ID)]


# Set the save directory
setwd("~/KBB_new_2/1_screener/final_data")

# Load in data
Binded.data <- read_excel("All Children.xlsx")

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

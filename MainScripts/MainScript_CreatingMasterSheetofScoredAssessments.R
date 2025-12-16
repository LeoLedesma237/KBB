# Creating a universal spreadsheet of scores from all measures (like a master sheet)
# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)

# Load in all scored data (Children)
BRSR <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Children/BRIEF2_SF.xlsx"))
LD <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Children/LettrDig.xlsx"))
PR <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Children/PatternReas.xlsx"))
PD <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Children/PhysicalData.xlsx"))
RV <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Children/RecepVocab.xlsx"))
TR <-  read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Children/Triangles.xlsx"))

# Load in all scored data (Adults)
BRPF <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Adults/BRIEF2_Parent.xlsx"))
CBC3_6  <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Adults/CBC_3_6.xlsx"))
CBC6_18 <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Adults/CBC_6_18.xlsx"))
PSC <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Adults/PSC.xlsx"))

# Load Epilepsy Data
Demo <- read_excel(paste0(DataLocation,"FINAL_DS/Screener/Matched_Siblings/Final_ID_Tracker.xlsx"))

# Set a save pathway for the full data
save.pathway <- paste0(DataLocation,"FINAL_DS/Behavioral/full_data.xlsx")

# Set age group parameters


###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############




# As of right now- we are only merging those for a behavioral report
PD2 <- select(PD, Child_ID, Left.eye:Both.Eyes, `1000Hz_Left_dB`:`4000Hz_Right_dB`)
Demo2 <- select(Demo, Child_ID = ID, Sex = Child_Gender, Age = Child_age,Epilepsy)
PSC2 <- select(PSC, Child_ID, PSC_StopRule_Num:PSC_Performance) # Fixed
CBC3_6_2 <- select(CBC3_6, Child_ID, CBC3_6_emotionally_reactive:CBC3_6_opositional_defiant_problems, CBC3_6_NA_Num)
CBC6_18_2 <- select(CBC6_18, Child_ID, CBC6_18_anxious_depressed:CBC6_18_Total_Prob, CBC6_18_NA_Num)
RV2 <- select(RV, Child_ID, RV_StopRule_Num:RV_Performance) # Fixed
PR2 <- select(PR, Child_ID, PR_StopRule_Num:PR_Performance) # Fixed
TR2 <- select(TR, Child_ID, TR_StopRule_Num:TR_Performance) 
LD2 <- select(LD, Child_ID, LetDig_CDK:LetDig_Performance)

# Join them all into one dataset
full_data <- PD2 %>%
  full_join(Demo2, by = "Child_ID") %>%
  full_join(PSC2, by = "Child_ID") %>%
  full_join(CBC3_6_2, by = "Child_ID") %>%
  full_join(CBC6_18_2, by = "Child_ID") %>%
  full_join(RV2, by = "Child_ID") %>%
  full_join(PR2, by = "Child_ID") %>%
  full_join(TR2, by = "Child_ID") %>%
  full_join(LD2, by = "Child_ID") %>%
  unique()

# Extract/Drop any duplicates
duplicate_IDs <- full_data$Child_ID[duplicated(full_data$Child_ID)]
dup_extract <- filter(full_data, Child_ID %in% duplicate_IDs)
full_data <- filter(full_data, !Child_ID %in% duplicate_IDs)

# Save the full data
write.xlsx(x= full_data, file = save.pathway)
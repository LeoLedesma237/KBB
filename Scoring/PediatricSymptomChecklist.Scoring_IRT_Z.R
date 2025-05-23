# This is the script for scoring Pediatric Symptom Checklist (PSC) Using IRT and Z-scores for custom groups
# The data loaded will be processed data from the scored/cleaned PSC data in the FINAL_DATA folder

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)

# Read in the cleaned PSC data
PSC <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Adults/PSC.xlsx"))

# Note: no demographics required since no age groups will be created

# Create a save pathway
save.pathway_PSC <- paste(DataLocation,"FINAL_DS/Behavioral/Adults/PSC_IRT_Z.xlsx", sep="")


###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############

# Extract the items associated with certain scales:



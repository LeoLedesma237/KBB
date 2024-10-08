# This is the script for Triangles and Letter & Digit Span
# We are starting from the Root Directory /KBB
Root.Folder <- "C:/Users/lledesma.TIMES/Documents/KBB"
setwd(Root.Folder)

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr) # str_count()

# Read in the file
Triangles_LettrDig <- read_excel("2_behavioral_assessments/Children/Raw/Triangles_LettrDig_Raw.xlsx")

# Check the IDs for errors
source("2_behavioral_assessments/id_error_function.R")
TR_Notes <-check_id_errors("Triangles and LettrDig",
                           Triangles_LettrDig$Child_ID)

# Select Vars of Interest
Front <- Triangles_LettrDig %>%
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation)

TR.Items <- Triangles_LettrDig %>%
  select(paste("_",1:8,"_001",sep=""),
         paste("_",9:27,sep=""))


NF.Items <- Triangles_LettrDig %>%
  select(`_1_Trial_1_4`:`_8a`)


NB.Items <- Triangles_LettrDig %>%
  select(`_1_Trial_4_1`:`_8a_bw_001`)


LF.Items <- Triangles_LettrDig %>%
  select(`_1_Trial_f_c`:`_8a_lf`)


LB.Items <- Triangles_LettrDig %>%
  select(`_1_Trial_d_g`:`_8a_bw`)

# Renaming the Items (Part 1)
names(TR.Items) <- 1:27
names(NF.Items) <- 1:16  
names(NB.Items) <- 1:16 
names(LF.Items) <- 1:16 
names(LB.Items) <- 1:16

new.names <- c("TR_", "NF_", "NB_", "LF_", "LB_")

# Create a list for all of the Items
Items.list <- list(TR.Items, NF.Items, NB.Items, LF.Items, LB.Items)

# Run a for loop for all Items in the list
for(ii in 1:length(Items.list)) {
  
  # Extract current Items (these are df)
  Items <- Items.list[[ii]]
  
  # Count the number of 777 (CDK), 888 (NRG), NAs, items given, and total number of items
  Items$CDK <- rowSums(mutate(Items,across(everything(), ~ str_count(., "777"))), na.rm = T)
  Items$NRG <- rowSums(mutate(Items,across(everything(), ~ str_count(., "888"))), na.rm = T)
  Items$NA.Num <- rowSums(is.na(Items))
  Items$Items.Given <- rowSums(!(is.na(Items)))
  Items$Total.Items <- length(Items) - 4
  
  # Change all rows so if not 1 or 2 they will turn into 0s
  Items.Cleaned <- data.frame(apply(Items, 1, function(row) ifelse(row %in% c(1, 2), row, 0)))
  
  # Convert the outputs to numeric from character
  Items.Cleaned.num <- data.frame(do.call(rbind, lapply(Items.Cleaned, function(x) as.numeric(x))))
  
  # Take the row sums of Items Cleaned to calculate performance
  Items$Performance <- rowSums(Items.Cleaned.num)
  
  # Rename the variables
  names(Items) <- paste(new.names[ii], names(Items),sep="")
  
  # Update this back into the list
  Items.list[[ii]] <- Items
}


# Create the final datasets
Triangles <- data.frame(cbind(Front), Items.list[[1]])

LetterDig <- data.frame(cbind(Front), Items.list[[2]], 
                        Items.list[[3]], Items.list[[4]], 
                        Items.list[[5]])

# Have a composite scores for all Letter and Digit subtest
LetterDig <- LetterDig %>%
  mutate(LetDig_CDK = NF_CDK + NB_CDK + LF_CDK + NB_CDK,
         LetDig_NRG = NF_NRG + NB_NRG + LF_NRG + NB_NRG,
         LetDig_NA.Num = NF_NA.Num + NB_NA.Num + LF_NA.Num + NB_NA.Num,
         LetDig_Items.Given = NF_Items.Given + NB_Items.Given + LF_Items.Given + NB_Items.Given,
         LetDig_Total.Items = NF_Total.Items + NB_Total.Items + LF_Total.Items + NB_Total.Items,
         LetDig_Performance = NF_Performance + NB_Performance + LF_Performance + NB_Performance)

# Create a save pathway
save.pathway_TR <- paste(Root.Folder,"/",
                         "2_behavioral_assessments/Children/Processed", "/",
                          "Triangles.xlsx", sep="")

save.pathway_LD <- paste(Root.Folder,"/",
                         "2_behavioral_assessments/Children/Processed", "/",
                         "LettrDig.xlsx", sep="")

# Save the scored data
write.xlsx(x= Triangles, file = save.pathway_TR)
write.xlsx(x= LetterDig, file = save.pathway_LD)


# Create a save pathway for Notes
save.pathway.notes <- paste(Root.Folder,"/",
                            "2_behavioral_assessments/Children/Processed_Notes", "/",
                            "Triangles_LettrDig_Notes.csv", sep="")

# Save the Notes as a CSV
write_csv(x = TR_Notes, save.pathway.notes)


# Make a note that the data was saved successfully
cat("Saving processed Triangles\n")
cat("Saving processed Letter and Digit Span\n")

# Remove all global environment objects to declutter
rm(list=ls())

# Set the working directory to the scoring scripts
setwd("~/GitHub/LeoWebsite/KBB.Scripts/Scoring")

# Set a pause time for 1 second
Sys.sleep(1)
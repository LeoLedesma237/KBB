# This is the script for Triangles and Letter & Digit Span

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr) # str_count()

# Read in the file
Triangles_LettrDig <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Children/Triangles_LettrDig_Raw.xlsx"))

# Create a save pathway for cleaned and scored Triangles data
save.pathway_TR <- paste(DataLocation,"FINAL_DS/Behavioral/Children/Triangles.xlsx", sep="")

# Create a save pathway for cleaned and scored Letter and Digit Span data
save.pathway_LD <- paste(DataLocation,"FINAL_DS/Behavioral/Children/LettrDig.xlsx", sep="")

# Create a save pathway for the outcome of the ID reports
save.pathway.notes <- paste(DataLocation,"REPORTS/Individual/Triangles_LettrDig_Notes.csv", sep="")


###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############


# Check the IDs for errors
source("Scoring/scoring_functions/IDError_FUNCTION.R")
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

# Create a list for all of the Items and their respective stop rules and names
Items.List <- list(TR.Items, NF.Items, NB.Items, LF.Items, LB.Items)
StopRules <- c(5,4,4,4,4)
new.names <- c("TR", "NF", "NB", "LF", "LB")


# Call the function to score Lettter and Digit
source("Scoring/scoring_functions/Scoring_FUNCTION1.R")

# Create a list to save the scored data
scoredItems_list <- list()

for(ii in 1:length(Items.List)) {
 
  # Save the current Item list
  current_ItemList <- Items.List[[ii]]
  
  # Rename items before they are scored 
  names(current_ItemList) <- paste0(new.names[ii],"_", names(current_ItemList))
  
  # Score the Letter and Digit Items
  scoredItems<- scoring_function1(current_ItemList, StopRules[ii], new.names[ii])
  
  # Save these scored and cleaned datasets
  scoredItems_list[[ii]] <- scoredItems
}


# Create the final datasets
Triangles <- data.frame(cbind(Front), scoredItems_list[[1]])

LetterDig <- data.frame(cbind(Front), scoredItems_list[[2]], 
                        scoredItems_list[[3]], scoredItems_list[[4]], 
                        scoredItems_list[[5]])

# Have a composite scores for all Letter and Digit subtest
LetterDig <- LetterDig %>%
  mutate(LetDig_CDK = NF_CDK + NB_CDK + LF_CDK + NB_CDK,
         LetDig_NRG = NF_NRG + NB_NRG + LF_NRG + NB_NRG,
         LetDig_NA.Num = NF_NA_num + NB_NA_num + LF_NA_num + NB_NA_num,
         LetDig_Performance = NF_Performance + NB_Performance + LF_Performance + NB_Performance)

# Save the scored data
write.xlsx(x= Triangles, file = save.pathway_TR)
write.xlsx(x= LetterDig, file = save.pathway_LD)

# Save the Notes as a CSV
write_csv(x = TR_Notes, save.pathway.notes)


# Make a note that the data was saved successfully
cat("Saving processed Triangles\n")
cat("Saving processed Letter and Digit Span\n")

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation"))])


# Set a pause time for 1 second
Sys.sleep(1)
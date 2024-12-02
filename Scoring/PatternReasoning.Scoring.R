# This is the script for scoring Pattern Reasoning
# We are starting from the Root Directory /KBB

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr) 

# Read in the file
PatternReasoning <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Children/PatternReas_Raw.xlsx"))

# Create a save pathway
save.pathway <- paste(DataLocation,"FINAL_DS/Behavioral/Children/PatternReas.xlsx", sep="")

# Create a save pathway for Notes
save.pathway.notes <- paste(DataLocation,"REPORTS/Individual/PatternReas_Notes.csv", sep="")



###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############


# Check the IDs for errors
source("Scoring/scoring_functions/IDError_FUNCTION.R")
PR_Notes <-check_id_errors("Pattern Reasoning",
                           PatternReasoning$Child_ID)

# Select Vars of Interest
Front <- PatternReasoning %>%
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation)

# Only scored items
Items <- PatternReasoning %>%
  select(paste("Iterm_",1:11,sep=""),
         "Item_12",
         paste("Iterm_",13:36,sep=""))

# Scored items and their times
All_Items <- PatternReasoning %>%
  select(Iterm_1:`_38a_Time`)
  

# Rename Items
names(Items) <- paste0("PR_",1:length(Items))

# Rename All Items
All_Items_Var1 <- sapply(str_split(names(All_Items),"_"), function(x) x[2])
names(All_Items) <- sapply(All_Items_Var1, function(x) 
  if(grepl("a",x)) {
    x <- gsub("a","",x)
    return(paste0("PR_",x,"_sec"))
  
    } else {
      return(paste0("PR_",x))
    
  }
)


# Call the function to score Pattern Reasoning
source("Scoring/scoring_functions/Scoring_FUNCTION1.R")

# Score the Pattern Reasoning Items
scoredItems <- scoring_function1(Items, 4)

# Introduce them into the dataset
PatternReasoning <- cbind(Front, All_Items, select(scoredItems,StopRule_Num:Performance))


# Save the scored data
write.xlsx(x= PatternReasoning, file = save.pathway)


# Save the Notes as a CSV
write_csv(x = PR_Notes, save.pathway.notes)


# Make a note that the data was saved successfully
cat("Saving processed Pattern Reasoning\n")

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation"))])


# Set a pause time for 1 second
Sys.sleep(1)
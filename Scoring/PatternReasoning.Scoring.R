# This is the script for scoring Pattern Reasoning
# We are starting from the Root Directory /KBB

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr) 

# Read in the file
PatternReasoning <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Children/PatternReas_Raw.xlsx"))


# Check the IDs for errors
source("Scoring/IDError_FUNCTION.R")
PR_Notes <-check_id_errors("Pattern Reasoning",
                           PatternReasoning$Child_ID)

# Select Vars of Interest
Front <- PatternReasoning %>%
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation)

Items <- PatternReasoning %>%
  select(paste("Iterm_",1:11,sep=""),
         "Item_12",
         paste("Iterm_",13:36,sep=""))

# Rename the Items (Part 1)

# Cbind these selected Vars to make a new dataset
PatternReasoning2 <- cbind(Front, Items)

# Count the number of 777 (CDK), 888 (NRG), NAs, items given, and total number of items
PatternReasoning2$CDK <- rowSums(mutate(Items,across(everything(), ~ str_count(., "777"))), na.rm = T)
PatternReasoning2$NRG <- rowSums(mutate(Items,across(everything(), ~ str_count(., "888"))), na.rm = T)
PatternReasoning2$NA.Num <- rowSums(is.na(Items))
PatternReasoning2$Items.Given <- rowSums(!(is.na(Items)))
PatternReasoning2$Total.Items <- length(Items)

# Change all rows so if not 1 or 2 they will turn into 0s
Items.Cleaned <- data.frame(apply(Items, 1, function(row) ifelse(row %in% c(1, 2), row, 0)))

# Convert the outputs to numeric from character
Items.Cleaned.num <- data.frame(do.call(rbind, lapply(Items.Cleaned, function(x) as.numeric(x))))

# Take the row sums of Items Cleaned to calculate performance
PatternReasoning2$Performance <- rowSums(Items.Cleaned.num)

# Change The names of the variables so we know it is pattern reasoning
names(PatternReasoning2) <- c(names(PatternReasoning2)[1:3], paste("PR_",names(PatternReasoning2)[4:45], sep=""))

# Create a save pathway
save.pathway <- paste(DataLocation,"FINAL_DS/Behavioral/Children/",
                     "PatternReas_Processed.xlsx", sep="")

# Save the scored data
write.xlsx(x= PatternReasoning2, file = save.pathway)

# Create a save pathway for Notes
save.pathway.notes <- paste(DataLocation,"REPORTS/Individual/",
                            "PatternReas_Notes.csv", sep="")

# Save the Notes as a CSV
write_csv(x = PR_Notes, save.pathway.notes)


# Make a note that the data was saved successfully
cat("Saving processed Pattern Reasoning\n")

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation"))])


# Set a pause time for 1 second
Sys.sleep(1)
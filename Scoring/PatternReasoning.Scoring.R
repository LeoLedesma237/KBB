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


# Create a function to score the bonus points
time_scoring_fun <- function(item, time, threshold1) {
  # return NA if either is missing
  na_idx <- is.na(item) | is.na(time)
  
  # scoring ifelse statement
  score <- ifelse(item != 0, 
                  ifelse(time <= threshold1, 2, item), item)
  score[na_idx] <- NA
  
  # if either field contains 777 or 888, return that code instead
  special_idx <- (item %in% c(777,888)) | (time %in% c(777,888))
  score[special_idx] <- ifelse(
    item[special_idx] %in% c(777,888),
    item[special_idx],
    time[special_idx]
  )
  
  score
}


##
##### Part 1: Scoring items 10-36 in Pattern Reasoning (one time threshold)
##
# If an item was correctly answered less than the time threshold then it gets a bonus point
# Set parameters for function1 scoring
items1 <- select(PatternReasoning, c(paste0("Iterm_",10:11),
                                     "Item_12",
                                     paste0("Iterm_",13:36)))

times1 <- select(PatternReasoning, c(paste0("_",10:15,"a_Time"),
                                     "_16a_TIme",
                                     paste0("_",17:20,"a_Time"),
                                     "_21a_Time_in_seconds",
                                     "_22a_Time",
                                     "_23a_Time_in_seconds",
                                     paste0("_",24:28,"a_Time"),
                                     "_29a_time",
                                     "_30a_Time",
                                     "_31_Time",
                                     paste0("_",32:35,"a_Time"),
                                     "_38a_Time"
                                     ))
threshold1 <- c(rep(10,4), rep(15,10), rep(20,2), rep(25,2), rep(20,3), 25, 30, rep(45,4))

# Score each item based on whether they met the bonus threshold
scores_10_36 <- mapply(
  FUN = time_scoring_fun,
  item = items1,
  time = times1,
  threshold1 = threshold1
)

# minor data cleaning
scores_10_36 <- sapply(data.frame(scores_10_36), function(x) as.numeric(x)) %>% data.frame()
names(scores_10_36) <- names(items1)

##
##### Part 2: Reconstructing Pattern Reasoning dataset to have updated scores
##


# Overwrite the original item scores with these updated ones
PatternReasoning[ , names(scores_10_36)] <- scores_10_36[ , names(scores_10_36)]


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

# Change the name of item question
names(All_Items)[names(All_Items) == "_31_Time"] <- "_31a_Time"

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
scoredItems <- scoring_function1(Items, 6, "PR")

# Introduce them into the dataset
PatternReasoning <- cbind(Front, All_Items, select(scoredItems,PR_StopRule_Num:PR_Performance))


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
# This is the script for scoring BRIEF2 Self Report

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)

# Read in the file
BRIEF2SR <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Children/BRIEF2_SF_Raw.xlsx"))

# Check the IDs for errors
source("Scoring/scoring_functions/IDError_FUNCTION.R")
BRSR_Notes <-check_id_errors("BRIEF2 Self Report",
                           BRIEF2SR$Child_ID)



# Select Vars of Interest
Front <- BRIEF2SR %>%
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation)

Items <- BRIEF2SR %>%
  select(`_1`:`_55`)

# Rename Items (Part 1)
names(Items) <- 1:length(Items)

# Change the Character Values to Numeric
Items2 <- Items %>%
  mutate_all(~ case_when(
    . == "N" ~ 1,
    . == "S" ~ 2,
    . == "O" ~ 3,
    TRUE ~ NA  
  ))

# Cbind them
BRIEF2SR2 <- tibble(cbind(Front,Items2))

# Count the number of NAs, items given, and total number of items
BRIEF2SR2$NA.Num <- rowSums(is.na(Items2))
BRIEF2SR2$Items.Given <- rowSums(!(is.na(Items2)))
BRIEF2SR2$Total.Items <- length(Items2)

# Change The names of the variables so we know it is pattern reasoning
names(BRIEF2SR2) <- c(names(BRIEF2SR2)[1:3], paste("BRSR_",names(BRIEF2SR2)[4:61], sep=""))


# Load in the subtest information
BRIEF2_subtests <- read_excel(paste0(DataLocation,"RAW_DATA/AnswerKeys/BRIEF2SelfReport.xlsx"))

# Get the subtest as a variable
subtest <- BRIEF2_subtests$Subtest

# Rename the variables to the subtest names (makes indexing easy later)
names(Items2) <- subtest

# Obtain the rowSums of all variables that measure the same construct
allConstructs <- sort(unique(subtest))

# Create a list that will contain the RowSums of these constructs
RowSums_list <- list()

for(ii in 1:length(allConstructs)) {
  
  # Extract the column names for each construct 
   currentConstruct <- Items2[,grepl(pattern = allConstructs[ii], subtest)]
  
  # Calculate the Row means
   ConstructScored <- data.frame(rowSums(currentConstruct))
  
  # Rename the variable
  names(ConstructScored) <- allConstructs[ii]
  
  # Save it into the list
  RowSums_list[[ii]] <- ConstructScored 
}

# Cbind all of these scored constructs 
AllConstructsScored <- do.call(cbind, RowSums_list)

# Remove the F variable
AllConstructsScored <- select(AllConstructsScored, -`F`)

# Introduce a couple more variables 
AllConstructsScored <- AllConstructsScored %>%
  mutate(BRI = Inhibit + Self_Monitor,
         ERI = Emotional_Control + Shift,
         CRI = Task_Completion + Working_Memory + Plan_Organize,
         GEC = BRI + ERI + CRI)


# Now introduce all of these scored measures into the final dataset
BRIEF2SR3 <- cbind(BRIEF2SR2, AllConstructsScored)


# Now let's score the validity portion
BREIF2_validity <- read_excel(paste0(DataLocation,"RAW_DATA/AnswerKeys/BRIEF2SelfReport.xlsx"), sheet = "Validity")

# Save the validity questions as an object
Validity <- paste0("Q",BREIF2_validity$Question,"_",BREIF2_validity$Validity)

# Rename the variables for validity scoring
names(Items2) <- Validity

# Extracting all items for each validity measure
Negativity <- Items2[,grepl(pattern = "Negativity", Validity)]
Infrequency <- Items2[,grepl(pattern = "Infrequency", Validity)]
Inconsistency1 <- Items2[,grepl(pattern = "Inconsistency1", Validity)]
Inconsistency2 <- Items2[,grepl(pattern = "Inconsistency2", Validity)]

# Scoring Negativity
BRIEF2SR3$NegativityScore <- rowSums(Negativity == 3)
BRIEF2SR3 <- mutate(BRIEF2SR3, Negativity = case_when(
                               NegativityScore <=3 ~ "Acceptable",
                               NegativityScore == 4 ~ "Elevated",
                               NegativityScore >= 5 ~ "Highly_Elevated"
))

# Scoring Infrequency
BRIEF2SR3$InfrequencyScore <- rowSums(Infrequency >= 2)
BRIEF2SR3 <- mutate(BRIEF2SR3, Infrequency = case_when(
                               InfrequencyScore == 0 ~ "Acceptable",
                               InfrequencyScore >= 1 ~ "Questionable",
))

# Scoring Inconsistency (Tedious)
# Change the order of inconsistency 2 so the matrices can match
Inconsistency2 <- select(Inconsistency2, Q12_Inconsistency2:Q27_Inconsistency2,
                         Q52_Inconsistency2,Q41_Inconsistency2, Q42_Inconsistency2,
                         Q55_Inconsistency2, Q53_Inconsistency2)

BRIEF2SR3$InconsistencyScore <- abs(rowSums(Inconsistency1 - Inconsistency2))
BRIEF2SR3 <- mutate(BRIEF2SR3, Inconsistency = case_when(
  InconsistencyScore <= 5 ~ "Acceptable",
  InconsistencyScore == 6 | InconsistencyScore == 7 ~ "Questionable",
  InconsistencyScore >= 8 ~ "Inconsistent"
))



# Create a save pathway
save.pathway_BRSR <- paste(DataLocation,"FINAL_DS/Behavioral/Children/",
                         "BRIEF2_SF.xlsx", sep="")

# Save the scored data
write.xlsx(x= BRIEF2SR3, file = save.pathway_BRSR)

# Create a save pathway for Notes
save.pathway.notes <- paste(DataLocation,"REPORTS/Individual/",
                            "BRIEF2_SF.csv", sep="")

# Save the Notes as a CSV
write_csv(x = BRSR_Notes, save.pathway.notes)


# Make a note that the data was saved successfully
cat("Saving processed BRIEF2 Self Report\n")

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation"))])

# Set a pause time for 1 second
Sys.sleep(1)


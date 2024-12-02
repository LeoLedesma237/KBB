# This is the script for scoring BRIEF2 Parent Form

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)

# Read in the file
BRIEF2PF <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Adults/BRIEF2_Parent_Raw.xlsx"))

# Load in the subtest information
BRIEF2_subtests <- read_excel(paste0(DataLocation,"RAW_DATA/AnswerKeys/BRIEF2ParentForm.xlsx"))

# Now let's score the validity portion
BREIF2_validity <- read_excel(paste0(DataLocation,"RAW_DATA/AnswerKeys/BRIEF2ParentForm.xlsx"), sheet = "Validity")

# Create a save pathway
save.pathway_BRPF <- paste(DataLocation,"FINAL_DS/Behavioral/Adults/BRIEF2_Parent.xlsx", sep="")

# Create a save pathway for Notes
save.pathway.notes <- paste(DataLocation,"REPORTS/Individual/BRIEF2_Parent.csv", sep="")



###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############



# Check the IDs for errors
source("Scoring/scoring_functions/IDError_FUNCTION.R")
BRPF_Notes <-check_id_errors("BRIEF2 Parent Form",
                             BRIEF2PF$Child_ID)


# Select Vars of Interest
Front <- BRIEF2PF %>%
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation)

Items <- BRIEF2PF %>%
  select(`_1`:`_63`)

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
BRIEF2PF2 <- tibble(cbind(Front,Items2))

# Count the number of NAs, items given, and total number of items
BRIEF2PF2$NA.Num <- rowSums(is.na(Items2))

# Change The names of the variables so we know it is pattern reasoning
names(BRIEF2PF2) <- c(names(BRIEF2PF2)[1:3], paste("BRPF_",names(BRIEF2PF2)[4:69], sep=""))


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
         ERI = Shift + Emotional_Control ,
         CRI = Initiate + Working_Memory + Plan_Organize + Task_Monitor + Organization_of_Materials,
         GEC = BRI + ERI + CRI)


# Now introduce all of these scored measures into the final dataset
BRIEF2PF3 <- cbind(BRIEF2PF2, AllConstructsScored)



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
BRIEF2PF3$NegativityScore <- rowSums(Negativity == 3)
BRIEF2PF3 <- mutate(BRIEF2PF3, Negativity = case_when(
  NegativityScore <=3 ~ "Acceptable",
  NegativityScore == 4 ~ "Elevated",
  NegativityScore >= 5 ~ "Highly_Elevated"
))

# Scoring Infrequency
BRIEF2PF3$InfrequencyScore <- rowSums(Infrequency >= 2)
BRIEF2PF3 <- mutate(BRIEF2PF3, Infrequency = case_when(
  InfrequencyScore == 0 ~ "Acceptable",
  InfrequencyScore >= 1 ~ "Questionable",
))

# Scoring Inconsistency (Tedious)
# Change the order of inconsistency 2 so the matrices can match
Inconsistency2 <- Inconsistency2 %>%
  select(Q21_Inconsistency2, Q55_Inconsistency2, Q48_Inconsistency2, Q40_Inconsistency2,
         Q26_Inconsistency2, Q56_Inconsistency2, Q50_Inconsistency2, Q63_Inconsistency2)
    
    
BRIEF2PF3$InconsistencyScore <- abs(rowSums(Inconsistency1 - Inconsistency2))
BRIEF2PF3 <- mutate(BRIEF2PF3, Inconsistency = case_when(
  InconsistencyScore <= 5 ~ "Acceptable",
  InconsistencyScore == 6 | InconsistencyScore == 7 ~ "Questionable",
  InconsistencyScore >= 8 ~ "Inconsistent"
))



# Save the scored data
write.xlsx(x= BRIEF2PF3, file = save.pathway_BRPF)


# Save the Notes as a CSV
write_csv(x = BRPF_Notes, save.pathway.notes)


# Make a note that the data was saved successfully
cat("Saving processed BRIEF2 Self Report\n")

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation"))])

# Set a pause time for 1 second
Sys.sleep(1)

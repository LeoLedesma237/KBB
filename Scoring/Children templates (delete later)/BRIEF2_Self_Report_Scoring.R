# Load in packages
library(tidyverse)
library(readxl)

# set working directory
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/BRIEF2_SRF/raw_data")

# import data
BRIEF2_SR_list <- list.files()

# import all data
BRIEF2 <- list()

for(ii in 1:length(BRIEF2_SR_list)) {
  
  BRIEF2[[ii]] <- read_excel(BRIEF2_SR_list[[ii]])  
  
}

BRIEF2_uncleaned <- do.call(rbind,BRIEF2)

# Clean the data
BRIEF2 <- BRIEF2_uncleaned %>%
  select(Child_ID,
         paste("_",1:55,sep=""))

BRIEF2_long <- pivot_longer(BRIEF2,
                              cols = starts_with("_"),
                              names_to = "Items",
                              values_to = "Score")


# Convert the Score answers into numeric values
Score_converter_fun <- function(x) {
  if(x == "N") {
    return(1)
    
  } else if (x == "S") {
    return(2)
    
  } else if (x == "O") {
    return(3)
    
  }
}

BRIEF2_long$Score <- sapply(BRIEF2_long$Score, Score_converter_fun) 


# Import subtest information for the BRIEF2
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/BRIEF2_SRF/subtests")
subtest <- read_excel("BRIEF2_subtests.xlsx")

unique_IDs_num <- unique(BRIEF2$Child_ID) %>% length()

BRIEF2_long$subtest <- rep(subtest$Subtest, unique_IDs_num)


# Create a table to show performance
BRIEF2_scores_long <- BRIEF2_long %>% 
  group_by(Child_ID,subtest) %>%
  summarize(Sum_correct = sum(Score)) %>%
  data.frame() %>%
  filter(subtest != "F")

# Convert from long to short format
BRIEF2_scores_overkill <- pivot_wider(BRIEF2_scores_long, names_from = subtest, values_from = Sum_correct) %>% as_tibble

BRIEF2_scores_overkill <- BRIEF2_scores_overkill %>%
  select(Child_ID,
         EC = Emotional_Control,
         IN = Inhibit,
         PO = Plan_Organize,
         SM = Self_Monitor,
         SH = Shift,
         TC = Task_Completion,
         WM = Working_Memory)


# Merge some of the subtests into more global measures
BRIEF2_scores <- BRIEF2_scores_overkill %>%
  mutate(BRI = IN + SM, ERI = EC + SH, CRI = TC + WM + PO) %>%
  mutate(GEC = BRI + ERI + CRI) %>%
  select(Child_ID, BRI, ERI, CRI, GEC)

# Save the data
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/BRIEF2_SRF/final_data")

save(BRIEF2_scores, file = "BRIEF2_SR.Rdata")

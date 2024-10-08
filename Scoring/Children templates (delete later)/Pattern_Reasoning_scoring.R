# Load in the packages
library(tidyverse)
library(readxl)

# set working directory
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/Pattern_Reasoning/raw_data")

# Import the data
file_names <- list.files()
files_to_be_imported <- list()

for(ii in 1:length(file_names)) {
  
  files_to_be_imported[[ii]] <- read_excel(file_names[[ii]])
  
}

Pattern_Reasoning_uncleaned <- do.call(rbind,files_to_be_imported)

# Remove duplicates
Pattern_Reasoning_uncleaned <- unique(Pattern_Reasoning_uncleaned)

names(Pattern_Reasoning_uncleaned)

# clean the data
Pattern_Reasoning <- Pattern_Reasoning_uncleaned %>%
  select(Child_ID,
         Date_of_Evaluation,
         Evaluator_ID,
         paste("Iterm_",1:11,sep=""),
         "Item_12",
         paste("Iterm_",13:36,sep=""))


# Convert to long form
pattern_reasoning_long <- pivot_longer(Pattern_Reasoning,
                                       cols = starts_with("Ite"),
                                       names_to = "Question",
                                       values_to = "Answer")
# Convert any NA's in Answer to 0
pattern_reasoning_long$Answer <- ifelse(is.na(pattern_reasoning_long$Answer), 0, pattern_reasoning_long$Answer)

# Transform 888 (NRG) or 777 (CDK) to 0
pattern_reasoning_long$Answer <- ifelse(pattern_reasoning_long$Answer == 777, 0, pattern_reasoning_long$Answer)
pattern_reasoning_long$Answer <- ifelse(pattern_reasoning_long$Answer == 888, 0, pattern_reasoning_long$Answer)

# Create a table to show performance
pattern_reasoning_scores <- pattern_reasoning_long %>% 
  group_by(Child_ID) %>%
  summarize(PR = sum(Answer)) %>%
  as_tibble()

# Save the data
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/Pattern_Reasoning/final_data")

save(pattern_reasoning_scores, file= "Pattern Reasoning.Rdata")

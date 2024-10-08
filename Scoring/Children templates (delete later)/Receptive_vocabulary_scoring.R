# The following R script will do some cleaning and scoring for the Receptive Vocabulary assessment

library(readxl)
library(tidyverse)

# set working directory
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/Receptive_Vocabulary/raw_data")

# load in data
receptive_vocabulary_raw_list <- list.files()

# import all data
receptive_vocabulary_raw <- list()

for(ii in 1:length(receptive_vocabulary_raw_list)) {
  
  receptive_vocabulary_raw[[ii]] <- read_excel(receptive_vocabulary_raw_list[[ii]])  
  
}

receptive_vocabulary_uncleaned <- do.call(rbind,receptive_vocabulary_raw)

# get to know your data
dim(receptive_vocabulary_uncleaned)

# Remove redundancies 
receptive_vocabulary_uncleaned <- unique(receptive_vocabulary_uncleaned)

# Clean the data
# There should be 54 questions
receptive_vocabulary <- receptive_vocabulary_uncleaned %>%
  select(Child_ID= Child_Study_ID,
         Evaluator_ID,
         Date_of_Evaluation,
         paste("PPV",1:12,sep=""),
         "PPV13_C",
         "PPV14",
         "PPV15",
         "PPV16_Ciindi",
         paste("PPV",17:30,sep=""))

receptive_vocabulary_long <- pivot_longer(receptive_vocabulary, 
                                          cols = starts_with("PPV"), 
                                          names_to = "Question", 
                                          values_to = "Answer")

# Convert all NA's into 0's
receptive_vocabulary_long$Answer <- ifelse(is.na(receptive_vocabulary_long$Answer), 0, receptive_vocabulary_long$Answer)


# Import the scoring key
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/Receptive_Vocabulary/answer_keys")
answer_key <- read_excel("Receptive_Vocabulary_Answer_key.xlsx")

# Add the scoring key to the long data frame
Num_Children <- unique(receptive_vocabulary_long$Child_ID) %>% length()
receptive_vocabulary_long$Answer_key <- rep(answer_key$Answer,Num_Children)

receptive_vocabulary_long$score <- ifelse(receptive_vocabulary_long$Answer == receptive_vocabulary_long$Answer_key, 1, 0)

# Create a table to show performance
receptive_vocabulary_scores <- receptive_vocabulary_long %>% 
  group_by(Child_ID) %>%
  summarize(RV = sum(score)) %>%
  as_tibble()

receptive_vocabulary_scores

# Save the data
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/Receptive_Vocabulary/final_data")

save(receptive_vocabulary_scores, file= "Receptive Vocabulary.Rdata")

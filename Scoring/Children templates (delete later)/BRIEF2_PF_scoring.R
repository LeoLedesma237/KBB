# load in packages
library(tidyverse)
library(readxl)

# set working directory
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Adults/BRIEF2_PF/raw_data")

# import data
BRIEF_PF_raw_list <- list.files()

# import all data
BRIEF_PF_raw <- list()

for(ii in 1:length(BRIEF_PF_raw_list)) {
  
  BRIEF_PF_raw[[ii]] <- read_excel(BRIEF_PF_raw_list[[ii]])  
  
}

BRIEF_PF_uncleaned <- do.call(rbind,BRIEF_PF_raw)


# Data cleaning
names(BRIEF_PF_uncleaned)

BRIEF_PF_uncleaned <- BRIEF_PF_uncleaned %>%
  select(Child_ID = Child_ID, everything())


BRIEF_PF_scores <- unique(BRIEF_PF_uncleaned$Child_ID) %>% as_tibble %>% select(Child_ID = value)
BRIEF_PF_scores$BRIEF2_PF <- rep("Yes",nrow(BRIEF_PF_scores))

# save the data
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Adults/BRIEF2_PF/final_data")

save(BRIEF_PF_scores, file = "BRIEF_PF_scores.Rdata")


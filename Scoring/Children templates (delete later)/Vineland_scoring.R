# load in packages
library(tidyverse)
library(readxl)

# set working directory
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Adults/Vineland_Adaptive_Behavior_Scales/raw_data")

# import data
Vineland_raw_list <- list.files()

# import all data
Vineland_raw <- list()

for(ii in 1:length(Vineland_raw_list)) {
  
  Vineland_raw[[ii]] <- read_excel(Vineland_raw_list[[ii]])  
  
}

Vineland_uncleaned <- do.call(rbind,Vineland_raw)


# Data cleaning
names(Vineland_uncleaned)

Vineland_uncleaned <- Vineland_uncleaned %>%
  select(Child_ID = Child_Study_ID, everything())


Vineland_scores <- unique(Vineland_uncleaned$Child_ID) %>% as_tibble %>% select(Child_ID = value)
Vineland_scores$Vineland <- rep("Yes",nrow(Vineland_scores))

# save the data
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Adults/Vineland_Adaptive_Behavior_Scales/final_data")

save(Vineland_scores, file = "Vineland_scores.Rdata")


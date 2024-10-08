# load in packages
library(tidyverse)
library(readxl)

# set working directory
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Adults/ASEBA_CBC/raw_data")

# import data
ASEBA_raw_list <- list.files()

# import all data
ASEBA_raw <- list()

for(ii in 1:length(ASEBA_raw_list)) {
  
  ASEBA_raw[[ii]] <- read_excel(ASEBA_raw_list[[ii]])  
  
}

ASEBA_uncleaned <- do.call(rbind,ASEBA_raw)


# Data cleaning
names(ASEBA_uncleaned)

ASEBA_uncleaned <- ASEBA_uncleaned %>%
  select(Child_ID = Child_s_Study_ID, everything())


ASEBA_CBC_scores <- unique(ASEBA_uncleaned$Child_ID) %>% as_tibble %>% select(Child_ID = value)
ASEBA_CBC_scores$ASEBA_CBC <- rep("Yes",nrow(ASEBA_CBC_scores))

# save the data
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Adults/ASEBA_CBC/final_data")

save(ASEBA_CBC_scores, file = "ASEBA_CBC_scores.Rdata")


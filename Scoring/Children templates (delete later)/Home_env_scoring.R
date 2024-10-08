# load in packages
library(tidyverse)
library(readxl)

# set working directory
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Adults/Home_Environment_Survery/raw_data")

# import data
Home_env_raw_list <- list.files()

# import all data
Home_env_raw <- list()

for(ii in 1:length(Home_env_raw_list)) {
  
  Home_env_raw[[ii]] <- read_excel(Home_env_raw_list[[ii]])  
  
}

Home_env_uncleaned <- do.call(rbind,Home_env_raw)


# Data cleaning
names(Home_env_uncleaned)

Home_env_uncleaned <- Home_env_uncleaned %>%
  select(Child_ID = Proband_ID, everything())


Home_env_scores <- unique(Home_env_uncleaned$Child_ID) %>% as_tibble %>% select(Child_ID = value)
Home_env_scores$Home_Env <- rep("Yes",nrow(Home_env_scores))

# save the data
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Adults/Home_Environment_Survery/final_data")

save(Home_env_scores, file = "Home_env_scores.Rdata")


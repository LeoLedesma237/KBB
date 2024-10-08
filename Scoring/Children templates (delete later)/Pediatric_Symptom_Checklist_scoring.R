# load in packages
library(tidyverse)
library(readxl)

# set working directory
setwd("~/KBB_new_2/2_behavioral_assessments/Adults/Pediatric_Symptom_Checklist_(PSC)/raw_data")

# import data
PSC_raw_list <- list.files()

# import all data
PSC_raw <- list()

for(ii in 1:length(PSC_raw_list)) {
  
  PSC_raw[[ii]] <- read_excel(PSC_raw_list[[ii]])  
  
}

PSC_uncleaned <- do.call(rbind,PSC_raw)


# Data cleaning
names(PSC_uncleaned)

PSC_uncleaned <- PSC_uncleaned %>%
  select(Child_ID = Child_Study_ID, everything())


PSC_scores <- unique(PSC_uncleaned$Child_ID) %>% as_tibble %>% select(Child_ID = value)
PSC_scores$PSC <- rep("Yes",nrow(PSC_CBC_scores))

# save the data
setwd("~/Documents/Zambia Data and r code/Behavioral Assessments/Adults/Pediatric_Symptom_Checklist_(PSC)/raw_data")

save(PSC_scores, file = "PSC_scores.Rdata")


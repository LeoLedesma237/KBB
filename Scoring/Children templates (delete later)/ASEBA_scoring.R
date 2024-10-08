# load in packages
library(tidyverse)

# set working directory
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/ASEBA_YSR/raw_data")

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

ASEBA_uncleaned1 <- ASEBA_uncleaned %>%
  select(Child_ID = Child_study_ID,
         starts_with("_"))
         
ASEBA <- ASEBA_uncleaned1 %>%
  select(- c(`_GPS_altitude`,
              `_GPS_longitude`,
             `_GPS_latitude`,
             `_GPS_precision`,
             `_uuid`,
             `_id`,
             `_validation_status`,
             `_notes`,
             `_status`,
             `_submitted_by`,
             `__version__`,
             `_tags`,
             `_index`,
             `_submission_time`))

ASEBA_long <- pivot_longer(ASEBA,
             cols = starts_with("_"),
             names_to = "Items",
             values_to = "Score")

ASEBA_scores <- unique(ASEBA_long$Child_ID) %>% as_tibble %>% select(Child_ID = value)
ASEBA_scores$AS <- rep("Yes",nrow(ASEBA_scores))

# Save data
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/ASEBA_YSR/final_data")

save(ASEBA_scores, file = "ASEBA.Rdata")


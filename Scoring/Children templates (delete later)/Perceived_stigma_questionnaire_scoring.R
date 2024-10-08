# load in packages
library(tidyverse)
library(readxl)

# set working directory
setwd("~/Documents/Zambia Data and r code/Behavioral Assessments/Adults/Perceived_Stigma_Questionnaire_(PSQ)/raw_data")

# import data
PSQ_raw_list <- list.files()

# import all data
PSQ_raw <- list()

for(ii in 1:length(PSQ_raw_list)) {
  
  PSQ_raw[[ii]] <- read_excel(PSQ_raw_list[[ii]])  
  
}

PSQ_uncleaned <- do.call(rbind,PSQ_raw)


# Data cleaning
names(PSQ_uncleaned)

PSQ_uncleaned <- PSQ_uncleaned %>%
  select(Child_ID = Proband_ID, everything())


PSQ_scores <- unique(PSQ_uncleaned$Child_ID) %>% as_tibble %>% select(Child_ID = value)
PSQ_scores$PSQ <- rep("Yes",nrow(PSQ_CBC_scores))

# save the data
setwd("~/Documents/Zambia Data and r code/Behavioral Assessments/Adults/Perceived_Stigma_Questionnaire_(PSQ)/final_data")

save(PSQ_scores, file = "PSQ_scores.Rdata")


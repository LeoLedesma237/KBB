# This task will do the following:

library(readxl)
library(tidyverse)

# set working directory
setwd("~/KBB_new_2/2_behavioral_assessments/Children/Atlantis/raw_data")

# import data
atlantis_raw_list <- list.files()

# import all data
atlantist_raw <- list()

for(ii in 1:length(atlantis_raw_list)) {
  
  atlantist_raw[[ii]] <- read_excel(atlantis_raw_list[[ii]])  
  
}

atlantis <- do.call(rbind,atlantist_raw)


# get to know your data
dim(atlantis)

# Clean the data
# There should be 54 questions
cleaned_atlantis <- atlantis %>%
  select(Evaluator_Initials = Evaluator_Intials,
         Evaluator_ID,
         Date_of_Evaluation,
         Child_ID = Child_s_Study_ID,
         `_1_KOH_fish`,
         `_2_ZUKE_fish`,
         `_3_KOH_fish`,
         `_4_ZUKE_fish`,
         `_5_LOPS_fish`,
         `_6_LOPS_fish`,
         `_7_NEEF_fish`,
         `_8_KOH_fish`,
         `_9_ZUKE_fish`,
         `_10_DA_LEE_plant`,
         `_11_NEEF_fish`,
         `_12_LOPS_fish`,
         `_13_DAB_lee_plant`,
         `_14_TY_lar_plant`,
         `_15_KOH`,
         `_16_KOH_plant`,
         `_17_TY_lar_plant`,
         `_18_ZUKE_fish`,
         `_19_NEE_dew_plant`,
         `_20_NEEF_fish`,
         `_21_NEEF_fish`,
         `_22_MAY_car_plant`,
         `_23_KOH_fish`,
         `_24_NEE_dew`,
         `_25_LOPS_fish`,
         `_26_DAB_lee_plant`,
         `_27_WIM_ple_mat_shell`,
         `_28_NEEF_fish`,
         `_29_MAY_car_plant`,
         `_30_KOH_fish`,
         `_31_TY_lar_plant`,
         `_32_WIM_ple_mat_shell`,
         `_33_ZUKE_fish`,
         `_34_DAB_lee_plant`,
         `_35_JEN_a_lease_shell`,
         `_36_NO_NAME_fish`,
         `_37_LOPS_fish`,
         `_38_SPEE_mar_ton_shell`,
         `_39_ZUKE_fish`,
         `_40_JEN_a_lease`,
         `_41_NEE_dew`,
         `_42_NO_NAME_FIsh`,
         `_43_TY_lar_plant`,
         `_44_SPEE_mar_ton_shell`,
         `_45_KOH_fish`,
         `_46_TROH_zen_dill_shell`,
         `_47_MAY_car_plant`,
         `_48_NO_NAME`,
         `_49_DAB_lee_plant`,
         `_50_NEE_dew_plant`,
         `_51_TROH_zen_dill_shell`,
         `_52_NEEF_fish`,
         `_53_WIM_ple_mat_shell`,
         `_54_NO_NAME`)


# Change the data to long format
atlantis_long <- pivot_longer(cleaned_atlantis,
             cols = starts_with("_"),
             names_to = "Items",
             values_to = "Score")

# Remove any 888 or 777
atlantis_long$Score <- ifelse(atlantis_long$Score %in% c(888,777),0, atlantis_long$Score)

# Score performance by ID
atlantis_scores <- atlantis_long %>% 
  group_by(Child_ID) %>%
  summarize(AT = sum(Score, na.rm = TRUE))

# Set working directory to save this
setwd("~/KBB_new_2/2_behavioral_assessments/Children/Atlantis/final_data")

# Save the data
save(atlantis_scores, file = "atlantis.Rdata")


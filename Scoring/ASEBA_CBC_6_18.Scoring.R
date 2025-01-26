# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)

# Read in CBC file for the older kids
CBC_6_18 <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Adults/ASEBA_CBC_6_18_Raw.xlsx"))

# Create a save pathway
save.pathway <- paste(DataLocation,"FINAL_DS/Behavioral/Adults/CBC_6_18.xlsx", sep="")

# Create a save pathway for Notes
save.pathway.notes <- paste(DataLocation,"REPORTS/Individual/CBC_6_18.csv", sep="")


###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############


# Minor data Cleaning
CBC_6_18 <- dplyr::rename(CBC_6_18, Child_ID = Child_s_Study_ID)

# Check the IDs for errors
source("Scoring/scoring_functions/IDError_FUNCTION.R")
CBC_6_18_Notes <-check_id_errors("ASEBA CBC 6-18", CBC_6_18$Child_ID)

# Select Vars of Interest 
Front <- CBC_6_18 %>%
  select(Child_ID,
         Evaluator_ID = Evaluator_s_ID,
         Date_of_Evaluation)


Items_Raw <- CBC_6_18 %>%
  select(`_1_Ucita_mbuli_mwana_kwiinda_musela_wakwe`:`_113_Mwalombwa_amul_mutwaambo_atala_awa`)

# Rename the Raw Items
names(Items_Raw) <- paste0("CBC6_18_",1:length(Items_Raw))

# Missing data (as of 1/25/25) is 104 rows but that is due to Q63 and Q120
Items_Raw %>% filter(!complete.cases(.))

# Data cleaning: Changing values to correct numbers!
Items_Raw$CBC6_18_57 <- ifelse(Items_Raw$CBC6_18_57 == "0_1", "2", Items_Raw$CBC6_18_57)
Items_Raw$CBC6_18_62 <- ifelse(Items_Raw$CBC6_18_62 == "talimasimpe", "0", 
                               ifelse(Items_Raw$CBC6_18_62 == "zimwi_ziindi", "1", "2"))
Items_Raw$CBC6_18_65 <- ifelse(Items_Raw$CBC6_18_65 == "`1", "1", Items_Raw$CBC6_18_65)


# Data cleaning: Converting everything into numeric
Items_Raw2 <- sapply(Items_Raw, function(x) as.numeric(x)) %>% cbind() %>% data.frame()

###
###### Creating Vectors for Each Problems/Scale
###

### Internalizing Problems
anx_dep <- c(14, 29, 30, 31, 32, 33, 35, 45, 50, 52, 71, 91, 112)
wit_dep <- c(5, 42, 65, 69, 75, 102, 103, 111)
som_com <- c(47, 49, 51, 54, 56) #Q56 not program correctly- this measure is inaccurate

#### Other (who knows)
soc_pro <- c(11, 12, 25, 27, 34, 36, 38, 48, 62, 64, 79)
tho_pro <- c(9, 18, 40, 46, 58, 59, 60, 66, 70, 76, 83, 84, 85, 92, 100)
att_pro <- c(1, 4, 8, 10, 13, 17, 41, 61, 78, 80)

#### Externalizing Problems
rul_beh <- c(2, 26, 28, 39, 43, 63, 67, 72, 73, 81, 82, 90, 96, 99, 101, 105, 106)
agg_beh <- c(3, 16, 19, 20, 21, 22, 23, 37, 57, 68, 86, 87, 88, 89, 94, 95, 97, 104)

# Other Problems
oth_pro <- c(6, 7, 15, 24, 44, 53, 55, 74, 77, 93, 98, 107, 108, 109, 110, 113)

# Create a list that will be used to score the columns of the dataset
scoring_list <- list(anxious_depressed = anx_dep,
                     withdrawn_depressed = wit_dep,
                     somatic_complaints = som_com,
                     social_problems = soc_pro,
                     thought_problems = tho_pro,
                     attention_problems = att_pro,
                     ruleBreaking_behavior = rul_beh,
                     aggressive_behavior = agg_beh,
                     other_problems = oth_pro)

###
###### Scoring the data
###

scored_list <- list()

for(ii in 1:length(scoring_list)) {
  
  # Save the current scored list as a vector
  scored_list[[ii]] <- rowSums(Items_Raw2[,scoring_list[[ii]]], na.rm = T) # There is some missing data :(
  
}

# Cbind them into a dataset
scored_df <- data.frame(do.call(cbind, scored_list))

# Add Internal, Externalize and total Scoring
scored_df$X10 <- rowSums(scored_df[,1:3])
scored_df$X11 <- rowSums(scored_df[,7:8])
scored_df$X12 <- rowSums(scored_df[,1:9])

# Rename the dataframe
names(scored_df) <- c(names(scoring_list), "Internalizing", "Externalizing", "Total Prob")

# Recreate the final dataset
CBC6_18_2 <- tibble(cbind(Front, Items_Raw, scored_df))

# Save the scored data
write.xlsx(x= CBC6_18_2, file = save.pathway)

# Save the Notes as a CSV
write_csv(x = CBC_6_18_Notes, save.pathway.notes)

# Make a note that the data was saved successfully
cat("Saving processed CBC_6_18\n")

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation"))])

# Set a pause time for 1 second
Sys.sleep(1)

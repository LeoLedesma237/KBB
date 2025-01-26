# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)

# Read in the CBC file for the younger kids
CBC3_6 <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Adults/ASEBA_CBC_3_6_Raw.xlsx"))

# Create a save pathway
save.pathway <- paste(DataLocation,"FINAL_DS/Behavioral/Adults/CBC_3_6.xlsx", sep="")

# Create a save pathway for Notes
save.pathway.notes <- paste(DataLocation,"REPORTS/Individual/CBC_3_6.csv", sep="")


###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############


# Minor data Cleaning
CBC3_6 <- dplyr::rename(CBC3_6, Child_ID = Child_s_ID)

# Check the IDs for errors
source("Scoring/scoring_functions/IDError_FUNCTION.R")
CBC_3_6_Notes <-check_id_errors("ASEBA CBC 3-6", CBC3_6$Child_ID)

# Select Vars of Interest 
Front <- CBC3_6 %>%
  select(Child_ID,
         Evaluator_ID = Evaluator_s_ID,
         Date_of_Evaluation)


Items_Raw <- CBC3_6 %>%
  select(`_1_kubabwa_nokuba_ku_a_mwida_nokuba_mutwe`:`_100_Amulembe_kufumbw_ngajisi_ataambwa_awa`)

# Rename the Raw Items
names(Items_Raw) <- paste0("CBC3_6_",1:length(Items_Raw))

# There are 5 rows with NA's but its for Q100, which is okay!
Items_Raw %>% filter(!complete.cases(.))

# Data cleaning: Changing values to correct numbers!
Items_Raw$CBC3_6_44 <- ifelse(Items_Raw$CBC3_6_44 == "2g", "2", Items_Raw$CBC3_6_44)
Items_Raw$CBC3_6_98 <- ifelse(Items_Raw$CBC3_6_98 == "0_1", "2", Items_Raw$CBC3_6_98)

# Data cleaning: Converting everything into numeric
Items_Raw2 <- sapply(Items_Raw, function(x) as.numeric(x)) %>% cbind() %>% data.frame()

###
###### Creating Vectors for Each Problems/Scale
###

#### Internalizing Problems
emo_reac <- c(21, 46, 51, 79, 82, 83, 92, 97, 99)
anx_depr <- c(10, 33, 37, 43, 47, 68, 87, 90)
som_comp <- c(1, 7, 12, 19, 24, 39, 45, 52, 78, 86, 93)
withdr <- c(2, 4, 23, 62, 67, 70, 71, 96)

# Sleep its own thing
sleep_prob <- c(22, 38, 48, 64, 74, 84, 94)

# Externalizing Problems
atten_prob <- c(5, 6, 56, 59, 95)
aggr_beh <- c(8, 15, 16, 18, 20, 27, 29, 35, 40, 42, 44, 53, 58, 66, 69, 81, 85, 88, 96)

# This is its own thing
othr_prob <- c(3, 9, 11, 13, 14, 17, 25, 26, 28, 30, 31, 32, 34, 36, 41, 49,
               50, 54, 55, 57, 60, 61, 63, 65, 72, 73, 75, 76, 77, 80, 89, 91, 100)

# DSM orientated scaled
depress_scale <- c(13, 24, 38, 43, 49, 50, 71, 74, 89, 90)
anxiet_scale <- c(10, 22, 28, 32, 37, 47, 48, 51, 87, 99)
autism_scale <- c(4, 7, 21, 23, 25, 63, 67, 70, 76, 80, 92, 98)
ADHD_scale <- c(5, 6, 8, 16, 36, 59)
opp_def_prob_scale <- c(15, 20, 44, 81, 85, 88)


# Create a list that will be used to score the columns of the dataset
scoring_list <- list(emotionally_reactive = emo_reac, 
                     anxious_depressed = anx_depr,
                     somatic_complaints = som_comp,
                     withdrawn = withdr,
                     sleep_problems = sleep_prob,
                     attention_problems = atten_prob,
                     aggressive_behaviors = aggr_beh,
                     other_problems = othr_prob,
                     depressive_problems = depress_scale,
                     anxiety_problems = anxiet_scale,
                     autism_spectrum_problems = autism_scale,
                     attention_deficit_hyperactivity_problems = ADHD_scale,
                     opositional_defiant_problems = opp_def_prob_scale)

###
###### Scoring the data
###

scored_list <- list()

for(ii in 1:length(scoring_list)) {

  # Save the current scored list as a vector
  scored_list[[ii]] <- rowSums(Items_Raw2[,scoring_list[[ii]]], na.rm = T) # Only applies to Q100

}

# Cbind them into a dataset
scored_df <- data.frame(do.call(cbind, scored_list))

# Add Internal, Externalize and total Scoring
scored_df$x14 <- rowSums(scored_df[,1:4])
scored_df$x15 <- rowSums(scored_df[,6:7])
scored_df$x16 <- rowSums(scored_df[,1:8])

# Rename the dataframe
names(scored_df) <- c(names(scoring_list), "Internalizing", "Externalizing", "Total Prob")

# Reorder variables to reduce confusion
scored_df <- select(scored_df, 
                    emotionally_reactive:other_problems,
                    Internalizing:`Total Prob`,
                    depressive_problems:opositional_defiant_problems)

# Recreate the final dataset
CBC3_6_2 <- tibble(cbind(Front, Items_Raw, scored_df))


# Save the scored data
write.xlsx(x= CBC3_6_2, file = save.pathway)

# Save the Notes as a CSV
write_csv(x = CBC_3_6_Notes, save.pathway.notes)

# Make a note that the data was saved successfully
cat("Saving processed CBC_3_6\n")

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation"))])

# Set a pause time for 1 second
Sys.sleep(1)

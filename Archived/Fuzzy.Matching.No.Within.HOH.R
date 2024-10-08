# Quality control script using Fuzzy Matching. 
library(readxl)
library(tidyverse)
library(ggplot2)
library(fuzzyjoin)
library(openxlsx)

# Set working directory
setwd("~/KBB_new_2/1_screener/final_data")

# Load in the data
No.Matches.Within.HOH <- read_excel("3) HOH No Matches (level 1).xlsx")

# Select variables of interest
No.Matches.Within.HOH <- select(No.Matches.Within.HOH, HOH_ID, Name_of_the_Village, Date_of_Evaluation, Child_ID, KBB_DD_status)

# Load in cleaned processed CFM data
setwd("~/KBB_new_2/1_screener/processed_data")

CFM2_4.location <- read.csv("CFM2_4_clean.csv") %>% select(Child_ID, GPS.lat, GPS.long)
CFM5_17.location <- read.csv("CFM5_17_clean.csv") %>% select(Child_ID, GPS.lat, GPS.long)

Binded.data.location <- rbind(CFM2_4.location, CFM5_17.location)

# Introduce the location information into the No Matches dataset
No.Matches.Within.HOH <- No.Matches.Within.HOH %>%
  left_join(Binded.data.location, by = "Child_ID")

# Split the data into DD and no DD
DD.No.Matches <- filter(No.Matches.Within.HOH, KBB_DD_status == "Yes")
noDD.No.Matches <- filter(No.Matches.Within.HOH, KBB_DD_status == "No")


# Print a graph with location
Binded.data.location %>%
  ggplot(aes(x = GPS.lat, y = GPS.long)) +
  geom_point(color = "blue") +
  labs(title = "Data collection Location in Choma") +
  theme_linedraw()




# Take the HOH_IDs for both groups and fuzzy match them.
fuzzy.matched.results <- stringdist_join(DD.No.Matches, noDD.No.Matches, 
                                            by='HOH_ID', #match based on HOH_ID
                                            mode='left', #use left join
                                            method = "osa", #use jw distance metric
                                            max_dist=3, 
                                            distance_col='dist') %>%
  filter(complete.cases(.))

# Save this as an excel sheet 
setwd("~/KBB_new_2/Fuzzy matching")

write.xlsx(fuzzy.matched.results, "Potential HOH IDs Fuzzy Matched in HOH No Matches (level 1).xlsx")

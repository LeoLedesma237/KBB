# Load in packages
library(tidyverse)
library(readxl)

# Set working directory to obtain problematic data
setwd("~/KBB_new_2/errors")

problematic.data <- read_excel("wrong.screener.xlsx")

# Set working directory to obtain data from all children screened
setwd("C:/Users/lledesma.TIMES/Documents/KBB_new_2/1_screener/final_data")

# Load in all useful data
All.Children <- read_excel("All Children.xlsx")

# Remove problematic screeners
All.Children.np <- All.Children %>%
  filter(!(Child_ID %in% problematic.data$Child_ID)) 

# Obtain the numbers that Mei needs
# 
All.Children.np$KBB_DD_status

table(All.Children.np$Child_Gender)


# Use the mutate function and else if
All.Children.np <- All.Children.np %>%
  mutate(CFM_DD_status = ifelse(CFM_DD_status == "Yes", "DD","noDD"),
         epilepsy = ifelse(epilepsy == "Yes", "Epilepsy", "noEpilepsy"))

table(All.Children.np$CFM_DD_status, All.Children.np$Child_Gender)

# Obtain a dataframe of all children that do not have that DD difficulty variable blank
All.Children.KBB.CFM.difficulties <- All.Children.np %>%
  drop_na(KBB_DD_type)


All.Children.KBB.CFM.difficulties.bois <- All.Children.KBB.CFM.difficulties %>%
  filter(Child_Gender == "male")
  
  
  All.Children.KBB.CFM.difficulties.girls <-  All.Children.KBB.CFM.difficulties %>%
    filter(Child_Gender == "female")

# Get the type of difficulty for males and females separately
Boys.difficulties <- str_split(All.Children.KBB.CFM.difficulties.bois$KBB_DD_type, "; ") %>%
  do.call(c,.) %>%
  table() %>%
  cbind() %>%
  data.frame(frequency =.)

Girls.difficulties <- str_split(All.Children.KBB.CFM.difficulties.girls$KBB_DD_type, "; ") %>%
  do.call(c,.) %>%
  table() %>%
  cbind() %>%
  data.frame(frequency = .)

# Rename the variables
names(Boys.difficulties) <- c("Boys.Frequency")
names(Girls.difficulties) <- c("Girls.Frequency")

# Add sex as a variable
Boys.difficulties$Difficulty <- row.names(Boys.difficulties)
Girls.difficulties$Difficulty <- row.names(Girls.difficulties)


# Remove the row names
row.names(Boys.difficulties) <- NULL
row.names(Girls.difficulties) <- NULL

# Merge these two together
x <- Boys.difficulties %>%
  full_join(Girls.difficulties , by = "Difficulty") %>%
  select(Difficulty, everything())

sum(x$Boys.Frequency)
sum(x$Girls.Frequency)

x %>%
  arrange(desc(Boys.Frequency))

x %>%
  arrange(desc(Girls.Frequency))

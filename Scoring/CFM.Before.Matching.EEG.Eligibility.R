library(tidyverse)
library(readxl)
library(lubridate)
library(openxlsx) # To save excel files with multiple tabs


# Set the directory to the clean screener
CleanedScreener_pw <- paste0(DataLocation,"MODIFIED_DS/Screener/")

# Set the save directory
save.pathwayScreener <- paste0(DataLocation,"FINAL_DS/Screener/")


###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############

# Load the cleaned CFM2_4
CFM2_4 <- read_csv(paste0(CleanedScreener_pw,"CFM2_4_clean.csv"))

# Load the cleaned CFM5_17
CFM5_17 <- read_csv(paste0(CleanedScreener_pw,"CFM5_17_clean.csv"))


# Select the variables that both datasets have in common
CFM2_4 <- CFM2_4 %>%
  select(HOH_ID, 
         Date_of_Evaluation, 
         Evaluator_ID, 
         Name_of_the_Village,
         Location_Type,
         HOH_First_Name,
         HOH_Last_Name,
         Respondant_First_Name,
         Respondant_Last_Name,
         Respondant_relationship,
         Child_First_Name,
         Child_Last_Name,
         Child_Gender,
         Child_age,
         Child_Date_of_Birth,
         BF,
         BM,
         glasses,
         hearing.aid,
         walking.equipment,
         Seeing = CF3_Seeing,
         Hearing = CF6_Hearing,
         Walking = CF10_Walking,
         Physical_difficulty_type,
         CFM_DD,
         KBB_CFM_DD,
         CFM_DD_type,
         KBB_CFM_DD_type,
         Epilepsy,
         KBB_DD_status,
         Excluded,
         Child_ID)

CFM5_17 <- CFM5_17 %>%
  select(HOH_ID, 
         Date_of_Evaluation, 
         Evaluator_ID, 
         Name_of_the_Village,
         Location_Type,
         HOH_First_Name,
         HOH_Last_Name,
         Respondant_First_Name,
         Respondant_Last_Name,
         Respondant_relationship,
         Child_First_Name,
         Child_Last_Name,
         Child_Gender,
         Child_age,
         Child_Date_of_Birth,
         BF,
         BM,
         glasses,
         hearing.aid,
         walking.equipment,
         Seeing = CF3_Seeing,
         Hearing = CF6_Hearing,
         Walking = CF13_Walking_500,
         Physical_difficulty_type,
         CFM_DD,
         KBB_CFM_DD,
         CFM_DD_type,
         KBB_CFM_DD_type,
         Epilepsy,
         KBB_DD_status,
         Excluded,
         Child_ID)

# Create a variable indicating the screener type
CFM2_4$Screener.Type <- rep("CFM2_4", nrow(CFM2_4))
CFM5_17$Screener.Type <- rep("CFM5_17", nrow(CFM5_17))

# Bind the datasets
Binded.data <- rbind(CFM2_4, CFM5_17)



# Halt children after this date
#Day = '10'
#Month = '09'
#Year =  '2023'

#Binded.data <- Binded.data %>% 
#  filter(Date_of_Evaluation < paste(Year, Month, Day, sep="-"))


# Correct screener used?
Screener.Test.fun <- function(Age, Screener.Type) {
  
  if(is.na(Age)) {
    return("Age is missing")
    
  }
  
  if(Screener.Type == "CFM2_4") {
    
    if(between(Age,1.8,5)) {
      return("Correct: CFM2-4")
    } else if(Age > 5) {
      return("Incorrect: Should have used CFM5-17")
    } else {
      return("Incorrect: Too young for the study")
    }
    
    
  } else if(Screener.Type == "CFM5_17") {
    
    if(between(Age,4.5,19)) {
      return("Correct: CFM5-17")
    } else if(Age > 19) {
      return("Incorrect: Too old for the study")
      
    } else {
      return("Incorrect: Should have used CFM2-4")
      
    }
  }
  
}

Binded.data <- Binded.data %>%
  mutate(Screener.Test = mapply(Screener.Test.fun, Child_age, Screener.Type))

# Print a data frame with the frequency of screener errors
data.frame(Frequency = cbind(table(Binded.data$Screener.Test)))

# Save the correct screeners 
Correct.Screeners <- Binded.data %>%
  filter(Screener.Test %in% c("Correct: CFM2-4", "Correct: CFM5-17"))

# Save the incorrect screeners
Incorrect.Screeners <- Binded.data %>%
  filter(!(Screener.Test %in% c("Correct: CFM2-4", "Correct: CFM5-17")))

# Save excluded children
Excluded.Children <- Correct.Screeners %>%
  filter(Excluded == "Yes")

# Save only children that do not have an excluded status
Not.Excluded.Children <- Correct.Screeners %>%
  filter(Excluded == "No")

# Nest the data by HOH_IDs
Not.Excluded.Children.Nested <- Not.Excluded.Children %>%
  group_by(HOH_ID) %>%
  nest()

# Run a function using the map from Purr to see if there are DD and non-DD status
Not.Excluded.Children.Mapped <- Not.Excluded.Children.Nested %>%
  mutate(table = map(.x = data, .f = ~table(.x$KBB_DD_status))) %>%
  mutate(matches = map(.x = table, .f = ~length(.x) == 2))

# Return household with at least one potential match 
HOH.Potential.Matches <- Not.Excluded.Children.Mapped[do.call(c,Not.Excluded.Children.Mapped$matches),]

# Return households with no matches
HOH.No.Matches <- Not.Excluded.Children.Mapped[!do.call(c,Not.Excluded.Children.Mapped$matches),]

# Unnest bothdatasets so they can be saved as CSVs
HOH.Potential.Matches.unnested <- unnest(HOH.Potential.Matches, data)
HOH.No.Matches.unnested <- unnest(HOH.No.Matches, data)

# Remove unneeded variables
HOH.Potential.Matches.unnested <- select(HOH.Potential.Matches.unnested, -c(table, matches))
HOH.No.Matches.unnested <- select(HOH.No.Matches.unnested, -c(table, matches))


## Add an overall summary to each of the datasets
Incorrect.Screeners$Overall.Summary <- "Incorrect Screener"
Excluded.Children$Overall.Summary <- "Excluded Children"
HOH.No.Matches.unnested$Overall.Summary <- "No Matches Within HOH"
HOH.Potential.Matches.unnested$Overall.Summary <- "To be Determined"

## Sort by date (Not Matches- it is better if they are alphabetical)
Incorrect.Screeners <- Incorrect.Screeners %>% arrange(Date_of_Evaluation)
Excluded.Children <- Excluded.Children %>% arrange(Date_of_Evaluation)
HOH.No.Matches.unnested <- HOH.No.Matches.unnested %>% arrange(Date_of_Evaluation)

# Load in the EEG_Eligibility Function
source("Scoring/scoring_functions/EEG_Eligibility_FUNCTION.R")

# Run the EEG Eligibility function on all screener datasets
Incorrect.Screeners<- dryEEG_function(Incorrect.Screeners)
Excluded.Children <- dryEEG_function(Excluded.Children)
HOH.No.Matches.unnested <- dryEEG_function(HOH.No.Matches.unnested)
HOH.Potential.Matches.unnested <- dryEEG_function(HOH.Potential.Matches.unnested)
Binded.data <- dryEEG_function(Binded.data)

# Save level one datasets
write.xlsx(list(data = Incorrect.Screeners), file =  paste0(save.pathwayScreener,"1) Incorrect Screeners (level 1).xlsx"))
write.xlsx(list(data = Excluded.Children), file =  paste0(save.pathwayScreener,"2) Excluded Children (level 1).xlsx"))
write.xlsx(list(data = HOH.No.Matches.unnested), file =  paste0(save.pathwayScreener,"3) HOH No Matches (level 1).xlsx"))
write.xlsx(list(data = HOH.Potential.Matches.unnested), file =  paste0(save.pathwayScreener,"4) HOH Potential Matches (level 1).xlsx"))
write.xlsx(list(data = Binded.data), file =  paste0(save.pathwayScreener,"All Children.xlsx"))


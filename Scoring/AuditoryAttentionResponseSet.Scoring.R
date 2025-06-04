# This is the script for scoring Auditory Attention and Response Set
# We are starting from the Root Directory /KBB

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr) 

# Set location to where the individual files are
AuditoryAttention_location <- paste0(DataLocation,"RAW_DATA/Behavioral/Children/auditory_attention_data/")
ResponseSet_location <- paste0(DataLocation,"RAW_DATA/Behavioral/Children/response_set_data/")

# Create a save pathway
save.pathwayAA <- paste(DataLocation,"FINAL_DS/Behavioral/Children/AuditoryAttention.xlsx", sep="")
save.pathwayRS <- paste(DataLocation,"FINAL_DS/Behavioral/Children/ResponseSet.xlsx", sep="")

# Create a save pathway for Notes
save.pathway.notesAA <- paste(DataLocation,"REPORTS/Individual/AuditoryAttention.csv", sep="")
save.pathway.notesRS <- paste(DataLocation,"REPORTS/Individual/ResponseSet.csv", sep="")

###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############


######
############### Scoring Auditory Attention
######
 
# Extract full file names (set full.names = False, helps with for loop code)
AA_file_names <- list.files(AuditoryAttention_location, pattern = ".csv")

# Create empty lists
AA_files_list <- list()

# Load in all files for Auditory Attention
for(ii in 1:length(AA_file_names)) {
  
  # Extract current file name ID
  current_file_name <- sub("_.*", "", AA_file_names[ii])
  
  # Read in current file
  current_read_file <- read.csv(paste0(AuditoryAttention_location, AA_file_names[ii]))
  
  # If file has no row information skip iteration
  if(dim(current_read_file)[1] == 0){
    next
  }
  
  # If they have 'Countdown_1.stopped' as a variable remove it
  if(grepl("Countdown_1.stopped", paste(names(current_read_file),collapse = " "))){
    current_read_file <- select(current_read_file, - Countdown_1.stopped)
  }
  
  # If they have the following variables (remove and target them as 'Yes' for practice)
  if(grepl("Practice.started", paste(names(current_read_file),collapse = " "))){
    current_read_file <- current_read_file[, !names(current_read_file) %in% 
                                             c("thisRow.t", "notes", "Practice.started", "Practice.stopped",
                                               "WelcomeScreen.started", "WelcomeScreen.stopped", "AuditoryTrial.started",
                                               "AuditoryTrial.stopped", "expStart")]
    current_read_file$Practice <- "Yes"
    } else {
      current_read_file$Practice <- "No"
    
  }
  
  # Introduce the ID into the dataset
  current_read_file$Child_ID <- current_file_name
  
  #If they do not have 35 variables skip the iteration (either a test or incomplete session)
  if(dim(current_read_file)[2] != 35){
   next
  }

  # Save this information into the list
  AA_files_list[[ii]] <- current_read_file
  names(AA_files_list)[ii] <- current_file_name
  
}

# Combining them was successful
AA_merged <- unique(do.call(rbind, AA_files_list)) # Unique removes literally duplicate data
  
# Keep variables of interest (Cleaning the data)
AA_merged2 <- AA_merged %>%
  select(Child_ID, date, Stimulus = `X...Auditory_Stimuli_Var`, TrialNum = trials.thisN, SoundPlayedTime = SoundStim.started, Response = key_resp.keys, ReactionTime = key_resp.rt)

# Create a new Child_ID to differentiate duplicates (date would be identical within the same recordings)
AA_merged2$Child_ID2 <- as.integer(as.factor(paste0(AA_merged2$Child_ID, AA_merged2$date)))

# Drop duplicates from the data until this issues gets fully resolves
AA_duplicate_df <- AA_merged2 %>% 
  select(Child_ID, Child_ID2) %>% 
  unique() %>%
  group_by(Child_ID) %>%
  summarise(count_Child_ID = length(Child_ID2)) %>%
  as.data.frame()

# Identify and remove duplicate IDs
AA_duplicate_IDs <- AA_duplicate_df$Child_ID[AA_duplicate_df$count_Child_ID > 1]
AA_merged2 <- AA_merged2 %>% filter(!Child_ID %in% AA_duplicate_IDs)

# Drop rows missing Stimulus (first trial)
AA_merged2 <- drop_na(AA_merged2, TrialNum)

# Cleaning the Stimulus variable
AA_merged2 <- AA_merged2 %>%
  mutate(Stimulus = gsub("Audio/|.wav", "", Stimulus))

# Add a trial type variable
AA_merged2 <- AA_merged2 %>%
  mutate(TrialType = ifelse(grepl("RED", Stimulus), "Target", "NonTarget"))

# Add correct response
AA_merged2 <- AA_merged2 %>%
  mutate(CorrectResponse = case_when(
    grepl("RED", Stimulus) ~ "v",
    TRUE ~ " "
  ))

# Remove the brackets from the responses
AA_merged2$Response <- gsub("\\[", "", AA_merged2$Response)
AA_merged2$Response <- gsub("\\]", "", AA_merged2$Response)

# Recording the total number of responses
AA_merged2$TotalRespNum <- sapply(str_split(AA_merged2$Response, ", "), function(x) length(x))
AA_merged2$TotalRespNum <- ifelse(AA_merged2$Response == "None", 0, AA_merged2$TotalRespNum)  

# Recording the total number of unique responses
AA_merged2$UniqueRespNum <- sapply(str_split(AA_merged2$Response, ", "), function(x) length(unique(x)))
AA_merged2$UniqueRespNum <- ifelse(AA_merged2$Response == "None", 0, AA_merged2$UniqueRespNum) 


# Scoring RED Target trials (SCORING IT DIFFERENTLY FROM THE MANUAL ON PURPOSE!!!)
# How is it different?
# We only care about what was reported for the single trial and not look into the subsequent trial response
# Why? It is messy and seems unnecessary, especially since the audio was slowed down and the children hands hover the keys
# Additionally, doing so would make counting the number of responses impossible

AA_merged2 <- AA_merged2 %>%
  mutate(
    # Scoring Target Trial Red
    Target_Correct = case_when(
      grepl("RED", Stimulus) & mapply(function(resp, corr) grepl(corr, resp, fixed = TRUE), Response, CorrectResponse) ~ 1, # If response for RED contains 'v', even if other answers given, then it's correct
      TRUE ~ 0
    ),
    # Counting the Number of Commission Errors (included in target trials)
    Commission_num = case_when(
      TrialType == "NonTarget" & Response != "None" ~ 1, # If a response is given on a non target then its a commission error
      grepl("RED", Stimulus) & Target_Correct != 1 & Response != "None" ~ 1, # If the wrong response was given to RED target then commission error
      grepl("RED", Stimulus) & Target_Correct == 1 & UniqueRespNum > 1 ~ 1, # If a wrong response was given with a correct response for RED target then commission error
      TRUE ~ 0),
    # Counting Number of Omission Errors
    Omission_num = case_when(
      TrialType == "Target" & grepl("None",Response) ~ 1, # If no response is given to a target trial then omission error
      TRUE ~ 0),
    # Counting the Number of Inhibitory Errors (Can't make an inhibitory error for red trials)
    Inhibitory_num = case_when(
      grepl("yellow", Stimulus) & Response != "None" ~ 1, # If a response was made on yellow then inhibition error
      grepl("blue", Stimulus) & Response != "None" ~ 1, # If a response was made on blue then inhibition error
      grepl("black", Stimulus) & Response != "None" ~ 1, # If a response was made on black then inhibition error
      TRUE ~ 0
    ))



# Quality Control the Data by looking for anything weird
names(AA_merged2)
AA_sum_scores <- AA_merged2 %>%
  group_by(Child_ID) %>%
  summarise(sum_Target_Correct = sum(Target_Correct),
            sum_Commission_Num = sum(Commission_num),
            sum_Ommission_Num = sum(Omission_num),
            Sum_Inhibitory_Num = sum(Inhibitory_num)) 

# Add task information
AA_sum_scores$Task <- "Auditory Attention"

library(patchwork)

# Max score should be 30
target_correct <- AA_sum_scores %>% 
  ggplot(aes(x= sum_Target_Correct)) +
  geom_boxplot() +
  coord_flip() + theme_classic()

# Max score should be 150 (Because word after Target included in the same trial)
Commission_Err <- AA_sum_scores %>%
  ggplot(aes(x= sum_Commission_Num)) +
  geom_boxplot() +
  coord_flip() + theme_classic()

# Max score should be 30
Ommission_Err <- AA_sum_scores %>%
  ggplot(aes(x= sum_Ommission_Num)) +
  geom_boxplot() +
  coord_flip() + theme_classic()

# Max score should be 35
Inhibitory_Err <- AA_sum_scores %>%
  ggplot(aes(x= Sum_Inhibitory_Num)) +
  geom_boxplot() +
  coord_flip() + theme_classic()

(target_correct + Commission_Err) / (Ommission_Err + Inhibitory_Err) +
  plot_annotation(title = "Total Sum of Correct Responses and Errors")





######
############### Scoring Response Set
######



# Extract full file names (set full.names = False, helps with for loop code)
AA_RS_file_names <- list.files(ResponseSet_location, pattern = ".csv")
RS_file_names <- AA_RS_file_names[grepl("Response",AA_RS_file_names)]

# Create empty lists
RS_files_list <- list()

# Load in all files for Response Set
for(ii in 1:length(RS_file_names)) {
  
  # Extract current file name ID
  current_file_name <- sub("_.*", "", RS_file_names[ii])
  
  # Read in current file
  current_read_file <- read.csv(paste0(ResponseSet_location, RS_file_names[ii]))
  
  # If file has no row information skip iteration
  if(dim(current_read_file)[1] == 0){
    next
  }
  
  # If they have 'Countdown_1.stopped' as a variable remove it
  if(grepl("Countdown_1.stopped", paste(names(current_read_file),collapse = " "))){
    current_read_file <- select(current_read_file, - Countdown_1.stopped)
  }
  
  # If they have the following variables (remove and target them as 'Yes' for practice)
  if(grepl("practice.started", paste(names(current_read_file), collapse = " "))){
    current_read_file <- current_read_file[, !names(current_read_file) %in% 
                                             c("thisRow.t", "notes", "practice.started", "practice.stopped",
                                               "WelcomeScreen.started", "WelcomeScreen.stopped", "AuditoryTrial.started",
                                               "AuditoryTrial.stopped", "expStart")]
    current_read_file$Practice <- "Yes"
  } else {
    current_read_file$Practice <- "No"
    
  }
  
  # Introduce the ID into the dataset
  current_read_file$Child_ID <- current_file_name
  
  #If they do not have 35 variables skip the iteration (either a test or incomplete session)
  if(dim(current_read_file)[2] != 35){
    next
  }
  
  # Save this information into the list
  RS_files_list[[ii]] <- current_read_file
  names(RS_files_list)[ii] <- current_file_name
}

# Combining them was successful
RS_merged <- unique(do.call(rbind, RS_files_list)) # Unique literally moves duplicate data


# Keep variables of interest (Cleaning the data)
RS_merged2 <- RS_merged %>%
  select(Child_ID, date, Stimulus = `X...Auditory_Stimuli_Var`, TrialNum = trials.thisN, SoundPlayedTime = SoundStim.started, Response = key_resp.keys, ReactionTime = key_resp.rt)

# Create a new Child_ID to differentiate duplicates (date would be identical within the same recordings)
RS_merged2$Child_ID2 <- as.integer(as.factor(paste0(RS_merged2$Child_ID, RS_merged2$date)))

# Drop duplicates from the data until this issues gets fully resolves
RS_duplicate_df <- RS_merged2 %>% 
  select(Child_ID, Child_ID2) %>% 
  unique() %>%
  group_by(Child_ID) %>%
  summarise(count_Child_ID = length(Child_ID2)) %>%
  as.data.frame()

# Identify and remove duplicate IDs
RS_duplicate_IDs <- RS_duplicate_df$Child_ID[RS_duplicate_df$count_Child_ID > 1]
RS_merged2 <- RS_merged2 %>% filter(!Child_ID %in% RS_duplicate_IDs)

# Drop rows missing Stimulus (first trial)
RS_merged2 <- drop_na(RS_merged2, TrialNum)

# Cleaning the Stimulus variable
RS_merged2 <- RS_merged2 %>%
  mutate(Stimulus = gsub("Audio/|.wav", "", Stimulus))

# Add a trial type variable
RS_merged2 <- RS_merged2 %>%
  mutate(TrialType = ifelse(grepl("BLUE|RED|YELLOW", Stimulus), "Target", "NonTarget"))

# Add correct response (Hear Red touch Yellow ('n'); Hear Yellow touch Red ('v'); Hear Blue touch Blue ('b'))
RS_merged2 <- RS_merged2 %>%
  mutate(CorrectResponse = case_when(
    grepl("RED", Stimulus) ~ "n",
    grepl("YELLOW", Stimulus) ~ "v",
    grepl("BLUE", Stimulus) ~ "b",
    TRUE ~ " "
  ))

# Remove the brackets from the responses
RS_merged2$Response <- gsub("\\[", "", RS_merged2$Response)
RS_merged2$Response <- gsub("\\]", "", RS_merged2$Response)

# Recording the total number of responses
RS_merged2$TotalRespNum <- sapply(str_split(RS_merged2$Response, ", "), function(x) length(x))
RS_merged2$TotalRespNum <- ifelse(RS_merged2$Response == "None", 0, RS_merged2$TotalRespNum)  

# Recording the total number of unique responses
RS_merged2$UniqueRespNum <- sapply(str_split(RS_merged2$Response, ", "), function(x) length(unique(x)))
RS_merged2$UniqueRespNum <- ifelse(RS_merged2$Response == "None", 0, RS_merged2$UniqueRespNum) 


# Scoring Target trials (SCORING IT DIFFERENTLY FROM THE MANUAL ON PURPOSE!!!)
# How is it different?
# We only care about what was reported for the single trial and not look into the subsequent trial response
# Why? It is messy and seems unnecessary, especially since the audio was slowed down and the children hands hover the keys
# Additionally, doing so would make counting the number of responses impossible

RS_merged2 <- RS_merged2 %>%
  mutate(
    # Scoring All Target Trials (RED, YELLOW, BLUE)
    Target_Correct = case_when(
      grepl("RED", Stimulus) & mapply(function(resp, corr) grepl(corr, resp, fixed = TRUE), Response, CorrectResponse) ~ 1, # If response for RED contains 'n' (yellow), even if other answers given, then it's correct
      grepl("YELLOW", Stimulus) & mapply(function(resp, corr) grepl(corr, resp, fixed = TRUE), Response, CorrectResponse) ~ 1, # If response for YELLOW contains 'v' (red), even if other answers given, then it's correct
      grepl("BLUE", Stimulus) & mapply(function(resp, corr) grepl(corr, resp, fixed = TRUE), Response, CorrectResponse) ~ 1, # If response for Blue contains 'b', even if other answers given, then it's correct
      TRUE ~ 0
    ),
    # Counting the Number of Commission Errors (included in target trials)
    Commission_num = case_when(
      TrialType == "NonTarget" & Response != "None" ~ 1, # If a response is given on a non target then its a commission error
      grepl("RED", Stimulus) & Target_Correct != 1 & Response != "None" ~ 1, # If the wrong response was given to RED target then commission error
      grepl("RED", Stimulus) & Target_Correct == 1 & UniqueRespNum > 1 ~ 1, # If a wrong response was given with a correct response for RED target then commission error
      grepl("YELLOW", Stimulus) & Target_Correct != 1 & Response != "None" ~ 1, # If the wrong response was given to YELLOW target then commission error
      grepl("YELLOW", Stimulus) & Target_Correct == 1 & UniqueRespNum > 1 ~ 1, # If a wrong response was given with a correct response for YELLOW target then commission error
      grepl("BLUE", Stimulus) & Target_Correct != 1 & Response != "None" ~ 1, # If the wrong response was given to BLUE target then commission error
      grepl("BLUE", Stimulus) & Target_Correct == 1 & UniqueRespNum > 1 ~ 1, # If a wrong response was given with a correct response for BLUE target then commission error
      TRUE ~ 0),
    # Counting Number of Omission Errors
    Omission_num = case_when(
      TrialType == "Target" & grepl("None",Response) ~ 1, # If no response is given to a target trial then omission error
      TRUE ~ 0),
    # Counting the Number of Inhibitory Errors (Can't make inhibitory error for Blue- Errors include: Responding for Black; Pressing Red for Red or Yellow for Yellow )
    Inhibitory_num = case_when(
      grepl("RED", Stimulus) & grepl("v",Response) ~ 1, # If they press red for red then inhibition error
      grepl("YELLOW", Stimulus) & grepl("n",Response) ~ 1, # If they press yellow for yellow then inhibition error
      grepl("black", Stimulus) & Response != "None" ~ 1, # If a response was made on black then inhibition error
      TRUE ~ 0
    ))

# Quality Control the Data by looking for anything weird
names(RS_merged2)
RS_sum_scores <- RS_merged2 %>%
  mutate(TargetType = case_when(
    grepl("RED", Stimulus) ~ "Red",
    grepl("YELLOW", Stimulus) ~ "Yellow",
    grepl("BLUE", Stimulus) ~ "Blue",
    TRUE ~ "NonTarget"
  )) %>%
  group_by(Child_ID, TargetType) %>%
  summarise(sum_Target_Correct = sum(Target_Correct),
            sum_Commission_Num = sum(Commission_num),
            sum_Ommission_Num = sum(Omission_num),
            Sum_Inhibitory_Num = sum(Inhibitory_num)) 

# Add behavioral information as a variable
RS_sum_scores$Task <- "Response Set"

library(patchwork)

# Max score should be 36, adding everything up
# Max for Red is 11
# Max for Yellow is 11
# Max for Blue is 14
RS_target_correct <- RS_sum_scores %>% 
  filter(TargetType != "NonTarget") %>%
  ggplot(aes(x = TargetType, y= sum_Target_Correct)) +
  geom_boxplot() +
  coord_flip() + theme_classic()

# Max score should be 143 (Because word after Target included in the same trial)
RS_Commission_Err <- RS_sum_scores %>%
  ggplot(aes(x = TargetType,y= sum_Commission_Num)) +
  geom_boxplot() +
  coord_flip() + theme_classic()

# Max score should be 36
RS_Ommission_Err <- RS_sum_scores %>%
  filter(TargetType != "NonTarget") %>%
  ggplot(aes(x = TargetType,y= sum_Ommission_Num)) +
  geom_boxplot() +
  coord_flip() + theme_classic()

# Max score should be 37 (Doesnt apply to Blue)
RS_Inhibitory_Err <- RS_sum_scores %>%
  filter(TargetType != "Blue") %>%
  ggplot(aes(x = TargetType,y= Sum_Inhibitory_Num)) +
  geom_boxplot() +
  coord_flip() + theme_classic()

(RS_target_correct + RS_Commission_Err) / (RS_Ommission_Err + RS_Inhibitory_Err) +
  plot_annotation(title = "Total Sum of Correct Responses and Errors (RS)")


# Save the datasets
write.xlsx(x= AA_sum_scores, file = save.pathwayAA)
write.xlsx(x= RS_sum_scores, file = save.pathwayRS)
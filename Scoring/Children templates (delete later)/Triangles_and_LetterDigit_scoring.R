# load in packages
library(tidyverse)
library(readxl)

# set working directory
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/Triangles_and_LetterDigit/raw_data")

# Import the data
file_names <- list.files()
files_to_be_imported <- list()

for(ii in 1:length(file_names)) {
  
  files_to_be_imported[[ii]] <- read_excel(file_names[[ii]])
  
}

Triangles_and_LetterDigit_uncleaned <- do.call(rbind,files_to_be_imported)

# Keep variables of interest
Triangles_uncleaned <- Triangles_and_LetterDigit_uncleaned %>% 
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation,
         paste("_",1:8,"_001",sep=""),
         paste("_",9:27,sep=""))

Triangles_long <- pivot_longer(Triangles_uncleaned, 
                            cols = starts_with("_"), 
                            names_to = "Question", 
                            values_to = "Score")


# Convert all non responses to 0's
Triangles_long$Score <- ifelse(Triangles_long$Score !=1, 0, 1)


# Clean DigitSpan
Numbers_forward <- Triangles_and_LetterDigit_uncleaned %>% 
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation,
         paste("_",1:8,sep=""),
         paste("_",1:8,"a",sep=""))

Numbers_backward <- Triangles_and_LetterDigit_uncleaned %>% 
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation,
         "_1_Trial_4_1",
         paste("_",1:8,"a_bw_001",sep=""),
         paste("_",2:8,"_bw_001",sep=""))

Letters_forward <- Triangles_and_LetterDigit_uncleaned %>% 
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation,
         "_1_Trial_f_c",
         paste("_",2:7,"_lf",sep=""),
         "_8_LF",
         paste("_",1:8,"a_lf",sep="")) 

Letters_backward <-  Triangles_and_LetterDigit_uncleaned %>% 
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation,
         "_1_Trial_d_g",
         paste("_",1:8,"a_bw",sep=""),
         paste("_",2:8,"_bw",sep="")) 

# Convert them to long
Numbers_forward_long <- pivot_longer(Numbers_forward, 
                                     cols = starts_with("_"), 
                                     names_to = "Question", 
                                     values_to = "Score")

Numbers_backward_long <- pivot_longer(Numbers_backward, 
                                     cols = starts_with("_"), 
                                     names_to = "Question", 
                                     values_to = "Score")

Letters_forward_long <- pivot_longer(Letters_forward, 
                                     cols = starts_with("_"), 
                                     names_to = "Question", 
                                     values_to = "Score")

Letters_backward_long <- pivot_longer(Letters_backward, 
                                     cols = starts_with("_"), 
                                     names_to = "Question", 
                                     values_to = "Score")

# Add a subtest variable
Numbers_forward_long$subtest <- rep("Numbers_forward", nrow(Numbers_forward_long))
Numbers_backward_long$subtest <- rep("Numbers_backward", nrow(Numbers_backward_long))
Letters_forward_long$subtest <- rep("Letter_forward", nrow(Letters_forward_long))
Letters_backward_long$subtest <- rep("Letters_backward", nrow(Letters_backward_long))

# Bind them together
DigitLetterSpan_data <- rbind(Numbers_forward_long,
                              Numbers_backward_long,
                              Letters_forward_long,
                              Letters_backward_long)

# data cleaning: If Score output is not 1 then convert it to 0
DigitLetterSpan_data$Score <- ifelse(DigitLetterSpan_data$Score == 1, 1, 0 )
# data cleaning: Convert all Na's to 0's
DigitLetterSpan_data$Score <- ifelse(is.na(DigitLetterSpan_data$Score), 0, DigitLetterSpan_data$Score)

# Create a table to show performance
DigitLetterSpan_data_long <- DigitLetterSpan_data %>% 
  group_by(Child_ID,subtest) %>%
  summarize(Sum_correct = sum(Score)) %>%
  data.frame()



# Convert from long to short format
DigitLetterSpan_scores <- pivot_wider(DigitLetterSpan_data_long, names_from = subtest, values_from = Sum_correct) %>% as_tibble

DigitLetter_scores <- DigitLetterSpan_scores %>%
  select(Child_ID,
         LF= Letter_forward,
         LB= Letters_backward,
         NF= Numbers_forward,
         NB= Numbers_backward)

Triangles_scores <- Triangles_long %>%
  group_by(Child_ID) %>%
  summarize(TR = sum(Score,na.rm = TRUE))

?sum()

# Save the data
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/Triangles_and_LetterDigit/final_data")
save(DigitLetter_scores, file= "LetterDigit.Rdata")
save(Triangles_scores, file= "Triangles.Rdata")

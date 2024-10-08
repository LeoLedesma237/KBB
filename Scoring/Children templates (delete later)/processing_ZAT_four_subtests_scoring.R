# Load in libraries
library(tidyverse)
library(readxl)

# Set working directory
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/ZAT/answer_keys")

# load in answer key
ZAT_RR_answer_key <- read_excel("ZAT_all_answer_keys.xlsx", sheet = "ZAT_RR")

ZAT_RC_answer_key <- read_excel("ZAT_all_answer_keys.xlsx", sheet = "ZAT_RC")

ZAT_M_answer_key <- read_excel("ZAT_all_answer_keys.xlsx", sheet = "ZAT_M")

ZAT_P_answer_key <- read_excel("ZAT_all_answer_keys.xlsx", sheet = "ZAT_P")

# set working directory
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/ZAT/raw_data")

# load in the data
ZAT_files <- list.files()
ZAT_files_list <- list()


for(ii in 1:length(ZAT_files)) {
  
  ZAT_files_list[[ii]] <- read_excel(ZAT_files[ii])  
  
}

ZAT <- do.call(rbind,ZAT_files_list)

ZAT <- unique(ZAT)

# Data cleaning: Variables of interest
ZAT_RR_questions <- c("_RR1",
                      paste("RR",2:20,sep=""),
                      "_21",
                      paste("RR",22:32,sep=""))

ZAT_RC_questions <- ZAT[,grepl("RC",names(ZAT))] %>% names()
ZAT_P_questions <- c(paste("A",1:5,sep=""),
                     paste("B",1:4,sep=""),
                     "B5_Correct_Response_j",
                     paste("C",1:7,sep=""),
                     paste("D",1:7,sep=""),
                     paste("E",1:7,sep=""),
                     paste("F",1:7,sep=""),
                     paste("H",1:7,sep=""),
                     paste("G",1:7,sep=""),
                     paste("J",1:7,sep=""))

ZAT_M_questions <- c(paste("M",3:16,sep=""),
                     "_M17",
                     paste("M",18:52,sep=""))

# Subset data into the corresponding ZAT tests
ZAT_RR <- ZAT %>%
  select(Child_ID, Evaluator_ID, Date_of_Evaluation, ZAT_RR_questions)

ZAT_RC <- ZAT %>%
  select(Child_ID,  Evaluator_ID, Date_of_Evaluation,ZAT_RC_questions)

ZAT_P <- ZAT %>%
  select(Child_ID, Evaluator_ID, Date_of_Evaluation,ZAT_P_questions)

ZAT_M <- ZAT %>%
  select(Child_ID, Evaluator_ID, Date_of_Evaluation,ZAT_M_questions)



# Take each ZAT and convert it into long form
ZAT_RR_long <- pivot_longer(ZAT_RR, 
                          cols = ZAT_RR_questions, 
                          names_to = "Question", 
                          values_to = "Answer")

ZAT_RC_long <- pivot_longer(ZAT_RC, 
                            cols = ZAT_RC_questions, 
                            names_to = "Question", 
                            values_to = "Answer")

ZAT_P_long <- pivot_longer(ZAT_P,
                              cols = ZAT_P_questions,
                              names_to = "Question",
                              values_to = "Answer")

ZAT_M_long <- pivot_longer(ZAT_M,
                           cols = ZAT_M_questions,
                           names_to = "Question",
                           values_to = "Answer")


## Add a subtest variable to each of the long subtests
ZAT_RR_long$subtest <- rep("Reading_Recognition",nrow(ZAT_RR_long))
ZAT_RC_long$subtest <- rep("Reading_Comprehension",nrow(ZAT_RC_long))
ZAT_P_long$subtest <- rep("PW_WR",nrow(ZAT_P_long))
ZAT_M_long$subtest <- rep("Math",nrow(ZAT_M_long))



###
### Scoring
###

# Change all letters from scoring into numbers in line with the answer key (must be capitalized)
char_to_num <- function(x) {
  if (x == "A") {
    return("1")
    
  } else if (x == "B") {
    return(2)
    
  } else if (x == "C") {
    return(3)
      
  } else if (x == "D") {
    return(4) 
}
}

ZAT_RR_answer_key$Answer <- lapply(ZAT_RR_answer_key$Answer_char,char_to_num) %>% unlist()
ZAT_RC_answer_key$Answer <- lapply(ZAT_RC_answer_key$Answer_char, char_to_num) %>% unlist()
ZAT_M_answer_key$Answer <- lapply(ZAT_M_answer_key$Answer_char, char_to_num) %>% unlist()
 
##
## Score everything before binding
##
unique_IDs_num <- unique(ZAT$Child_ID) %>% length()

# score RR
ZAT_RR_answer_key_long <- rep(ZAT_RR_answer_key$Answer,unique_IDs_num)
ZAT_RR_long$Correct <- ifelse(ZAT_RR_long$Answer ==ZAT_RR_answer_key_long, 1, 0)

# score RC
ZAT_RC_answer_key_long <- rep(ZAT_RC_answer_key$Answer,unique_IDs_num)
ZAT_RC_long$Correct <- ifelse(ZAT_RC_long$Answer ==ZAT_RC_answer_key_long, 1, 0)

# score P
ZAT_P_answer_key_long <- rep(ZAT_P_answer_key$Answer,unique_IDs_num)
ZAT_P_long$Correct <- ifelse(ZAT_P_long$Answer == ZAT_P_answer_key_long, 1, 0)

# score M
ZAT_M_answer_key_long <- rep(ZAT_M_answer_key$Answer,unique_IDs_num)
ZAT_M_long$Correct <- ifelse(ZAT_M_long$Answer == ZAT_M_answer_key_long, 1, 0)


ZAT_long <- rbind(ZAT_RR_long,
                  ZAT_RC_long,
                  ZAT_P_long,
                  ZAT_M_long)


# Replace NA's with 0's
ZAT_long$Correct <- ifelse(is.na(ZAT_long$Correct),0,ZAT_long$Correct)

# Create a table to show performance
Zat_scores_long <- ZAT_long %>% 
  group_by(Child_ID,subtest) %>%
  summarize(Sum_correct = sum(Correct)) %>%
  data.frame()



# Convert from long to short format
Zat_scores <- pivot_wider(Zat_scores_long, names_from = subtest, values_from = Sum_correct) %>% as_tibble

Zat_scores <- Zat_scores %>%
  select(Child_ID,
         MA = Math,
         PW = PW_WR,
         RC = Reading_Comprehension,
         RR = Reading_Recognition)

# Save these data
setwd("C:/Users/KBB DATA ENTRY/Desktop/KBB/2_behavioral_assessments/Children/ZAT/final_data")

save(Zat_scores, file="ZAT.Rdata")

#Zat_scores_test <- Zat_scores %>%
 # mutate(Composite_score = Math + PW_WR + Reading_Comprehension + Reading_Comprehension)

#model <- lm(Composite_score~Grade,Zat_scores_test)

#residual <- model$residuals

#scale(residual)
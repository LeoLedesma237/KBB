# This is the script for scoring ZAT Subtests
# We are starting from the Root Directory /KBB

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr) 

# Read in the file
ZAT <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Children/ZAT_Raw.xlsx"))

# Read in the three answer keys (ZAT_P is already scored)
ZAT_RR_answer_key <- read_excel(paste0(DataLocation,"RAW_DATA/AnswerKeys/ZAT_all_answer_keys.xlsx"), sheet = "ZAT_RR")
ZAT_RC_answer_key <-read_excel(paste0(DataLocation,"RAW_DATA/AnswerKeys/ZAT_all_answer_keys.xlsx"), sheet = "ZAT_RC")
ZAT_M_answer_key <- read_excel(paste0(DataLocation,"RAW_DATA/AnswerKeys/ZAT_all_answer_keys.xlsx"), sheet = "ZAT_M")

# Create a save pathway
save.pathway <- paste(DataLocation,"FINAL_DS/Behavioral/Children/ZAT.xlsx", sep="")


###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############


# Convert Answer choices into numeric responses
ZAT_RR_answer_key$Answer_num <-  ifelse(ZAT_RR_answer_key$Answer_char == "A", 1, 
       ifelse(ZAT_RR_answer_key$Answer_char == "B", 2,
              ifelse(ZAT_RR_answer_key$Answer_char == "C", 3, 4)))

ZAT_RC_answer_key$Answer_num <- ifelse(ZAT_RC_answer_key$Answer_char == "A", 1, 
       ifelse(ZAT_RC_answer_key$Answer_char == "B", 2,
              ifelse(ZAT_RC_answer_key$Answer_char == "C", 3, 4)))

ZAT_M_answer_key$Answer_num <- ifelse(ZAT_M_answer_key$Answer_char == "A", 1, 
       ifelse(ZAT_M_answer_key$Answer_char == "B", 2,
              ifelse(ZAT_M_answer_key$Answer_char == "C", 3, 4)))


# Inspect the data
glimpse(ZAT)
nrow(ZAT)

# Identify duplicate IDs
duplicate_IDs <- ZAT$Child_ID[duplicated(ZAT$Child_ID)]
ZAT <- ZAT %>% filter(!Child_ID %in% duplicate_IDs)
nrow(ZAT)

# Selecting variable names of interest
ZAT_RR_questions <- c("_RR1",
                      paste("RR",2:20,sep=""),
                      "_21",
                      paste("RR",22:32,sep=""))

ZAT_RC_questions <- ZAT[,grepl("RC",names(ZAT))] %>% names()

ZAT_M_questions <- c(paste("M",3:16,sep=""),
                     "_M17",
                     paste("M",18:52,sep=""))

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


# Subset data into the corresponding ZAT tests
ZAT_RR <- ZAT[, c(ZAT_RR_questions)]
ZAT_RC <- ZAT[, c(ZAT_RC_questions)]
ZAT_M <- ZAT[, c(ZAT_M_questions)]
ZAT_P <- ZAT[, c(ZAT_P_questions)]


# Rename the items to something prettier
names(ZAT_RR) <- paste0("RR_",1:length(ZAT_RR),"_RAW")
names(ZAT_RC) <- paste0("RC_",1:length(ZAT_RC),"_RAW")
names(ZAT_M) <- paste0("M_",1:length(ZAT_M),"_RAW")
names(ZAT_P) <- paste0("P_",1:length(ZAT_P),"_RAW")


# Data cleaning, convert all items from character into numeric
ZAT_RR <- data.frame(sapply(ZAT_RR, function(x) as.numeric(x)))
ZAT_RC <- data.frame(sapply(ZAT_RC, function(x) as.numeric(x)))
ZAT_M <- data.frame(sapply(ZAT_M, function(x) as.numeric(x)))
ZAT_P <- data.frame(sapply(ZAT_P, function(x) as.numeric(x)))

# Create a new scored dataframe for each subtest
ZAT_RR_scored <- as.data.frame(matrix(0, nrow = nrow(ZAT_RR), ncol = ncol(ZAT_RR)))
ZAT_RC_scored <- as.data.frame(matrix(0, nrow = nrow(ZAT_RC), ncol = ncol(ZAT_RC)))
ZAT_M_scored <- as.data.frame(matrix(0, nrow = nrow(ZAT_M), ncol = ncol(ZAT_M)))
ZAT_P_scored <- as.data.frame(matrix(0, nrow = nrow(ZAT_P), ncol = ncol(ZAT_P)))

# Score ZAT_RR
for (i in 1:ncol(ZAT_RR)) {
  ZAT_RR_scored[[i]] <- as.integer(ZAT_RR[[i]] == ZAT_RR_answer_key$Answer_num[i])
}

# Score ZAT_RC
for (i in 1:ncol(ZAT_RC)) {
  ZAT_RC_scored[[i]] <- as.integer(ZAT_RC[[i]] == ZAT_M_answer_key$Answer_num[i])
}

# Score ZAT_M
for (i in 1:ncol(ZAT_M)) {
  ZAT_M_scored[[i]] <- as.integer(ZAT_M[[i]] == ZAT_M_answer_key$Answer_num[i])
}

# Score ZAT P
for (i in 1:ncol(ZAT_P)) {
  ZAT_P_scored[[i]] <- as.integer(ZAT_P[[i]] == 1)
}

# Rename the scored datasets
names(ZAT_RR_scored) <- paste0("RR_",1:length(ZAT_RR_scored))
names(ZAT_RC_scored) <- paste0("RC_",1:length(ZAT_RC_scored))
names(ZAT_M_scored) <- paste0("M_",1:length(ZAT_M_scored))
names(ZAT_P_scored) <- paste0("P_",1:length(ZAT_P_scored))

# Create a dataframe of sum of correct responses
scored_df <- data.frame(scored_RR = rowSums(ZAT_RR_scored, na.rm = T),
                        scored_RC = rowSums(ZAT_RC_scored, na.rm = T),
                        scored_M = rowSums(ZAT_M_scored, na.rm = T),
                        scored_P = rowSums(ZAT_P_scored, na.rm = T))

# Merging all important variables into one final dataset
final_ZAT <- cbind(select(ZAT, Child_ID:Evaluator_ID),
                   ZAT_RR, 
                   ZAT_RC,
                   ZAT_M,
                   ZAT_P,
                   ZAT_RR_scored,
                   ZAT_RC_scored,
                   ZAT_M_scored,
                   ZAT_P_scored,
                   scored_df)


# Save the scored data
write.xlsx(x= final_ZAT, file = save.pathway)

# Make a note that the data was saved successfully
cat("Saving processed ZAT 4 Subtests\n")

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation"))])

# Set a pause time for 1 second
Sys.sleep(1)


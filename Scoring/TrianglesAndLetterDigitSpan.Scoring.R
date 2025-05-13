# This is the script for Triangles and Letter & Digit Span

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr) # str_count()

# Read in the file
Triangles_LettrDig <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Children/Triangles_LettrDig_Raw.xlsx"))

# Read in Age
Demo <- read_excel(paste0(DataLocation, "FINAL_DS/Demographics/Demographics.xlsx"))

# Create a save pathway for cleaned and scored Triangles data
save.pathway_TR <- paste(DataLocation,"FINAL_DS/Behavioral/Children/Triangles.xlsx", sep="")

# Create a save pathway for cleaned and scored Letter and Digit Span data
save.pathway_LD <- paste(DataLocation,"FINAL_DS/Behavioral/Children/LettrDig.xlsx", sep="")

# Create a save pathway for the outcome of the ID reports
save.pathway.notes <- paste(DataLocation,"REPORTS/Individual/Triangles_LettrDig_Notes.csv", sep="")


###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############


# Check the IDs for errors
source("Scoring/scoring_functions/IDError_FUNCTION.R")
TR_Notes <-check_id_errors("Triangles and LettrDig",
                           Triangles_LettrDig$Child_ID)


# Precursor Scoring for Triangles (Adding bonus points or counting items wrong for taking too long)
# THis only applies to children 7-18
# introduce Age to separate dataset
Triangles_LettrDig2 <- Triangles_LettrDig %>%
  left_join(Demo, by = "Child_ID")

# Drop any duplicate ids
(duplicate_IDs <- Triangles_LettrDig2$Child_ID[duplicated(Triangles_LettrDig2$Child_ID)])
Triangles_LettrDig3 <- Triangles_LettrDig2 %>% filter(!Child_ID %in% duplicate_IDs)

# Separate the datasets by Age grouping (based on authors scoring recommendations)
Triangles3_6 <- filter(Triangles_LettrDig3, Age <= 6)
Triangles7_18 <- filter(Triangles_LettrDig3, Age >= 7)

# Load in functions to score Triangles7_18
source("Scoring/scoring_functions/Scoring_triangles_7_18_FUNCTION.R")

##
##### Part 1: Scoring items 11-16 in Triangles_7_18 (one time threshol)
##
# Set parameters for function1 scoring
items1 <- select(Triangles7_18, paste0("_",11:16))
times1 <- select(Triangles7_18, paste0("Time_s_00",1:6))
threshold1 <- 30

# Use mapply to run the function with multiple arguments
scores_11_16 <- mapply(
    FUN = time_scoring_fun1,
    item = items1,
    time = times1,
    threshold1 = threshold1
  )

# minor data cleaning
scores_11_16 <- sapply(data.frame(scores_11_16), function(x) as.numeric(x)) %>% data.frame()
names(scores_11_16) <- names(items1)

# Quality Control (This works as intended)
#cbind(items1, times1, scores_11_16) %>% view() 

##
##### Part 2: Scoring items 17-22 in Triangles_7_18 (two time thresholds)
##
# Set parameters for function2 scoring
items2 <- select(Triangles7_18, paste0("_",17:22))
times2 <- select(Triangles7_18, c(paste0("Time_s_00",7:9), paste0("Time_s_01",0:2)))
threshold2 <- list(times1 = c(20,45), times2= c(20,45), times3 = c(15,45), 
                   times4 = c(15,45), times5= c(15,45), times6= c(20,45))


# Use mapply to run the function with multiple arguments
scores_17_22 <- Map(
  function(item_col, time_col, thresh) {
    mapply(time_scoring_fun2, item = item_col, time = time_col, MoreArgs = list(thresh = thresh))
  },
  items2,
  times2,
  threshold2
)

# minor data cleaning
scores_17_22 <- do.call(cbind, scores_17_22)
scores_17_22 <- sapply(data.frame(scores_17_22), function(x) as.numeric(x)) %>% data.frame()
names(scores_17_22) <- names(items2)

# Quality Control (This works as intended)
# cbind(items2, times2, scores_17_22) %>% view()


##
##### Part 3: Scoring items 23-27 in Triangles_7_18 (three time thresholds)
##
# Set parameters for function3 scoring
items3 <- select(Triangles7_18, paste0("_",23:27))
times3 <- select(Triangles7_18, paste0("Time_s_01",3:7))
threshold3 <- list(times1 = c(20,40,90), times2= c(20,35,90), times3 = c(20,50,105), 
                   times4 = c(30,50,105), times5= c(35,50,105))


# Use mapply to run the function with multiple arguments
scores_23_27 <- Map(
  function(it, tm, thr) {
    mapply(time_scoring_fun3, item = it, time = tm, MoreArgs = list(thr = thr))
  },
  items3,
  times3,
  threshold3
)


# minor data cleaning
scores_23_27 <- do.call(cbind, scores_23_27)
scores_23_27 <- sapply(data.frame(scores_23_27), function(x) as.numeric(x)) %>% data.frame()
names(scores_23_27) <- names(items3)

# Quality Control (This works as intended)
# cbind(items3, times3, scores_23_27) %>% view()


##
##### Part 4: Reconstructing Triangles dataset to include scores from all ages
##

# cbind all of the updated scored items 
updated_scores <- cbind(scores_11_16, 
                       scores_17_22, 
                       scores_23_27)

# Overwrite the original item scores with these updated ones
Triangles7_18[ , names(updated_scores)] <- updated_scores[ , names(updated_scores)]

# Bind Triangles back into one dataset (3-6 and 7-18)
Triangles_allAges <- rbind(Triangles3_6, Triangles7_18)


##
##### Part 5: Business as usual (except there are two datasets now)
##


# Select Vars of Interest
Front <- Triangles_LettrDig %>%
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation)

Front_Triangles <- Triangles_allAges %>%
  select(Child_ID,
         Evaluator_ID,
         Date_of_Evaluation)

TR.Items <- Triangles_allAges %>%
  select(paste("_",1:8,"_001",sep=""),
         paste("_",9:27,sep=""))


NF.Items <- Triangles_LettrDig %>%
  select(`_1_Trial_1_4`:`_8a`)


NB.Items <- Triangles_LettrDig %>%
  select(`_1_Trial_4_1`:`_8a_bw_001`)


LF.Items <- Triangles_LettrDig %>%
  select(`_1_Trial_f_c`:`_8a_lf`)


LB.Items <- Triangles_LettrDig %>%
  select(`_1_Trial_d_g`:`_8a_bw`)

# Renaming the Items (Part 1)
names(TR.Items) <- 1:27
names(NF.Items) <- 1:16  
names(NB.Items) <- 1:16 
names(LF.Items) <- 1:16 
names(LB.Items) <- 1:16

# Create a list for all of the Items and their respective stop rules and names
Items.List <- list(TR.Items, NF.Items, NB.Items, LF.Items, LB.Items)
StopRules <- c(5,4,4,4,4)
new.names <- c("TR", "NF", "NB", "LF", "LB")



# Call the function to score Letter and Digit
source("Scoring/scoring_functions/Scoring_FUNCTION1.R")

# Create a list to save the scored data
scoredItems_list <- list()

for(ii in 1:length(Items.List)) {
 
  # Save the current Item list
  current_ItemList <- Items.List[[ii]]
  
  # Rename items before they are scored 
  names(current_ItemList) <- paste0(new.names[ii],"_", names(current_ItemList))
  
  # Score the Letter and Digit Items
  scoredItems<- scoring_function1(current_ItemList, StopRules[ii], new.names[ii])
  
  # Save these scored and cleaned datasets
  scoredItems_list[[ii]] <- scoredItems
}


# Create the final datasets
Triangles <- data.frame(cbind(Front_Triangles), scoredItems_list[[1]])

LetterDig <- data.frame(cbind(Front), scoredItems_list[[2]], 
                        scoredItems_list[[3]], scoredItems_list[[4]], 
                        scoredItems_list[[5]])

# Have a composite scores for all Letter and Digit subtest
LetterDig <- LetterDig %>%
  mutate(LetDig_CDK = NF_CDK + NB_CDK + LF_CDK + NB_CDK,
         LetDig_NRG = NF_NRG + NB_NRG + LF_NRG + NB_NRG,
         LetDig_NA.Num = NF_NA_num + NB_NA_num + LF_NA_num + NB_NA_num,
         LetDig_Performance = NF_Performance + NB_Performance + LF_Performance + NB_Performance)

# Save the scored data
write.xlsx(x= Triangles, file = save.pathway_TR)
write.xlsx(x= LetterDig, file = save.pathway_LD)

# Save the Notes as a CSV
write_csv(x = TR_Notes, save.pathway.notes)


# Make a note that the data was saved successfully
cat("Saving processed Triangles\n")
cat("Saving processed Letter and Digit Span\n")

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation"))])


# Set a pause time for 1 second
Sys.sleep(1)
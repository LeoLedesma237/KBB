# This script will investigating school attendance among those given IDs in our study

# Load in packates
library(psych)
library(readxl)
library(tidyverse)
library(ggplot2)
library(kableExtra)
library(openxlsx)

# Set working directory and load data of interest
setwd("~/KBB/Data/FINAL_DS/Screener/Matched_Siblings")
ID_df_org <- read_excel("Final_ID_Tracker.xlsx", col_types = "text")
setwd("~/KBB/Data/RAW_DATA/Behavioral/Adults")
Home_Env <- read_excel("HomeEnv_Raw.xlsx")
setwd <- setwd("~/KBB/Data/FINAL_DS/Demographics")
demo <- read_excel("Demographics.xlsx")

# IDs present in datasets
cat("There are", length(unique(ID_df_org$ID)),"unique IDs in the Final Tracker ID")
cat("There are", length(unique(Home_Env$Child_1_ID)) + length(unique(Home_Env$Child_2_ID)),"unique IDs in the Home Environment")

# Minor data cleaning for ID Tracker
ID_df_org <- select(ID_df_org, HOH_ID, ID, Relatedness_ID)
ID_df_org <- rename(ID_df_org, Child_ID = ID)
ID_df_org$Child_ID <- as.numeric(ID_df_org$Child_ID)
ID_df_org <- ID_df_org %>%
  mutate(Relatedness = case_when(
    Relatedness_ID == 1 ~ "Proband",
    Relatedness_ID == 2 ~ "Sibling",
    Relatedness_ID == 3 ~ "Half_Sibling",
    Relatedness_ID == 4 ~ "Cousin"
  ))

# Data cleaning on Home Environment
#Home_Env <- rename(Home_Env, Child_ID = Child_1_ID)
Home_Env_Child1_items <- select(Home_Env, Child_1_ID, `_11_How_old_was_the_e_she_started_school`:`_21b_If_yes_please_list_at_what_age`)
Home_Env_Child2_items <- select(Home_Env, Child_2_ID, `_11_How_old_was_the_e_she_started_school_001`:`_21b_If_yes_please_list_at_what_age_001`)

# Data inspection
cat("There are",nrow(Home_Env_Child1_items), "rows in Hom_Env_Child_1 and", nrow(Home_Env_Child2_items),"rows in Home_Env_Child_2")
cat("There are",length(unique(Home_Env_Child1_items$Child_1_ID)), "unique IDs for Child_1 and",length(unique(Home_Env_Child2_items$Child_2_ID)), "unique IDs for Child_2")

# Rename the variables
names(Home_Env_Child1_items) <- c("Child_ID","school_started_age", "school_years_attended", "highest_grade_achieved", "attending_school",
                     "why_not_attending_school", "ever_enrolled_in_school", "missing_school_period_sick", "missing_school_period_work",
                     "time_doing_HW", "problems_at_school", "what_problems", "skipped_grade", "what_grade_skipped", "what_age_grade_skipped",
                     "repeated_grade", "which_repeated_grade", "what_repeated_grade_age")

names(Home_Env_Child2_items) <- c("Child_ID","school_started_age", "school_years_attended", "highest_grade_achieved", "attending_school",
                                  "why_not_attending_school", "ever_enrolled_in_school", "missing_school_period_sick", "missing_school_period_work",
                                  "time_doing_HW", "problems_at_school", "what_problems", "skipped_grade", "what_grade_skipped", "what_age_grade_skipped",
                                  "repeated_grade", "which_repeated_grade", "what_repeated_grade_age")

# Bind the two datasets into one
Home_Env_items2 <- rbind(Home_Env_Child1_items, Home_Env_Child2_items)
cat("We have data (not including NAs) for", length(unique(Home_Env_items2$Child_ID)),"unique IDs in the Home Environment Questionnaire")

# Minor data cleaning for demographics
demo <- select(demo, Child_ID, Sex, Age)

# Identify and remove duplicated IDs
dup_id_HE <- Home_Env_items2$Child_ID[duplicated(Home_Env_items2$Child_ID)]
Home_Env_items2 <- Home_Env_items2 %>% filter(! Child_ID %in% dup_id_HE)
cat(length(dup_id_HE),"children were dropped from Home Environment for having Duplicate IDs")
dup_demo <- demo$Child_ID[duplicated(demo$Child_ID)]
demo <- demo %>% filter(! Child_ID %in% dup_demo)
cat(length(dup_demo),"children were dropped from demo for having Duplicate IDs")

# Add demographics information into the ID_df
ID_df <- ID_df_org %>% left_join(demo, by = "Child_ID")

# Descriptive information from Final IDs
cat("There are ",length(ID_df$Child_ID), " children have been given an ID")
cat(" Here is a table of the sex distribution:")
table(ID_df$Sex)
cat(" Here are the probands and their controls and how distance they are genetically:")
table(ID_df$Relatedness)


########
########## Intense data cleaning of Home Environment Values
#######
# Q1 - Q3 are okay as continuous (but a few wrong answers)
# Q4 Categorical can be changed into proportion (remove NAs first)
# Q5 Is a follow up to Q4 but has too many responses
# Q6 Categorical can be changed into proportion (remove NAs first)
# Q7 Categorical has 7 unique responses needs to be changed into meaningful values
# Q8 Categorical has 5 unique responses needs to be changed into meaningful values
# Q9 Has many responses but can be cleaned- should be converted into time
# Q10 Categorical can be changed into proportion (remove NAs first)
# Q11 Is a follow up to Q10 but has too many responses
# Q12 Categorical can be changed into proportion (remove NAs first)
# Q13 Follow up to Q12 very few responses in general
# Q14 Categorical can be changed into proportion (remove NAs first)
# Q15 Follow up to Q14 (needs to have an outlier removed)
# Q16 Follow up to Q14
sapply(select(Home_Env_items2, -Child_ID), function(x) table(x, useNA = "ifany"))

# Begin cleaning here
ID_df2 <- ID_df %>% left_join(Home_Env_items2, by = "Child_ID")
ID_df2 <- ID_df2 %>% mutate(Relatedness = factor(Relatedness, levels = c("Proband", "Sibling", "Half_Sibling", "Cousin")))
ID_df2 <- ID_df2 %>% mutate(ever_enrolled_in_school = 
                              case_when(ever_enrolled_in_school == "no" ~ "N",
                                        ever_enrolled_in_school == "yes" ~ "Y",
                                        TRUE ~ ever_enrolled_in_school))

ID_df2 <- ID_df2 %>% 
  mutate(missing_school_period_sick2 = 
           # Change it into 
           case_when(missing_school_period_sick == "dk" ~ "Dont_Know",
                     missing_school_period_sick == "nev" ~ "Never",
                     missing_school_period_sick == "1_3" ~ "1-3 Days",
                     missing_school_period_sick == "wk" ~ "A week",
                     missing_school_period_sick %in% c("mo", ">mo", "yr") ~ "A Month of Longer",
                     TRUE ~ missing_school_period_sick))
ID_df2 <- ID_df2 %>% 
  mutate(missing_school_period_work2 = 
           #Change into
           case_when(missing_school_period_work == "dk" ~ "Dont_Know",
                     missing_school_period_work == "nev" ~ "Never",
                     missing_school_period_work == "1_3" ~ "1-3 Days",
                     missing_school_period_work == "wk" ~ "A week",
                     missing_school_period_work %in% c("mo", ">mo", "yr") ~ "A Month of Longer",
                     missing_school_period_work == "n/a" ~ "Not Applicable",
                     TRUE ~ missing_school_period_work))

ID_df2 <- ID_df2 %>%
  mutate(time_doing_HW = ifelse(time_doing_HW == "1o", "10", time_doing_HW),
         time_doing_HW = ifelse(grepl("No", time_doing_HW, ignore.case = TRUE), 0, time_doing_HW),
         hw_weight = ifelse(grepl("hour|hr", time_doing_HW), 60, 1),
         time_doing_HW_num = as.numeric(gsub("\\D", "", time_doing_HW)),
         time_doing_HW2 = hw_weight * time_doing_HW_num)

ID_df2 <- ID_df2 %>%
  mutate(problems_at_school = 
           case_when(problems_at_school == "Dk" ~ "DK",
                     problems_at_school == "n/a" ~ "N/A",
                     TRUE ~ problems_at_school))

ID_df2 <- ID_df2 %>%
  mutate(skipped_grade = 
           case_when(skipped_grade == "Dk" ~ "DK",
                     skipped_grade == "n/a" ~ "N/A",
                     TRUE ~ skipped_grade))

ID_df2 <- ID_df2 %>%
  mutate(repeated_grade = 
           case_when(repeated_grade == "don_t_know" ~ "DK",
                     repeated_grade == "no" ~ "N",
                     repeated_grade == "yes" ~ "Y",
                     TRUE ~ repeated_grade))



# Let's investigate schooling situation for each child and separate it by proband and ctrl category
# Select variables of interest to investigate
ID_df3 <- ID_df2 %>%
  select(Relatedness, school_started_age:attending_school, ever_enrolled_in_school, missing_school_period_sick2, 
         missing_school_period_work2, time_doing_HW2, problems_at_school, skipped_grade, repeated_grade)

# Remove some outliers real quick
ID_df3 <- ID_df3 %>%
  filter(school_years_attended <= 13 & highest_grade_achieved <= 12) # Removing bad data

#####
###### Run a for loop to obtain descriptives for each variable
#####

Results_cont_list <- list()
count_0 <- list()
Results_per_list <- list()
non_Na_cont_list <- list()
non_Na_per_list <- list()
Na_cont_list <- list()
Na_per_list <- list()

for(ii in 2:length(ID_df3)){ # I did this for a reason but forgot why but it works
  tryCatch({
    # Isolate the current variable
    cv <- unlist(ID_df3[,ii])
    cdf <- data.frame(cbind(ID_df3[,1], cv))
    #cdf <- cdf %>% filter(complete.cases(.))
    
    # Check whether this variable is numeric or categorical
    if(is.numeric(cv)) {
      count_0[[ii]] <- sum(cdf$cv == 0, na.rm = T)
      if(ii != 9) { ;cdf <- cdf[cdf$cv != 0,]} # Do not apply to HW time
      num_des <- data.frame(do.call(rbind, (describeBy(cdf$cv, cdf$Relatedness))))
      Results_cont_list[[ii]] <- round(select(num_des, mean, sd, n, min, max),1)
      Na_cont_list[[ii]] <- sum(is.na(cdf$cv))
      non_Na_cont_list[[ii]] <- sum(!is.na(cdf$cv))
      
    } else {
      per_Tab <- as.data.frame.matrix(round(prop.table(table(cdf$cv, cdf$Relatedness), margin = 2) * 100, 2))
      num_Tab <- as.data.frame.matrix(table(cdf$cv, cdf$Relatedness))
      Results_per_list[[ii]] <- cbind(num_Tab, sapply(per_Tab, function(x) paste0(x,"%")))
      Na_per_list[[ii]] <- sum(is.na(cdf$cv))
      non_Na_per_list[[ii]] <- sum(!is.na(cdf$cv))
    }
  }, warning = function(w) {
    cat("Warning in iteration:", ii, "Column:", colnames(ID_df3)[ii], "\n")
    cat("Warning message:", conditionMessage(w), "\n\n")
  })
}


# Remove list elements that are null
Results_cont_list <- Results_cont_list[!sapply(Results_cont_list, is.null)]
count_0 <-  count_0[!sapply(count_0, is.null)]
Results_per_list <- Results_per_list[!sapply(Results_per_list, is.null)]
non_Na_cont_list <- non_Na_cont_list[!sapply(non_Na_cont_list, is.null)]
non_Na_per_list <- non_Na_per_list[!sapply(non_Na_per_list, is.null)]
Na_cont_list <- Na_cont_list[!sapply(Na_cont_list, is.null)]
Na_per_list <- Na_per_list[!sapply(Na_per_list, is.null)]

# Some Additionally data cleaning before generating tables (Change row order)
desired_order1 <- c("Never", "1-3 Days", "A week", "A Month of Longer", "Dont_Know")
desired_order2 <- c("Not Applicable", "Never", "1-3 Days", "A week")

tab3 <- Results_per_list[[3]]
tab4 <- Results_per_list[[4]]

Results_per_list[[3]] <- tab3[desired_order1, ]
Results_per_list[[4]] <- tab4[desired_order2, ]

# Write down the questions to use them as titles
Results_cont_names <-  c("At what age did your child start school?", 
                         "How many years did your child attend school?",
                         "What is the highest grade your child has completed?",
                         "How many minutes per day does your child spend doing HW?")

Results_per_names <-  c("Is the child currently attending school?",
                        "Has the child ever been enrolled in school?",
                        "The longest period of time the child missed school for being sick?",
                        "The longest period of time the child missed school for being need for working at home?",
                        "Has the child had any problems at school?",
                        "Has the child skipped a grade?",
                        "Has the child repeated a grade?")
  
  

# Print the continuous tables
cont_tables <- list()
for(ii in 1:length(Results_cont_list)) {
  if(ii <= 3) {
  cont_tables[[ii]] <- Results_cont_list[[ii]] %>%
    kbl(caption = Results_cont_names[[ii]]) %>%
    kable_classic(full_width = F) %>%
    footnote(general = paste0("Above we have responses from ",non_Na_cont_list[[ii]]," IDs. Data from ",Na_cont_list[[ii]]," IDs are missing. Additionally, data from ",count_0[[ii]]," IDs were removed from the calculation above for reporting the value 0."))
  } else {
    cont_tables[[ii]] <- Results_cont_list[[ii]] %>%
      kbl(caption = Results_cont_names[[ii]]) %>%
      kable_classic(full_width = F) %>%
      footnote(general = paste0("Above we have responses from ",non_Na_cont_list[[ii]]," IDs. Data from ",Na_cont_list[[ii]]," IDs are missing"))
  }
}
# cont_tables

# Print the categorical tables
cat_tables <- list()
for(ii in 1:length(Results_per_list)) {
  cat_tables[[ii]] <- Results_per_list[[ii]] %>%
    kbl(caption = Results_per_names[[ii]]) %>%
    kable_classic(full_width = F) %>%
    footnote(general = paste0("Above we have responses from ",non_Na_per_list[[ii]]," IDs. Data from ",Na_per_list[[ii]]," IDs are missing"))
  
}
# cat_tables

# Identify the number of Probands and Ctrls for Home Environment (after comining data across the questionnaire)
Home_Env_kiddos <- Home_Env_items2 %>%
  left_join(ID_df_org, by = "Child_ID")

table(Home_Env_kiddos$Relatedness)

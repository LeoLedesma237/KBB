library(tidyverse)
library(readxl)
library(lubridate)


# Set working directory to import data
setwd("~/KBB_new_2/1_screener/raw_data")

# Load in CFM2_4 files
CFM5_17.uncleaned <- read_excel(list.files(pattern = "5_18"))

# Remove any redundant rows
CFM5_17.uncleaned <- unique(CFM5_17.uncleaned)

CFM5_17.removed.variables <- CFM5_17.uncleaned %>%
  select(GPS.lat = `_GPS_latitude`,
         GPS.long = `_GPS_longitude`,
         Date_of_Evaluation = start,
         Evaluator_ID,
         Name_of_the_Village,
         Location_Type,
         HOH_First_Name,
         HOH_Last_Name,
         HOH_Date_of_Birth = HOH_Date_of_birth,
         Respondant_First_Name,
         Respondant_Last_Name,
         Child_First_Name,
         Child_Last_Name,
         Child_Gender,
         Child_Date_of_Birth,
         BF = Child_s_Biological_Father,
         BM = Child_s_Biological_Mother,
         Respondant_relationship = `_08_Respondent_Relationship_to`,
         CF1,
         CF2,
         CF3,
         CF4,
         CF5,
         CF6,
         CF7,
         CF8,
         CF9,
         CF10,
         CF11,
         CF12,
         CF13,
         CF14,
         CF15,
         CF16,
         CF17,
         CF18,
         CF19,
         CF20,
         CF21,
         CF22,
         CF23,
         CF24,
         E1 = `_25_Mumwaka_wamana_akakozyeka_kulesegwa`,
         E2 = `_26_Mmwaka_wamana_m_uwa_akucinca_cikanda`,
         E3 = `_27_Mumwaka_wamana_m_oyu_kuli_na_nyongene`,
         E4 = `_28_Mumwaka_wamana_kazilukide_kumizeezo`,
         E5 = `_29_Mumwaka_wamana_akuwa_akuluma_mulaka`,
         E6 = `_30_Mumwaka_wamana_ilwa_kulijata_kusuba`,
         E7 = `_31_Mumwaka_wamana_umaulu_nokuba_kumeso`,
         E8 = `_32_Mumwaka_wamana_limwi_akuvwa_kununka`,
         E9 = `_33_Mumwaka_wamana_idwe_bulwazi_bwakuwa`,
         EN10 = `_34_Eeli_penzi_lyaka_na_buyo_ciindi_comwe`,
         EN11 = `_35_Ipenzi_eeli_lyak_o_wakalivwide_ampeyo`,
         EN12 = `_36_Ipenzi_eeli_lyak_e_mumalo_kucibbadela`)


# Cleaning variables with date data 
CFM5_17.cleaned.date <- CFM5_17.removed.variables %>%
  mutate(Date_of_Evaluation = substr(Date_of_Evaluation, start = 1, stop = 10),
         HOH_Date_of_Birth = substr(HOH_Date_of_Birth, start = 1, stop = 10),
         Child_Date_of_Birth = substr(Child_Date_of_Birth, start = 1, stop = 10))

# Creating the variable age
dob <- ymd(CFM5_17.cleaned.date$Child_Date_of_Birth)
doe <- ymd(CFM5_17.cleaned.date$Date_of_Evaluation)

age_weeks <- as.numeric(difftime(doe, dob, units = "weeks"))
age <- age_weeks/52
CFM5_17.cleaned.date$Child_age <- round(age,1)


CFM5_17.HOH.ID <- CFM5_17.cleaned.date %>%
  mutate(HOH_ID = paste(HOH_First_Name, 
                        HOH_Last_Name, 
                        HOH_Date_of_Birth),
         Child_ID = paste(Child_First_Name,
                          Child_Last_Name,
                          Child_Date_of_Birth,
                          Child_Gender))


CFM5_17.HOH.ID <- CFM5_17.HOH.ID %>%
  mutate(glasses = ifelse(CF1 == 1, "Yes", "No"),
         hearing.aid = ifelse(CF4 == 1, "Yes", "No"),
         walking.equipment = ifelse(CF7 == 1, "Yes", "No"))

# Create a function to change numeric values to the associate difficulty severity
difficulty_type_fun <- function(value) {
  case_when(
    value == 1 ~ "No difficulty",
    value == 2 ~ "Some Difficulty",
    value == 3 ~ "A lot of Difficulty",
    value == 4 ~ "Cannot at all",
    TRUE ~ as.character(value)
  )
}

# This is for the mental health related questions 
mental_health_freq_fun <- function(value) {
  case_when(
    value == 1 ~ "Daily",
    value == 2 ~ "Weekly",
    value == 3 ~ "Monthly",
    value == 4 ~ "A few times a year",
    value == 0 ~ "Never",
    TRUE ~ as.character(value)
  )
}

# Create new variables with these labels
CFM5_17.CFM.labeled <- CFM5_17.HOH.ID %>%
  mutate(CF3_Seeing = difficulty_type_fun(CF3),
         CF6_Hearing = difficulty_type_fun(CF6),
         CF12_Walking_100 = difficulty_type_fun(CF12),
         CF13_Walking_500 = difficulty_type_fun(CF13),
         CF14_Self_care = difficulty_type_fun(CF14),
         CF15_Understood_Inside = difficulty_type_fun(CF15),
         CF16_Understood_Outside = difficulty_type_fun(CF16),
         CF17_Learning = difficulty_type_fun(CF17),
         CF18_Remembering = difficulty_type_fun(CF18),
         CF19_Concentrating = difficulty_type_fun(CF19),
         CF20_Accepting_Challenge = difficulty_type_fun(CF20),
         CF21_Controlling_Behavior = difficulty_type_fun(CF21),
         CF22_Making_Friends = difficulty_type_fun(CF22),
         CF23_Anxiety = mental_health_freq_fun(CF23),
         CF24_Depression = mental_health_freq_fun(CF24)) 


# Weird ChatGPT function
CFM_opr_fun <- function(...) {
  # Use pmap to iterate over rows
  pmap_chr(list(...), function(...) {
    if ("Cannot at all" %in% c(...)) {
      return("Cannot at all")
      
    } else if ("A lot of Difficulty" %in% c(...)) {
      return("A lot of Difficulty")
      
    } else if ("Some Difficulty" %in% c(...)) {
      return("Some Difficulty")
      
    } else {
      return("No difficulty")
    }
  })
}

# Weird ChatGPT function
mental_health_opr_fun <- function(...) {
  # Use pmap to iterate over rows
  pmap_chr(list(...), function(...) {
    if ("Daily" %in% c(...)) {
      return("Daily")
      
    } else if ("Weekly" %in% c(...)) {
      return("Weekly")
      
    } else if ("Monthly" %in% c(...)) {
      return("Monthly")
      
    } else if ("A few times a year" %in% c(...)) {
      return("A few times a year")
      
    } else {
      return("Never")
    }
  })
}

CFM5_17 <- CFM5_17.CFM.labeled %>%
  mutate(CFM_DD = CFM_opr_fun(CF3_Seeing, 
                              CF6_Hearing, 
                              CF12_Walking_100, 
                              CF13_Walking_500,
                              CF14_Self_care,
                              CF15_Understood_Inside,
                              CF16_Understood_Outside,
                              CF17_Learning,
                              CF18_Remembering,
                              CF19_Concentrating,
                              CF20_Accepting_Challenge,
                              CF21_Controlling_Behavior,
                              CF22_Making_Friends))

CFM5_17 <- CFM5_17 %>%
  mutate(KBB_CFM_DD = CFM_opr_fun(CF14_Self_care,
                                  CF15_Understood_Inside,
                                  CF16_Understood_Outside,
                                  CF17_Learning,
                                  CF18_Remembering,
                                  CF19_Concentrating,
                                  CF20_Accepting_Challenge,
                                  CF21_Controlling_Behavior,
                                  CF22_Making_Friends))


CFM5_17 <- CFM5_17 %>%
  mutate(DD_mental = mental_health_opr_fun(CF23_Anxiety,
                                           CF24_Depression))


# First for CFM questions 
CFM_data <- CFM5_17 %>% select(CF3_Seeing, 
                               CF6_Hearing, 
                               CF12_Walking_100, 
                               CF13_Walking_500,
                               CF14_Self_care,
                               CF15_Understood_Inside,
                               CF16_Understood_Outside,
                               CF17_Learning,
                               CF18_Remembering,
                               CF19_Concentrating,
                               CF20_Accepting_Challenge,
                               CF21_Controlling_Behavior,
                               CF22_Making_Friends,
                               CF23_Anxiety,
                               CF24_Depression)


CFM_data[is.na(CFM_data)] <- "No difficulty"

`%nin%` = Negate(`%in%`)

CFM_DD_type_list <- list()
for(ii in 1:nrow(CFM_data)) {
  current_row <- CFM_data[ii,]
  current_row_name <- names(current_row)
  current_CFM_DD_type_list <- list()
  
  for(iii in 1:length(current_row)) {
    
    if(current_row[[iii]] %nin% c("No difficulty", "Never", "A few times a year", "Monthly" )) {
      
      current_CFM_DD_type_list[[iii]] <- current_row_name[iii]
      
    } 
  }
  CFM_DD_type_list[[ii]] <- paste(current_CFM_DD_type_list %>% unlist(), collapse="; ")
}



CFM5_17$CFM_DD_type <- CFM_DD_type_list %>% unlist()

# Next for how KBB is operationalizing it 
KBB_CFM_data <- CFM5_17 %>% select(CF14_Self_care,
                                   CF15_Understood_Inside,
                                   CF16_Understood_Outside,
                                   CF17_Learning,
                                   CF18_Remembering,
                                   CF19_Concentrating,
                                   CF20_Accepting_Challenge,
                                   CF21_Controlling_Behavior,
                                   CF22_Making_Friends,
                                   CF23_Anxiety,
                                   CF24_Depression)


KBB_CFM_data[is.na(KBB_CFM_data)] <- "No difficulty"

KBB_CFM_DD_type_list <- list()
for(ii in 1:nrow(KBB_CFM_data)) {
  current_row <- KBB_CFM_data[ii,]
  current_row_name <- names(current_row)
  current_KBB_DD_type_list <- list()
  
  for(iii in 1:length(current_row)) {
    
    if(current_row[[iii]] %nin% c("No difficulty", "Never", "A few times a year", "Monthly" )) {
      
      current_KBB_DD_type_list[[iii]] <- current_row_name[iii]
      
    } 
    
    
  }
  KBB_CFM_DD_type_list[[ii]] <- paste(current_KBB_DD_type_list %>% unlist(), collapse="; ")
}

CFM5_17$KBB_CFM_DD_type <- KBB_CFM_DD_type_list %>% unlist()



epilepsy_positive_questions <- CFM5_17 %>%
  select(E1,
         E2,
         E3,
         E4,
         E5,
         E6,
         E7,
         E8,
         E9)

epilepsy_negative_questions <- CFM5_17 %>%
  select(EN10,
         EN11,
         EN12)

# Any NA's present convert them into 0's
epilepsy_positive_questions[is.na(epilepsy_positive_questions)] <- 0
epilepsy_negative_questions[is.na(epilepsy_negative_questions)] <- 0

# Score if epilepsy is present or not

epilepsy <- list()

for (ii in 1:nrow(epilepsy_positive_questions)) {
  if(any(epilepsy_positive_questions[ii,] > 0)) {
    if (any(epilepsy_negative_questions[ii,] == 1)) {
      epilepsy[[ii]] <- "No"
    } else {
      epilepsy[[ii]] <- "Yes"
    }
  } else {
    epilepsy[[ii]] <- "No"
  }
}

# Add this back to the screener
CFM5_17$Epilepsy <- unlist(epilepsy)



CFM5_17 <- CFM5_17 %>%
  mutate(KBB_DD_status = case_when(
    Epilepsy == "Yes" ~ "Yes",
    KBB_CFM_DD != "No difficulty" ~ "Yes",
    DD_mental %in% c("Daily", "Weekly") ~ "Yes",
    TRUE ~ "No"
  ))

# Extract only the sensory questions
CFM5_17_physical_questions <- CFM5_17 %>%
  select(CF3_Seeing,
         CF6_Hearing,
         CF12_Walking_100,
         CF13_Walking_500)

# Some data cleaning
CFM5_17_physical_questions[is.na(CFM5_17_physical_questions)] <- "No difficulty"


# Create a for loop to obtain the rows that have "Cannot at all" for sensory or motor difficulties
physical_difficulty_type <- list()

for(ii in 1:nrow(CFM5_17_physical_questions)) {
  current_row <- CFM5_17_physical_questions[ii,]
  
  if(any(current_row == "Cannot at all")) {
    physical_difficulty_type[[ii]] <- "Cannot at all"
    
  } else if (any(current_row == "A lot of Difficulty")){
    physical_difficulty_type[[ii]] <- "A lot of Difficulty"
    
  } else if (any(current_row == "Some Difficulty")) {
    physical_difficulty_type[[ii]] <- "Some Difficulty"
    
  } else {
    physical_difficulty_type[[ii]] <- "No difficulty"
    
  }
  
}

CFM5_17$Physical_difficulty_type <- unlist(physical_difficulty_type)

# Create an exclusion variable
CFM5_17 <- CFM5_17 %>%
  mutate(Excluded = case_when(
    KBB_DD_status == "Yes" & Physical_difficulty_type == "Cannot at all" ~ "Yes",
    KBB_DD_status == "No" & Physical_difficulty_type != "No difficulty" ~ "Yes",
    TRUE ~ "No"
  ))

# Set working directory to save the data
setwd("~/KBB_new_2/1_screener/processed_data")

# Save the data
write_csv(CFM5_17, file = "CFM5_17_clean.csv")



# Set working directory to where the scripts are
setwd("~/GitHub/LeoWebsite/KBB.Scripts")


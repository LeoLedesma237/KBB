# Load in packages
library(tidyverse)
library(readxl)
library(lubridate)


# Set working directory to import data
setwd("~/KBB_new_2/1_screener/raw_data")

# Load in CFM2_4 files
CFM2_4.uncleaned <- read_excel(list.files(pattern = "3_4"))

# Remove any redundant rows
CFM2_4.uncleaned <- unique(CFM2_4.uncleaned)

# Selecting and renaming variables to keep
CFM2_4.removed.variables <- CFM2_4.uncleaned %>%
  select(GPS.lat = `_GPS_latitude`,
         GPS.long = `_GPS_longitude`,
         Date_of_Evaluation = start,
         Evaluator_ID,
         Name_of_the_Village,
         Location_Type,
         HOH_First_Name,
         HOH_Last_Name,
         HOH_Date_of_Birth,
         Respondant_First_Name,
         Respondant_Last_Name,
         Child_First_Name,
         Child_Last_Name,
         Child_Gender,
         Child_Date_of_Birth,
         BF = Child_s_Biological_Father,
         BM = Child_s_Biological_Mother,
         Respondant_relationship = `_08_Respondant_s_Relationship_`,
         CF1,
         CF2,
         CF3,
         CF4 = `_04_SENA_ZYINA_ULABESYA_KANC`,
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
         E1 = `_17_Mumwaka_wamana_akakozyeka_kulesegwa`,
         E2 = `_18_Mwaka_wamana_mw_uwa_akucinca_cikanda`,
         E3 = `_19_Mumwaka_wamana_m_oyu_kuli_nakayongene`,
         E4 = `_20_Mumwaka_wamana_kazilukide_kumizeezo`,
         E5 = `_21_Mumwaka_wamana_akuwa_akuluma_mulaka`,
         E6 = `_22_Mumwaka_wamana_ilwa_kulijata_kusuba`,
         E7 = `_23_Mumwaka_wamana_umaulu_nokuba_kumeso`,
         E8 = `_24_Mumwaka_wamana_limwi_akuvwa_kununka`,
         E9 = `_25_Mumwaka_wamana_idwe_bulwazi_bwakuwa`,
         EN10 = `_26_Eeli_penzi_lyaka_na_buyo_ciindi_comwe`,
         EN11 =`_27_Ipenzi_eeli_lyak_o_wakalivwide_ampeyo`,
         EN12 = `_28_Ipenzi_eeli_lyak_e_mumalo_kucibbadela`)

# Cleaning variables with date data 
CFM2_4.cleaned.date <- CFM2_4.removed.variables %>%
  mutate(Date_of_Evaluation = substr(Date_of_Evaluation, start = 1, stop = 10),
         HOH_Date_of_Birth = substr(HOH_Date_of_Birth, start = 1, stop = 10),
         Child_Date_of_Birth = substr(Child_Date_of_Birth, start = 1, stop = 10))

# Creating the variable age
dob <- ymd(CFM2_4.cleaned.date$Child_Date_of_Birth)
doe <- ymd(CFM2_4.cleaned.date$Date_of_Evaluation)

age_weeks <- as.numeric(difftime(doe, dob, units = "weeks"))
age <- age_weeks/52
CFM2_4.cleaned.date$Child_age <- round(age,1)

CFM2_4.HOH.ID <- CFM2_4.cleaned.date %>%
  mutate(HOH_ID = paste(HOH_First_Name, 
                        HOH_Last_Name, 
                        HOH_Date_of_Birth),
         Child_ID = paste(Child_First_Name,
                          Child_Last_Name,
                          Child_Date_of_Birth,
                          Child_Gender))


# Changing binary responses to strings 
CFM2_4.HOH.ID <- CFM2_4.HOH.ID %>%
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

# Create new variables with these labels
CFM2_4.CFM.labeled <- CFM2_4.HOH.ID %>%
  mutate(CF3_Seeing = difficulty_type_fun(CF3),
         CF6_Hearing = difficulty_type_fun(CF6),
         CF10_Walking = difficulty_type_fun(CF10),
         CF11_Fine_Motor = difficulty_type_fun(CF11),
         CF12_Understanding = difficulty_type_fun(CF12),
         CF13_Communicating = difficulty_type_fun(CF13),
         CF14_Learning = difficulty_type_fun(CF14),
         CF15_Playing = difficulty_type_fun(CF15),
         CF16_Controlling_Behavior = difficulty_type_fun(CF16))


# Function to score difficulty type or status
CFM_opr_fun <- function(...) {
  
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

# Add this variable using the criteria for the CFM definition of DD
CFM2_4 <- CFM2_4.CFM.labeled %>%
  mutate(CFM_DD = CFM_opr_fun(CF3_Seeing, 
                              CF6_Hearing, 
                              CF10_Walking, 
                              CF11_Fine_Motor,
                              CF12_Understanding,
                              CF13_Communicating,
                              CF14_Learning,
                              CF15_Playing,
                              CF16_Controlling_Behavior))


# Add this variable using the criteria for the KBB definition of DD
CFM2_4 <- CFM2_4 %>%
  mutate(KBB_CFM_DD = CFM_opr_fun(CF12_Understanding,
                                  CF13_Communicating,
                                  CF14_Learning,
                                  CF15_Playing,
                                  CF16_Controlling_Behavior))

# At least some difficulty
CFM2_4 <- CFM2_4 %>%
  mutate(CFM_DD_at_some = ifelse(CFM_DD == "No difficulty",CFM_DD,"Some Difficulty"))

CFM2_4 <- CFM2_4 %>%
  mutate(KBB_CFM_DD_at_some = ifelse(KBB_CFM_DD == "No difficulty",KBB_CFM_DD,"Some Difficulty")) 


# First for CFM questions 
CFM_data <- CFM2_4 %>% select(CF3_Seeing,
                              CF6_Hearing,
                              CF10_Walking,
                              CF11_Fine_Motor,
                              CF12_Understanding,
                              CF13_Communicating,
                              CF14_Learning,
                              CF15_Playing,
                              CF16_Controlling_Behavior)


CFM_data[is.na(CFM_data)] <- "No difficulty"

CFM_DD_type_list <- list()
for(ii in 1:nrow(CFM_data)) {
  current_row <- CFM_data[ii,]
  current_row_name <- names(current_row)
  current_CFM_DD_type_list <- list()
  
  for(iii in 1:length(current_row)) {
    
    if(current_row[[iii]] != "No difficulty") {
      
      current_CFM_DD_type_list[[iii]] <- current_row_name[iii]
      
    } 
    
    
  }
  CFM_DD_type_list[[ii]] <- paste(current_CFM_DD_type_list %>% unlist(), collapse="; ")
}

CFM2_4$CFM_DD_type <- CFM_DD_type_list %>% unlist()

# Next for how KBB is operationalizing it 
KBB_CFM_data <- CFM2_4 %>% select(CF12_Understanding,
                                  CF13_Communicating,
                                  CF14_Learning,
                                  CF15_Playing,
                                  CF16_Controlling_Behavior)


KBB_CFM_data[is.na(KBB_CFM_data)] <- "No difficulty"

KBB_CFM_DD_type_list <- list()
for(ii in 1:nrow(KBB_CFM_data)) {
  current_row <- KBB_CFM_data[ii,]
  current_row_name <- names(current_row)
  current_KBB_CFM_DD_type_list <- list()
  
  for(iii in 1:length(current_row)) {
    
    if(current_row[[iii]] != "No difficulty") {
      
      current_KBB_CFM_DD_type_list[[iii]] <- current_row_name[iii]
      
    } 
    
  }
  KBB_CFM_DD_type_list[[ii]] <- paste(current_KBB_CFM_DD_type_list %>% unlist(), collapse="; ")
}

CFM2_4$KBB_CFM_DD_type <- KBB_CFM_DD_type_list %>% unlist()


epilepsy_positive_questions <- CFM2_4 %>%
  select(E1,
         E2,
         E3,
         E4,
         E5,
         E6,
         E7,
         E8,
         E9)

epilepsy_negative_questions <- CFM2_4 %>%
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
CFM2_4$Epilepsy <- unlist(epilepsy)


CFM2_4 <- CFM2_4 %>%
  mutate(KBB_DD_status = case_when(
    Epilepsy == "Yes" ~ "Yes",
    KBB_CFM_DD_at_some != "No difficulty" ~ "Yes",
    TRUE ~ "No"
  ))

# Extract only the sensory questions
CFM2_4_physical_questions <- CFM2_4 %>%
  select(CF3_Seeing,
         CF6_Hearing,
         CF10_Walking,
         CF11_Fine_Motor)

# Some data cleaning
CFM2_4_physical_questions[is.na(CFM2_4_physical_questions)] <- "No difficulty"

# Create a for loop to obtain the rows that have "Cannot at all" for sensory or motor difficulties
physical_difficulty_type <- list()

for(ii in 1:nrow(CFM2_4_physical_questions)) {
  current_row <- CFM2_4_physical_questions[ii,]
  
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


CFM2_4$Physical_difficulty_type <- unlist(physical_difficulty_type)

# Create an exclusion variable
CFM2_4 <- CFM2_4 %>%
  mutate(Excluded = case_when(
    KBB_DD_status == "Yes" & Physical_difficulty_type == "Cannot at all" ~ "Yes",
    KBB_DD_status == "No" & Physical_difficulty_type != "No difficulty" ~ "Yes",
    TRUE ~ "No"
  ))


# Set working directory to save the data
setwd("~/KBB_new_2/1_screener/processed_data")

# Save the data
write_csv(CFM2_4, file = "CFM2_4_clean.csv")



# Set working directory to where the scripts are
setwd("~/GitHub/LeoWebsite/KBB.Scripts")
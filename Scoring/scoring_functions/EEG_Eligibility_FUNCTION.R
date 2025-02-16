# The purpose of this function is to take in screener data and indicate if the
# child is eligible for dry-EEG. We will be creating two variables:

# Var1 = EEG_Group: It will contain the values "Epilepsy"; "ID&Epilepsy"; "ID"; "OtherDD"; "Potential Ctrl
# Var2 = EEG_Eligible: It will contain the values "No"; "rsEEG"; "rsEEG, MMN, CPT"
# Var3 = EEG_Exc_Rea: It will give reasons why the child was not eligible for dry-EEG

# There will be five groups created:

# 1) Epilepsy: must have epilepsy and may have at least some difficulties in 
# domains that do not meet ID

# 2) ID & Epilepsy: ID criteria and epilepsy

# 3) ID: no epilepsy + at least some difficulties in at least two domains: 
# communication, learning, remembering

# 4) other CFM-DD: no epilepsy + at least some difficulties in domains that do not meet ID

# 5) No-DD: no epilepsy + no difficulties in the CFM domains


dryEEG_function <- function(screener_data) {
  
  # Save data into a new object that will be returned later
  dat <- screener_data
  
  # Extract the variables we are interested in
  data <- dat %>%
    select(Child_age,
           Excluded,
           Seeing,
           Hearing,
           Epilepsy,
           KBB_CFM_DD_type,
           Screener.Test)
  
  # First round of exclusion criteria explanation
  data <- data %>%
    mutate(Reason1 = case_when(
      # Wrong screener used
      !grepl("Correct", Screener.Test) ~ "Wrong CFM screener used",
      # Exclusion reasoning
      Excluded == "Yes" ~ "Child did not meet sensory capability requirements for their DD/noDD group",
      # Child age under 13
      Child_age < 13 ~ "Child is less than 13 years-old",
      # No exclusion 
      TRUE ~ ""
    ))
  
  # Creating a variable for the ID.status
  data <- data %>%
    mutate(ID_Status = case_when(
      # Communication and learning
      grepl("CF15|CF16", KBB_CFM_DD_type) & grepl("CF17", KBB_CFM_DD_type) ~ "ID",
      # Communication and remembering
      grepl("CF15|CF16", KBB_CFM_DD_type) & grepl("CF18", KBB_CFM_DD_type) ~ "ID",
      # Learning and remembering
      grepl("CF17", KBB_CFM_DD_type) & grepl("CF18", KBB_CFM_DD_type) ~ "ID",
      # Does not meet criteria
      TRUE~ "No-ID"
    ))
  
  # Creating the group variable
  data <- data %>%
    mutate(EEG_Group = case_when(
      # Creating the Epilepsy and ID group
      Epilepsy == "Yes" & ID_Status == "ID" ~ "ID&Epilepsy",
      # Creating the Epilepsy group
      Epilepsy == "Yes" ~ "Epilepsy",
      # Creating the ID Group
      ID_Status == "ID" ~ "ID",
      # Creating the other CFM group
      ID_Status == "No-ID" & !is.na(KBB_CFM_DD_type) ~ "OtherDD",
      # Creating control group
      Epilepsy == "No" & is.na(KBB_CFM_DD_type) ~ "Potentrial Ctrl",
      # Does not meet criteria for a group
      TRUE ~ "-"
      ))
  
  # Second round of exclusion criteria explanation (sensory difficulty)
  data <- data %>%
    mutate(Reason2 = case_when(
      # Exclusion for ID&Epilepsy group
      EEG_Group == "ID&Epilepsy" & Seeing %in% c("A lot of Difficulty", "Cannot at all") |
        EEG_Group == "ID&Epilepsy" & Hearing %in% c("A lot of Difficulty", "Cannot at all") ~ "Excluded for severe seeing or hearing problems",
      # Exclusion for Epilepsy group
      EEG_Group == "Epilepsy" & Seeing %in% c("A lot of Difficulty", "Cannot at all") |
        EEG_Group == "Epilepsy" & Hearing %in% c("A lot of Difficulty", "Cannot at all") ~ "Excluded for severe seeing or hearing problems",
      # Exclusion for ID
      EEG_Group == "ID" & Seeing != "No difficulty" |
        EEG_Group == "ID" & Hearing != "No difficulty" ~ "Excluded for having any seeing/hearing impairment",
      # Exclusion for otherDD
      EEG_Group == "OtherDD" & Seeing != "No difficulty" |
        EEG_Group == "OtherDD" & Hearing != "No difficulty" ~ "Excluded for having any seeing/hearing impairment",
      # For those who do not get excluded
      TRUE ~ ""
    ))
  
  # Merge the two exclusion reasons into one variable
  data <- data %>%
    mutate(EEG_Exc_Rea = case_when(
      # if there are no reasons whatsoever
      nchar(Reason1) == 0 & nchar(Reason2) == 0 ~ "",
      # if only Reason1 has a value
      nchar(Reason1) != 0 & nchar(Reason2) == 0 ~ Reason1,
      # if only Reason2 has a value
      nchar(Reason1) == 0 & nchar(Reason2) != 0 ~ Reason2,
      # If both Reason1 and Reason2 have values
      nchar(Reason1) != 0 & nchar(Reason2) != 0 ~ paste0(Reason1,"; ", Reason2),
      # QC
      TRUE ~ "ERROR"
    ))
  
  # Create a variable for dryEEG eligibility that contains what tasks to do
  data <- data %>%
    mutate(EEG_Eligible = case_when(
      # Not eligible for dry-EEG
      nchar(EEG_Exc_Rea) != 0 ~ "Not Eligible",
      # Epilepsy eligibility
      EEG_Group == "Epilepsy" ~ "rsEEG",
      # Epilepsy and ID eligibility
      EEG_Group == "ID&Epilepsy" ~ "rsEEG",
      # ID and no epilepsy
      EEG_Group == "ID" ~ "rsEEG, MMN, CPT",
      # otherDD and no epilepsy
      EEG_Group == "OtherDD" ~ "rsEEG, MMN, CPT",
      # Controls
      EEG_Group == "Potentrial Ctrl" ~ "rsEEG, MMN, CPT",
      # QC
      TRUE ~ "ERROR"
    ))
  
  # Return the screener dataset with the variables of interest added into it
  dat$EEG_Group <- data$EEG_Group
  dat$EEG_Eligible <- data$EEG_Eligible
  dat$EEG_Exc_Rea <- data$EEG_Exc_Rea
  
  # print out dat
  print(dat)
}


# # #                       # # # # 
# # #                       # # # # 
# # # Quality Control Tests # # # #
# # #                       # # # # 
# # #                       # # # # 

# Visual inspection confirms children were excluded from dry-EEG testing as intended
# Table inspection cofirms children that are Eligible meet the criteria
# Visual inspection shows that EEG groups were created correctly

# Doing several tests
#tests <- dryEEG_function(Binded.data)

# Keep vars of interest
#tests2 <- tests %>% select(Child_age, Seeing, Hearing, KBB_CFM_DD_type, Epilepsy, Screener.Type:EEG_Exc_Rea)

# From the excluded category- are there any people that should NOT have been excluded
#excluded_cat <- filter(tests2, EEG_Exc_Rea != "")
#view(excluded_cat)

# From the included category - are there any people that should NOT be here
#included_cat <- filter(tests2, EEG_Exc_Rea == "")
#table(included_cat$Seeing, included_cat$EEG_Group, included_cat$EEG_Eligible )
#table(included_cat$Hearing, included_cat$EEG_Group, included_cat$EEG_Eligible )
#table(included_cat$Screener.Test)
#summary(included_cat$Child_age)

# Check to make sure the EEG groups were created correctly
#view(filter(included_cat, EEG_Group == "ID")) # Yes
#view(filter(included_cat, EEG_Group == "Epilepsy")) # Yes
#view(filter(included_cat, EEG_Group == "ID&Epilepsy")) # Yes
#view(filter(included_cat, EEG_Group == "OtherDD")) # Yes
#view(filter(included_cat, EEG_Group == "Potentrial Ctrl")) # Yes
#ctrls <- filter(included_cat, EEG_Group == "Potentrial Ctrl")
#table(ctrls$Epilepsy)
#sum(is.na(ctrls$KBB_CFM_DD_type)) == nrow(ctrls)

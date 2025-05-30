# This is the script for scoring the Vineland-II
# We are starting from the Root Directory /KBB

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr) 


# Read in the file
Vineland <- read_excel(paste0(DataLocation,"RAW_DATA/Behavioral/Adults/VinelandII_Raw.xlsx"))

# Create a save pathway
save.pathway <- paste(DataLocation,"FINAL_DS/Behavioral/Adults/VinelandII.xlsx", sep="")



###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############

# Inspect the data
glimpse(Vineland)
nrow(Vineland)

# Rename variable of Child ID
Vineland <- rename(Vineland, Child_ID = Child_Study_ID)

# Identify duplicate IDs
duplicate_IDs <- Vineland$Child_ID[duplicated(Vineland$Child_ID)]
Vineland <- Vineland %>% filter(!Child_ID %in% duplicate_IDs)
nrow(Vineland)

names(Vineland)

# Selecting variable names of interest 
# Communication Receptive
CommR <- select(Vineland, starts_with("CommR"))

#Communication Expressive
most_CommE_names <- names(select(Vineland, starts_with("CommE")))
CommE_names <- c(paste0("CommE",c(5,9)),
                 "Comm19",
                 paste0("CommE",c(7,10)),
                 "_22_Ulaamba_izina_ly_anino_kuti_wabuzigwa",
                 most_CommE_names[5:length(most_CommE_names)])
CommE <- Vineland[, CommE_names]

# Daily Living Skills - Personal
most_DLSP_names <- names(select(Vineland, starts_with("DLSP")))
DLSP_names <- c(paste0("DLSP_",c(1,3,4,7,8,9,20,5,6,11,15,18,19,23)),
                "_25_Ulakopela_tunko_kakunyina_kwiindanya",
                paste0("DLSP_",c(26,28,17)),
                "_27_Ulatontobozya_m_o_cakonzya_kwaasamba",
                most_DLSP_names[18:length(most_DLSP_names)])

DLSP <- Vineland[, DLSP_names]

# Daily Living Skills - Domestic (Item 11 not captured but was supposed to)
DLSD <- select(Vineland, starts_with("DLSD"))

# Daily Living Skills - Community
DLSC <- select(Vineland, starts_with("DLSC"))

# Socialization - Interpersonal
SI <- select(Vineland, starts_with("SI"))

# Socialization - Play and Leisure
SPL <- select(Vineland, starts_with("SPL"))

# Socialization - Coping Skills
most_SCS_names <- names(select(Vineland, starts_with("SCS")))
SCS_names <- c(most_SCS_names[1:18],
                "Note_8",
                most_SCS_names[19:28])

SCS <- Vineland[, SCS_names]

# Quality Control
ncol(CommR)
ncol(CommE)
ncol(DLSP)
ncol(DLSD)
ncol(DLSC)
ncol(SI)
ncol(SPL)
ncol(SCS)

# Rename these items
names(CommR) <- paste0("CommR_", as.numeric(gsub(".*?([0-9]+).*", "\\1", names(CommR))),"_RAW")
names(CommE) <- paste0("CommE_", as.numeric(gsub(".*?([0-9]+).*", "\\1", names(CommE))),"_RAW") 
names(DLSP) <- paste0("DLSP_", as.numeric(gsub(".*?([0-9]+).*", "\\1", names(DLSP))),"_RAW")
names(DLSD) <- paste0("DLSD_", as.numeric(gsub(".*?([0-9]+).*", "\\1", names(DLSD))),"_RAW") 
names(DLSC) <- paste0("DLSC_", as.numeric(gsub(".*?([0-9]+).*", "\\1", names(DLSC))),"_RAW")
names(SI) <- paste0("SI_", as.numeric(gsub(".*?([0-9]+).*", "\\1", names(SI))),"_RAW")
names(SPL) <- paste0("SPL_", as.numeric(gsub(".*?([0-9]+).*", "\\1", names(SPL))),"_RAW")
names(SCS) <- paste0("SCS_", as.numeric(gsub(".*?([0-9]+).*", "\\1", names(SCS))),"_RAW")

#Convert responses into numeric
CommR <- data.frame(sapply(CommR, function(x) as.numeric(x)))
CommE <- data.frame(sapply(CommE, function(x) as.numeric(x)))
DLSP <- data.frame(sapply(DLSP, function(x) as.numeric(x)))
DLSD <- data.frame(sapply(DLSD, function(x) as.numeric(x)))
DLSC <- data.frame(sapply(DLSC, function(x) as.numeric(x)))
SI <- data.frame(sapply(SI, function(x) as.numeric(x)))
SPL <- data.frame(sapply(SPL, function(x) as.numeric(x)))
SCS <- data.frame(sapply(SCS, function(x) as.numeric(x)))

# Replace values that are not 0,1,2 with NAs (777, 888, others)
CommR_clean <- CommR
CommE_clean <- CommE
DLSP_clean <- DLSP
DLSD_clean <- DLSD
DLSC_clean <- DLSC
SI_clean <- SI
SPL_clean <- SPL
SCS_clean <- SCS

CommR_clean <- as.data.frame(lapply(CommR_clean, function(x) ifelse(x %in% c(0, 1, 2), x, NA)))
CommE_clean <- as.data.frame(lapply(CommE_clean, function(x) ifelse(x %in% c(0, 1, 2), x, NA)))
DLSP_clean <- as.data.frame(lapply(DLSP_clean, function(x) ifelse(x %in% c(0, 1, 2), x, NA)))
DLSD_clean <- as.data.frame(lapply(DLSD_clean, function(x) ifelse(x %in% c(0, 1, 2), x, NA)))
DLSC_clean <- as.data.frame(lapply(DLSC_clean, function(x) ifelse(x %in% c(0, 1, 2), x, NA)))
SI_clean <- as.data.frame(lapply(SI_clean, function(x) ifelse(x %in% c(0, 1, 2), x, NA)))
SPL_clean <- as.data.frame(lapply(SPL_clean, function(x) ifelse(x %in% c(0, 1, 2), x, NA)))
SCS_clean <- as.data.frame(lapply(SCS_clean, function(x) ifelse(x %in% c(0, 1, 2), x, NA)))


# Create a dataframe of sum of correct responses
scored_df <- data.frame(scored_CommR = rowSums(CommR_clean, na.rm = T),
                        scored_CommE = rowSums(CommE_clean, na.rm = T),
                        scored_DLSP = rowSums(DLSP_clean, na.rm = T),
                        scored_DLSD = rowSums(DLSD_clean, na.rm = T),
                        scored_DLSC = rowSums(DLSC_clean, na.rm = T),
                        scored_SI = rowSums(SI_clean, na.rm = T),
                        scored_SPI = rowSums(SPL_clean, na.rm = T),
                        scored_SCS = rowSums(SCS_clean, na.rm = T))

# Merging all important variables into one final dataset
final_Vineland <- cbind(select(Vineland, Child_ID, Date_of_Evaluation, Evaluator_ID),
                   CommR, 
                   CommE,
                   DLSP,
                   DLSD,
                   DLSC,
                   SI,
                   SPL,
                   SCS,
                   scored_df)

# Save the scored data
write.xlsx(x= final_Vineland, file = save.pathway)

# Make a note that the data was saved successfully
cat("Saving processed Vineland-II\n")

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation"))])

# Set a pause time for 1 second
Sys.sleep(1)




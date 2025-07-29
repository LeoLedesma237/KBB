# This is the final script that will be used to score PSC-40
# We decided that there will be no grouping by age for this assessment
# Additionally, we are ignoring the last 5 items, making this a PSC-35
# Since we have more than enough observations to run the mirt, we wont put
# any if statements to calculate z-scores.

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(lavaan)

# Read in the cleaned PSC data
PSC <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Adults/PSC.xlsx"))

# Note: no demographics required since no age groups will be created

# Create a save pathway
save.pathway_PSC <- paste(DataLocation,"FINAL_DS/Behavioral/Adults/final_PSC.xlsx", sep="")


###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############

# Drop any Duplicate IDs
duplicate_IDs <- PSC$Child_ID[duplicated(PSC$Child_ID)]
PSC <- PSC %>% filter(!Child_ID %in% duplicate_IDs)

# Extract the items associated with certain scales:
Attention <- select(PSC, paste0("PSC_",c(4,7,8,9,14)))
Internalizing <- select(PSC, paste0("PSC_",c(11,13,19,22,27)))
Externalizing <- select(PSC, paste0("PSC_",c(16,29,31,32,33,34,35)))
Other <-  select(PSC, paste0("PSC_",c(1,2,3,5,6,10,12,15,17,18,20,21,23,24,25,26,28,30)))

# Data cleaning- convert the responses into numeric
Attention_num <- data.frame(sapply(Attention, function(x) as.numeric(x)))
Internalizing_num <- data.frame(sapply(Internalizing, function(x) as.numeric(x)))
Externalizing_num <- data.frame(sapply(Externalizing, function(x) as.numeric(x)))
Other_num <- data.frame(sapply(Other, function(x) as.numeric(x)))

# Removing 3 items since they were problematic
Other_num2 <- select(Other_num, -PSC_18, -PSC_20, -PSC_23)

# Combine everything into one dataset
dat <- cbind(Attention_num, Internalizing_num, Externalizing_num, Other_num2)

# Specifiy the final model
final_model <- '
  General = 1-32
  Attention = 1-5
  Internalizing = 6-10
  Externalizing = 11-17
'

# Fit the final bifactor GRM with QMCEM method (Increase iterations to 5000)
fit_final_bi <- mirt(dat, final_model, itemtype = 'graded', SE = TRUE, method = 'QMCEM', 
                     QMC = TRUE, QMC.points = 2000, technical = list(NCYCLES = 5000))

# Extract theta scores for general factor (psychosocial dysfunction)
thetas <- fscores(fit_final_bi, method = 'EAP', QMC = TRUE)
theta_df <- data.frame(Observation = 1:nrow(dat), Theta = thetas)

# Merge this information with the original ID information
final_PSC_df <- cbind(Child_ID = PSC$Child_ID, select(theta_df, - Observation))

# Save this information
write.xlsx(final_PSC_df, save.pathway_PSC)

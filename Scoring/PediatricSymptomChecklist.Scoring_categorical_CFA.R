# This is the script for scoring Pediatric Symptom Checklist (PSC) Using IRT and Z-scores for custom groups
# The data loaded will be processed data from the scored/cleaned PSC data in the FINAL_DATA folder

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(lavaan)

# Read in the cleaned PSC data
PSC <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Adults/PSC.xlsx"))

# Note: no demographics required since no age groups will be created

# Create a save pathway
save.pathway_PSC <- paste(DataLocation,"FINAL_DS/Behavioral/Adults/PSC_IRT_Z.xlsx", sep="")


###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########                                   #############

# Extract the items associated with certain scales:
Attention <- select(PSC, paste0("PSC_",c(4,7,8,9,14)))
Internalizing <- select(PSC, paste0("PSC_",c(11,13,19,22,27)))
Externalizing <- select(PSC, paste0("PSC_",c(16,29,31,32,33,34,35)))
Other <-  select(PSC, paste0("PSC_",c(1,2,3,5,6,10,12,15,17,18,20,21,23,24,25,26,28,30)))
Novel <- select(PSC, paste0("PSC_",36:40))

# Data cleaning- convert the responses into numeric
Attention_num <- data.frame(sapply(Attention, function(x) as.numeric(x)))
Internalizing_num <- data.frame(sapply(Internalizing, function(x) as.numeric(x)))
Externalizing_num <- data.frame(sapply(Externalizing, function(x) as.numeric(x)))
Other_num <- data.frame(sapply(Other, function(x) as.numeric(x)))
Novel_num <- data.frame(sapply(Novel, function(x) as.numeric(x)))
  
####
######## Part 1: Running a categorical CFA (PSC-17)
####

# Step 1: Preliminary Analysis- how much variation is present within the item responses
rbind(
  mutate(pivot_longer(Attention, cols= PSC_4:PSC_14), scale = "Attention"),
  mutate(pivot_longer(Internalizing, cols= PSC_11:PSC_27), scale = "Internalizing"),
  mutate(pivot_longer(Externalizing, cols= PSC_16:PSC_35), scale = "Externalizing")
  ) %>%
  group_by(name, value) %>%
  reframe(count = length(value),
          scale) %>%
  unique() %>%
  mutate(name = gsub("PSC_","", name)) %>%
  ggplot(aes(x = name, y = count, fill = value)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  theme_classic() +
  facet_grid(scale~., scale = "free_y", space = "free_y") +
  labs(x = "Items", y = "Count",
       title = "Count of Responses by Item for each\nScale in the PSC-17")

# Investigate this using tables
sapply(items1, function(x) table(x))
sapply(items1, function(x) round(prop.table(table(x)),2))

# Bind all of the items of interest together
items1 <- cbind(Attention_num, Internalizing_num, Externalizing_num)

# Step 2: running two categorical CFAs
# Three factor correlated
mod1 <- '
  # Measurement Model
  AT =~ 1*PSC_4 + PSC_7 + PSC_8 + PSC_9 + PSC_14
  IN =~ 1*PSC_11 + PSC_13 + PSC_19 + PSC_22 + PSC_27
  EX =~ 1*PSC_16 + PSC_29 + PSC_31 + PSC_32 + PSC_33 + PSC_34 + PSC_35
'

# Second order factor (hierarchical)
mod2 <- '
  # Measurement Model
  AT =~ 1*PSC_4 + PSC_7 + PSC_8 + PSC_9 + PSC_14
  IN =~ 1*PSC_11 + PSC_13 + PSC_19 + PSC_22 + PSC_27
  EX =~ 1*PSC_16 + PSC_29 + PSC_31 + PSC_32 + PSC_33 + PSC_34 + PSC_35
  
  # Second Order Measurement Model
  PI =~ NA*AT + IN + EX
  
  # Second Order Factor Variance
  PI ~~ 1*PI
'

# One general factor
mod3 <- '
  # Measurement Model
  PI =~ 1*PSC_4 + PSC_7 + PSC_8 + PSC_9 + PSC_14 + PSC_11 + PSC_13 + PSC_19 + PSC_22 + PSC_27 + PSC_16 + PSC_29 + PSC_31 + PSC_32 + PSC_33 + PSC_34 + PSC_35
  
  # Second Order Factor Variance
  PI ~~ PI
'
# Three specific and general factor 
mod4 <- '
  # Measurement Model (Specific Factor)
  AT =~ 1*PSC_4 + PSC_7 + PSC_8 + PSC_9 + PSC_14
  IN =~ 1*PSC_11 + PSC_13 + PSC_19 + PSC_22 + PSC_27
  EX =~ 1*PSC_16 + PSC_29 + PSC_31 + PSC_32 + PSC_33 + PSC_34 + PSC_35
  
  # Measurement Model (General Factor)
  PI =~ 1*PSC_4 + PSC_7 + PSC_8 + PSC_9 + PSC_14 + PSC_11 + PSC_13 + PSC_19 + PSC_22 + PSC_27 + PSC_16 + PSC_29 + PSC_31 + PSC_32 + PSC_33 + PSC_34 + PSC_35
  
  # Assuming the general and specific factors are independent (orthogonal)
  PI ~~ 0*AT
  PI ~~ 0*IN
  PI ~~ 0*EX
  AT ~~ 0*IN
  AT ~~ 0*EX
  IN ~~ 0*EX
  
  
'


# Run the categorical CFA
fit1 <- cfa(mod1, data = items1, estimator = "WLSMV", ordered = colnames(items1))
fit2 <- cfa(mod2, data = items1, estimator = "WLSMV", ordered = colnames(items1))
fit3 <- cfa(mod3, data = items1, estimator = "WLSMV", ordered = colnames(items1))
fit4 <- cfa(mod4, data = items1, estimator = "WLSMV", ordered = colnames(items1))

# Inspect the models (their units)
parameterEstimates(fit1)
parameterEstimates(fit2)
parameterEstimates(fit3)
parameterEstimates(fit4)

# Inspect the models (standardized)
standardizedSolution(fit1)
standardizedSolution(fit2)
standardizedSolution(fit3)
standardizedSolution(fit4)


# Obtain fit indices
summary(fit1, fit.measures = TRUE)
summary(fit2, fit.measures = TRUE)

# Checking ways to improve the model through modification indices
modificationIndices(fit1, sort = TRUE, minimum = 10)
modificationIndices(fit2, sort = TRUE, minimum = 10)
modificationIndices(fit3, sort = TRUE, minimum = 10)
modificationIndices(fit4, sort = TRUE, minimum = 10)

# Identify and remove bad items?






####### EXPERIMENTAL USING IRT INSTEAD OF CFA

####
######## Part 2: Running GRM (PSC-35)
####

# Step 1: Preliminary Analysis- how much variation is present within the item responses
rbind(
  mutate(pivot_longer(Attention, cols= PSC_4:PSC_14), scale = "Attention"),
  mutate(pivot_longer(Internalizing, cols= PSC_11:PSC_27), scale = "Internalizing"),
  mutate(pivot_longer(Externalizing, cols= PSC_16:PSC_35), scale = "Externalizing"),
  mutate(pivot_longer(Other, cols= PSC_1:PSC_30), scale = "Other")
  ) %>%
  group_by(name, value) %>%
  reframe(count = length(value),
          scale) %>%
  unique() %>%
  mutate(name = gsub("PSC_","", name)) %>%
  ggplot(aes(x = name, y = count, fill = value)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  theme_classic() +
  facet_grid(scale~., scale = "free_y", space = "free_y") +
  labs(x = "Items", y = "Count",
       title = "Count of Responses by Item for each Scale in the PSC-35")


# Combine everything into one dataset
dat <- cbind(Attention_num, Internalizing_num, Externalizing_num, Other_num)

# Load in package to do IRT analyses
library(mirt)

# Define the unidimensional model
model_uni <- 'F = 1-35'

# Fit unidimensional GRM
fit_uni <- mirt(dat, model_uni, itemtype = 'graded', SE = TRUE)

# Check model fit
summary(fit_uni)
m2_stats_uni <- M2(fit_uni)
print(m2_stats_uni)
itemfit_stats_uni <- itemfit(fit_uni)
print(itemfit_stats_uni)

# Extract theta scores
thetas_uni <- fscores(fit_uni, method = 'EAP')
theta_df_uni <- data.frame(Observation = 1:nrow(dat), Theta = thetas_uni)

# Define the multidimensional model
model <- '
  General = 1-35
  Attention = 1-5
  Internalizing = 6-10
  Externalizing = 11-17
'

# Fit bifactor GRM with QMCEM method
fit_bifactor <- mirt(dat, model, itemtype = 'graded', SE = TRUE, method = 'QMCEM', QMC = TRUE, QMC.points = 1000)

# Check model fit
summary(fit_bifactor)  # Factor loadings and model summary
m2_stats <- M2(fit_bifactor, QMC= TRUE)  # Global fit statistic
print(m2_stats)

# Check item fit
itemfit_stats <- itemfit(fit_bifactor, QMC = TRUE)  # Item-level fit
print(itemfit_stats)

# Extract theta scores for general factor (psychosocial dysfunction)
thetas <- fscores(fit_bifactor, method = 'EAP')[,1]  # General factor only
theta_df <- data.frame(Observation = 1:nrow(dat), Theta = thetas)



# Visualize factor loadings
item_params <- coef(fit_bifactor, simplify = TRUE)$items

# Standardized loadings: a / sqrt(1 + Sigma a^2)
factors_only <- data.frame(item_params[,c("a1", "a2", "a3", "a4")])
den <- transmute(factors_only, denominator = sqrt(1 + a1^2 + a2^2 + a3^2 + a4^2))
factor_loadings <- sweep(factors_only, 1, den$denominator, "/")
factor_loadings$r2 <- rowSums(factor_loadings^2) 
round(factor_loadings, 3)

# Let's create the final model by removing non meaningful items (18, 20, 23)
Attention_num2 <-  Attention_num
Internalizing_num2 <- Internalizing_num
Externalizing_num2 <- Externalizing_num
Other_num2 <- select(Other_num, -PSC_18, -PSC_20, -PSC_23)

dat2 <- cbind(Attention_num2, Internalizing_num2, Externalizing_num2, Other_num2)

final_model <- '
  General = 1-32
  Attention = 1-5
  Internalizing = 6-10
  Externalizing = 11-17
'

# Fit the final bifactor GRM with QMCEM method
fit_final_bifactor <- mirt(dat2, final_model, itemtype = 'graded', SE = TRUE, method = 'QMCEM', 
                           QMC = TRUE, QMC.points = 2000, technical = list(NCYCLES = 5000))

# Check model fit
summary(fit_final_bifactor)  # Factor loadings and model summary
m2_stats_final <- M2(fit_final_bifactor, QMC= TRUE)  # Global fit statistic
print(m2_stats_final)  


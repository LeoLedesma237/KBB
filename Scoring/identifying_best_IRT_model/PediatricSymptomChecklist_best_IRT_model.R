# This is the script for scoring Pediatric Symptom Checklist (PSC) Using IRT and Z-scores for custom groups
# The data loaded will be processed data from the scored/cleaned PSC data in the FINAL_DATA folder
# Note this script takes a while to fully run due to the MIRT bi-factor model

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(lavaan)

# Read in the cleaned PSC data
PSC <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Adults/PSC.xlsx"))

# Note: no demographics required since no age groups will be created

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


# Visulaizing the data- how much variation is present within the item responses
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

####
######## Part 1: Unidimensional vs Multidimensional (bi-factor) model
#####

# Define the unidimensional and bi-factor models
model_uni <- 'F = 1-35'
model_bi <- '
  General = 1-35
  Attention = 1-5
  Internalizing = 6-10
  Externalizing = 11-17
'

# Fit unidimensional GRM and bifactor GRM with QMCEM method (speeds up the process)
fit_uni <- mirt(dat, model_uni, itemtype = 'graded', SE = TRUE)
fit_bi <- mirt(dat, model_bi, itemtype = 'graded', SE = TRUE, method = 'QMCEM', QMC = TRUE, QMC.points = 1000)

# Compare model fit
m2_stats_uni <- M2(fit_uni)
print(round(m2_stats_uni,3))
m2_stats <- M2(fit_bi, QMC= TRUE)
print(round(m2_stats,3))
mirt::anova(fit_uni, fit_bi)


####
######## Part 2: Bi-Factor model item investigation
#####

# Item performance
itemfit_stats <- itemfit(fit_bi, QMC = TRUE)

# Visualize the difficulty thresholds
item_params <- coef(fit_bi, simplify = TRUE)$items
difficulty_thresholds <- data.frame(item_params[,c("d1", "d2")])
mutate(difficulty_thresholds, diff = d2 - d1) %>% round(2)

# Standardized Factor loadings and h2
fit_bi_sum <- summary(fit_bi)
cbind(fit_bi_sum$rotF, fit_bi_sum$h2) %>% round(2)

####
######## Part 3: Developing the Final IRT Model
#####

# Let's create the final model by removing non meaningful items (18, 20, 23)
Attention_num2 <-  Attention_num
Internalizing_num2 <- Internalizing_num
Externalizing_num2 <- Externalizing_num
Other_num2 <- select(Other_num, -PSC_18, -PSC_20, -PSC_23)

dat2 <- cbind(Attention_num2, Internalizing_num2, Externalizing_num2, Other_num2)

# Specifiy the final model
final_model <- '
  General = 1-32
  Attention = 1-5
  Internalizing = 6-10
  Externalizing = 11-17
'

# Fit the final bifactor GRM with QMCEM method (Increase iterations to 5000)
fit_final_bi <- mirt(dat2, final_model, itemtype = 'graded', SE = TRUE, method = 'QMCEM', 
                           QMC = TRUE, QMC.points = 2000, technical = list(NCYCLES = 5000))

# Compare the model fit indices
print(round(m2_stats,3))
m2_final_stats <- M2(fit_final_bi, QMC= TRUE)
print(round(m2_final_stats,3))

# Checking item fit
# Item performance
itemfit_stats_final <- itemfit(fit_final_bi, QMC = TRUE)

# Visualize the difficulty thresholds
item_params_final <- coef(fit_final_bi, simplify = TRUE)$items
difficulty_thresholds_final <- data.frame(item_params_final[,c("d1", "d2")])
mutate(difficulty_thresholds_final, diff = d2 - d1) %>% round(2)

# Standardized Factor loadings and h2
fit_bi_sum_final <- summary(fit_final_bi)
stand_fl_mirt <- cbind(fit_bi_sum_final$rotF, fit_bi_sum_final$h2) %>% round(2)
(stand_fl_mirt <- data.frame(stand_fl_mirt))

####
######## Part 4: Extracting Theta Values
#####

# Extract theta scores for general factor (psychosocial dysfunction)
thetas <- fscores(fit_final_bi, method = 'EAP', QMC = TRUE)
theta_df <- data.frame(Observation = 1:nrow(dat), Theta = thetas)

# Plot the theta distributions
data.frame(thetas) %>%
  pivot_longer(cols = c(General:Externalizing), names_to = "Construct", values_to = "Theta") %>%
  ggplot(aes(x = Theta)) +
  geom_histogram(bins = 30) +
  facet_grid(~Construct) +
  theme_classic()

# Viewing distribution information
library(psych)
describe(data.frame(thetas)$General)
describe(data.frame(thetas)$Attention)
describe(data.frame(thetas)$Externalizing)
describe(data.frame(thetas)$Internalizing)


####
######## Part 5: Validating our factor loadings by running a categorical CFA
#####
library(lavaan)


# Set the items that correspond with the scale or lack of scale
A <- c(4, 7, 8, 9, 14)
I <- c(11, 13, 19, 22, 27)
E <- c(16, 29, 31, 32, 33, 34, 35)
O <- c(1, 2, 3, 5, 6, 10, 12, 15, 17,
      21, 24, 25, 26, 28, 30) # Remove 18, 20, 23

# Create the item names (copy them into the model)
paste(paste0("PSC_",A), collapse = " + ")
paste(paste0("PSC_",I), collapse = " + ")
paste(paste0("PSC_",E), collapse = " + ")
paste(paste0("PSC_",c(A,I,E, O)), collapse = " + ")

# Create the residual variance for the indicators
paste(rep(paste0("PSC_",c(A,I,E, O)), each = 2), collapse = " ~~ ")


# Let's create a bi-factor model (we will assume G and S are orthogonal)
bifactor_model <- '
  # specific factors (fixed = 0; free = 17,)
  A =~ NA*PSC_4 + PSC_7 + PSC_8 + PSC_9 + PSC_14
  I =~ NA*PSC_11 + PSC_13 + PSC_19 + PSC_22 + PSC_27
  E =~ NA*PSC_16 + PSC_29 + PSC_31 + PSC_32 + PSC_33 + PSC_34 + PSC_35

  # general factor (fixed = 0; free = 32)
  P  =~ NA*PSC_4 + PSC_7 + PSC_8 + PSC_9 + PSC_14 + PSC_11 + PSC_13 + PSC_19 + PSC_22 + PSC_27 + PSC_16 + PSC_29 + PSC_31 + PSC_32 + PSC_33 + PSC_34 + PSC_35 + PSC_1 + PSC_2 + PSC_3 + PSC_5 + PSC_6 + PSC_10 + PSC_12 + PSC_15 + PSC_17 + PSC_21 + PSC_24 + PSC_25 + PSC_26 + PSC_28 + PSC_30
  
  # fix variance of the specific factors (fixed = 3; free = 0)
  A ~~ 1*A
  I ~~ 1*I
  E ~~ 1*E
  
  # fix the variance of the general factor (fixed = 1; free = 0)
  P ~~ 1*P
  
  # fix the covariances of the factors to 0 (fixed = 6; free = 0)
  A ~~ 0*I
  A ~~ 0*E
  E ~~ 0*I
  
  P ~~ 0*A
  P ~~ 0*E
  P ~~ 0*I
  
  # We will not specify residual variances because it will cause the model to fail
'


# Fit the model
bifactor_fit <- cfa(bifactor_model, data = dat2, estimator = "WLSMV", parameterization = "theta", ordered = names(dat2))
standardizedSolution(bifactor_fit)

# Convert this into kinda wide format
stand_fl <- data.frame(standardizedSolution(bifactor_fit)) %>%
  filter(lhs %in% c("A","I","E","P")) %>%
  select(lhs, rhs, est.std) %>%
  filter(!est.std %in% c(0,1)) %>%
  pivot_wider(names_from = lhs, values_from = est.std)

# Convert the NA's into 0  
stand_fl[is.na(stand_fl)] <- 0

# Remove add rhs as a rowname and remove it as a variable
stand_fl <- data.frame(stand_fl)
row.names(stand_fl) <- stand_fl$rhs


# Print out the standardized loadings like in the mirt model
stand_fl_cfa <- stand_fl %>%
  mutate(r2 = P^2 + A^2 + I^2 + E^2) %>%
  select(General = P, Attention = A, Internalizing = I, Externalizing = E, r2) %>%
  data.frame() %>%
  round(2)


# Graph the standardized factor loadings from both approaches
stand_fl_mirt$model <- "mirt"
stand_fl_mirt <- rename(stand_fl_mirt, r2 = h2)
stand_fl_cfa$model <- "cfa"


rbind(stand_fl_mirt, stand_fl_cfa) %>%
  mutate(items = rep(row.names(stand_fl_mirt), 2)) %>%
  pivot_longer(cols = c(General, Attention, Internalizing, Externalizing, r2),
               names_to = "cat",
               values_to = "value") %>%
  pivot_wider(names_from = model,
              values_from = value,
              names_glue = "{model}") %>%
  select(items, cat, mirt, cfa) %>%
  mutate(mirt = round(mirt, 2),
         cfa = round(cfa, 2)) %>%
  ggplot(aes(x = mirt, y = cfa)) +
  geom_point() +
  facet_grid(~cat, scale = "free_x") +
  geom_smooth(method = "lm", se = F) +
  theme_classic() +
  labs(title = "Comparing standardized factor loadings and r2 between\n multidimensional IRT GRM and categorical CFA")


####
######## Part 6: Comparing theta values and z-scores
#####

# Calculate z-scores from original dataset
z_scored_df <- data.frame(General_raw = rowSums(dat),
                          Attention_raw = rowSums(dat[,c(names(Attention))]),
                          Internalizing_raw = rowSums(dat[,c(names(Internalizing))]),
                          Externalizing_raw= rowSums(dat[,c(names(Externalizing))]))


z_scored_df <- z_scored_df %>%
  transmute(General = c(scale(General_raw)),
            Attention = c(scale(Attention_raw)),
            Internalizing = c(scale(Internalizing_raw)),
            Externalizing = c(scale(Externalizing_raw)))


# Calculate the theta scores from the original model
thetas_org <- fscores(fit_bi, method = 'EAP', QMC = TRUE)
theta_org_df <- data.frame(Observation = 1:nrow(dat), Theta = thetas_org)

# Recalculate the theta scores fromt the final model
thetas <- fscores(fit_final_bi, method = 'EAP', QMC = TRUE)
theta_df <- data.frame(Observation = 1:nrow(dat), Theta = thetas)

# rename the dataframes to stack them together
names(theta_org_df) <- c("Observations", "General", "Attention", "Internalizing", "Externalizing")
names(theta_df) <- c("Observations", "General", "Attention", "Internalizing", "Externalizing")

# Remove the observation name
theta_org_df <- select(theta_org_df, - Observations)
theta_df <- select(theta_df, - Observations)

# Introduce variable to keep track of dataset type
z_scored_df$cat <- "z_scores"
theta_org_df$cat <- "theta_original"
theta_df$cat <- "theta_final"

# Bind them into one dataset
all_stand_df <- rbind(
  z_scored_df,
  theta_org_df,
  theta_df
) %>%
  pivot_longer(cols = General:Externalizing) %>%
  mutate(row.num = rep(1:(nrow(.)/3),3)) %>%
  pivot_wider(names_from = cat, values_from = value) 


# Load in the package patchwork
library(patchwork)


# Print out histograms to get the distribution
h1 <- all_stand_df %>%
  ggplot(aes(x = z_scores)) +
  geom_histogram() +
  facet_grid(~name) +
  theme_classic()

h2 <- all_stand_df %>%
  ggplot(aes(x = theta_original)) +
  geom_histogram() +
  facet_grid(~name) +
  theme_classic()

h3 <- all_stand_df %>%
  ggplot(aes(x = theta_final)) +
  geom_histogram() +
  facet_grid(~name) +
  theme_classic()


# combine
(h1 | h2) /
  h3



# scatter 1: z_scores vs theta_original
p1 <- all_stand_df %>%
  ggplot(aes(x = z_scores, y = theta_original)) +
  geom_point() +
  labs(x = "Z-scores", y = "Theta (original)") +
  facet_grid(~name) +
  geom_smooth() + 
  theme_minimal()

# scatter 2: z_scores vs theta_final
p2 <- ggplot(all_stand_df, aes(x = z_scores, y = theta_final)) +
  geom_point() +
  labs(x = "Z-scores", y = "Theta (final)") +
  facet_grid(~name) +
  geom_smooth() + 
  theme_minimal()

# scatter 3: theta_original vs theta_final
p3 <- ggplot(all_stand_df, aes(x = theta_original, y = theta_final)) +
  geom_point() +
  labs(x = "Theta (original)", y = "Theta (final)") +
  facet_grid(~name) +
  geom_smooth() + 
  theme_minimal()

# combine
(p1 | p2) /
  p3

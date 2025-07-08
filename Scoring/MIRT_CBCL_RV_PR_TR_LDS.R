# Ultimate MIRT Scoring Script
# The goal of this script is to score every behavioral assessment (not including physical) in this script
# RULES:
# 1) If the require sample size is present, then an MIRT model will be used instead of z-scores
# 2) All models will produce one score; The CBCL6-18 will not produce values for scales since the factor structure analysis was not promising
# 3) No data imputation will be done for missing values in datasets that are scored through z-scores; This is because it is almost impossible to tell in circumstances if the value is actually missing or the child stopped responding
# 4) All items with only one response category WILL BE REMOVED when running mirt

# load in the packages
library(tidyverse)
library(mirt)
library(purrr)
library(readxl)
library(openxlsx)


# Load in the cleaned datasets
CBCL3_6 <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Adults/CBC_3_6.xlsx"))
CBCL6_18 <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Adults/CBC_6_18.xlsx"))
RV <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Children/RecepVocab.xlsx"))
PR <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Children/PatternReas.xlsx"))
TR <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Children/Triangles.xlsx"))
LDS <- read_excel(paste0(DataLocation,"FINAL_DS/Behavioral/Children/LettrDig.xlsx"))
demo <- read_excel(paste0(DataLocation,"FINAL_DS/Demographics/Demographics.xlsx"))

# Create a save pathway for scored information
save.pathway <- paste(DataLocation,"FINAL_DS/Behavioral/Adults_Children_MIRT/", sep="")


###########                                   #############
########### THE REST OF THE CODE IS AUTOMATIC #############
###########  


# Set seed
set.seed(123)


#######
############### Part 1 Data Cleaning
######

# Data Cleaning - keep only items and Child_ID
CBCL3_6 <- select(CBCL3_6, Child_ID, CBC3_6_1:CBC3_6_100)
CBCL6_18 <- select(CBCL6_18, Child_ID, CBC6_18_1:CBC6_18_120)
RV <- select(RV, Child_ID, RV_1:RV_30)
PR <- select(PR, Child_ID, paste0("PR_",1:36)) # Removing timer info since already used for scoring
TR <- select(TR, Child_ID, TR_1:TR_27)
LDS <- select(LDS, Child_ID, c(paste0("NF_",1:16), paste0("NB_",1:16), paste0("LF_",1:16), paste0("LB_",1:16)))


# Create a function (with Grok help) that will return numeric variables and tell you what values were changed to NA
convert_to_numeric <- function(df) {
  na_values <- data.frame(row = integer(), column = character(), original_value = character())
  
  result_df <- df
  for (col_name in names(df)) {
    original_col <- df[[col_name]]
    suppressWarnings({ result_df[[col_name]] <- as.numeric(original_col) })
    na_indices <- which(is.na(result_df[[col_name]]) & !is.na(original_col))
    if (length(na_indices) > 0) {
      na_values <- rbind(na_values, data.frame(
        row = na_indices,
        column = col_name,
        original_value = as.character(original_col[na_indices])
      ))
    }
  }
  
  list(data = result_df, na_values = na_values)
}

# (Optional) View what variables were forced to be NAs
head(convert_to_numeric(CBCL3_6)$na_values,10)
head(convert_to_numeric(CBCL6_18)$na_values,10)
head(convert_to_numeric(RV)$na_values,10)
head(convert_to_numeric(PR)$na_values,10)
head(convert_to_numeric(TR)$na_values,10)
head(convert_to_numeric(LDS)$na_values,10)

# Convert datasets to numeric 
CBCL3_6 <- convert_to_numeric(CBCL3_6)$data
CBCL6_18 <- convert_to_numeric(CBCL6_18)$data
RV <- convert_to_numeric(RV)$data
PR <- convert_to_numeric(PR)$data
TR <- convert_to_numeric(TR)$data
LDS <- convert_to_numeric(LDS)$data


# Data Cleaning- turning all 777 and 888 into 0's
replace_777_888_with_zero <- function(df) {
  df[df == 777 | df == 888] <- 0
  return(df)
}

CBCL3_6 <- replace_777_888_with_zero(CBCL3_6)
CBCL6_18 <- replace_777_888_with_zero(CBCL6_18)
RV <- replace_777_888_with_zero(RV)
PR <- replace_777_888_with_zero(PR)
TR <- replace_777_888_with_zero(TR)
LDS <- replace_777_888_with_zero(LDS)


# Save datasets into a list for optimal cleaning
data_list <- list(CBCL3_6 = CBCL3_6, CBCL6_18 = CBCL6_18, RV = RV, PR = PR, TR = TR, LDS = LDS, demo = demo)


# Data cleaning- removing all duplicates
for(ii in 1:length(data_list)){
  cd <- data_list[[ii]]
  duplicated_ids <- cd$Child_ID[duplicated(cd$Child_ID)]
  cat(length(duplicated_ids),"duplicate IDs have been removed from", names(data_list[ii]), "\n")
  data_list[[ii]] <- cd %>% filter(!Child_ID %in% duplicated_ids)
}


# Introduce Age into all of the datasets
demo2 <- data_list[["demo"]]
data_list[["demo"]] <- NULL

for(ii in 1:length(data_list)) {
  cd <- data_list[[ii]]
  data_list[[ii]] <- cd %>% left_join(select(demo2, Child_ID, Age), by = "Child_ID")
  cat("Age information has been added into",  names(data_list[ii]), "\n")
}


# Set age group and save it into a list
CBCL3_6_n_group <- c("2-7")
CBCL6_18_n_group <- c("6-11.9; 12-19")
RV_n_group <- c("2-4.9; 5-6.9; 7-8.9; 9-14.9; 15-19")
PR_n_group <- c("4-6.9; 7-8.9; 9-11.9; 12-14.9; 15-18.9")
TR_n_group <- c("2-4.9; 5-6.9; 7-8.9; 9-13.9; 14-19")
LDS_n_group <- c("2-4.9; 5-6.9; 7-8.9; 9-12.9; 13-19")

age_group_list <- list(CBCL3_6_n_group = CBCL3_6_n_group, CBCL6_18_n_group = CBCL6_18_n_group, RV_n_group = RV_n_group, 
                       PR_n_group = PR_n_group, TR_n_group = TR_n_group, LDS_n_group = LDS_n_group)

# Create a helper function to parse the age strings (ChatGPT)
parse_age_groups <- function(age_string) {
  str_split(age_string, ";")[[1]] %>%
    map(~ {
      bounds <- as.numeric(str_split(.x, "-")[[1]])
      list(min = bounds[1], max = bounds[2])
    })
}

#####
########## Part 2: Scoring the data
#####

# Run the MIRT on each dataset (Exclude CBCL3-6 for now)
message_list <- list()
scored_data_list <- list()

for(ii in 1:length(data_list)) {
  cd <- data_list[[ii]]
  grp <- age_group_list[[ii]]
  
  # Create a list within a list and then give the parent list a name
  scored_data_list[[ii]] <- list() # Allows for nesting
  names(scored_data_list)[ii] <- names(data_list[ii])
  
  # Parsing the age groups
  age_grps <- parse_age_groups(grp)
  
  # Run a for loop for each age group!
  for(iii in 1:length(age_grps)) {
    # Subset current dataset by age group
    msg1 <- paste0(names(data_list[ii])," is being scored")
    c_grp <- age_grps[[iii]] # Current Age Group Range
    msg2 <- paste0("This is for the age group ",c_grp$min, "-", c_grp$max)
    sub_cd <- cd %>% filter(Age >= c_grp$min, Age <= c_grp$max)
    items <- select(sub_cd, - Child_ID, - Age) # Only keep the items
    
    # Give the sublist a name by the current age group
    scored_data_list[[ii]][[iii]] <- list() # Allows for further nesting
    names(scored_data_list[[ii]])[iii] <- paste0("Group_",c_grp$min, "_", floor(c_grp$max))
    
    # Print out the messages
    cat(msg1,"\n"); cat(msg2,"\n")
    
    # Count the number of unique value  for each item (ignore na's)
    item_values <- do.call(c, map(items, ~ length(unique(na.omit(.x)))))
    
    # Generate the item types vector
    item_types <- ifelse(item_values == 1, "Remove", 
                         ifelse(item_values == 2, "2PL" ,"graded"))
    
    # Identify items that only have NA values and mark them for removal
    all_Na_items <- names(items)[colSums(is.na(items)) == nrow(items)]
    if(length(all_Na_items) > 0) {item_types[names(item_values) %in% all_Na_items] <- "Remove"}
    
    # Create a vector or removed items
    removed_items <- names(item_types)[item_types == "Remove"]
    
    # Generate a message for items that had to be removed
    if(any(item_types == "Remove")) {
      msg3 <- paste0("Items ",paste0(removed_items, collapse =", "), " were removed (",round(length(removed_items)/length(items)*100,1),"%)")
      } else{
      msg3 <- paste0("No Items were removed")
      }
    cat(msg3,"\n")
    
    # Generate the final item dataset and corresponding item types vector
    kept_items <- names(item_types)[item_types != "Remove"]
    items2 <- items[,kept_items]
    item_types2 <- item_types[item_types != "Remove"]
    
    # Generate the mode specifications
    mod <- paste0("F = 1-",length(items2))
    
    # Generate a message on what model is being used
    sum_2 <- sum(item_values == 2)
    sum_3 <- sum(item_values == 3)
    if(sum_2 > 1 & sum_3 == 0) {msg4 <- "A 2PL only model will be used, requiring a sample size of 150"; required_n = 150}
    if(sum_2 > 1 & sum_3 > 1) {msg4 <- "A 2PL and graded model will be used, requiring a sample size of 200"; required_n = 200}
    if(sum_2 == 0 & sum_3 > 1) {msg4 <- "A graded model will be used, requiring a sample size of 200"; required_n = 200}
    cat(msg4,"\n")
    
    # Obtain the current sample size and print if the model can be run or not
    c_n <- nrow(items2)
    
    # Score the data based on whether sample size was met or not
    # Scoring using mirt
    if(c_n >= required_n){
      msg5 <- paste0("We have a sample size of ",c_n,", which is enough to run the MIRT model")
      cat(msg5,"\n")
      
      # Run the mirt model and produce the theta scores
      fit_mirt <- mirt(items2, model = mod, itemtype = item_types2, SE = TRUE, technical = list(NCYCLES = 20000))
      theta <- fscores(fit_mirt, method = "EAP")
      
      # Also calculate z-scores just to compare for quality control
      zscores <- scale(rowSums(items2, na.rm = T))
      
      # Introduce this information back into the dataset
      final_df <- cbind(Child_ID = sub_cd$Child_ID, items2, group_n = c_n, standardized_type = "theta", standardized_score = c(theta), z = c(zscores))
      
      # Save the message and the scored data into a list
      scored_data_list[[ii]][[iii]] <- list(Messages = paste0(c(msg1, msg2, msg3, msg4), collapse = "\n"), Data = final_df)
    
    }
    # Scoring using z-scores
    if(c_n < required_n){
      msg5 <- paste0("We have a sample size of ",c_n,", which is too small so we will compute z-scores")
      cat(msg5,"\n")
      
      # Calculate the sum of rows and then turn them into z-scores
      zscores <- scale(rowSums(items2, na.rm = T))
      
      # Introduce this information back into the dataset
      final_df <- cbind(Child_ID = sub_cd$Child_ID, items2, group_n = c_n, standardized_type = "zscores", standardized_score = c(zscores), z = c(zscores))
      
      # Save the message and the scored data into a list
      scored_data_list[[ii]][[iii]] <- list(Messages = paste0(c(msg1, msg2, msg3, msg4, msg5), collapse = "\n"), Data = final_df)
    }
  }
  cat("Scoring the datasets was successful ;) \n\n")
}


#####
########## Part 3: Binding the datasets together
#####

data_names_list <- names(scored_data_list)
binded_data <- list()

for(ii in 1:length(data_names_list)) {
  # Extract the first dataset
  cd_names <- names(scored_data_list[[data_names_list[ii]]])
  
  # Bind all of the subdatasets together
  fixed_items_data <- list()
  
  for(iii in 1:length(cd_names)){
    # Extract current subset of the dataset
    cd_dat <- scored_data_list[[data_names_list[ii]]][[cd_names[iii]]]$Data
    cd_dat_org <- select(data_list[[ii]], - Age)
    
    # Identify missing items
    missing_items <- setdiff(names(cd_dat_org), names(cd_dat))
    
    # Introduce missing items (if any) back into the dataset
    cd_dat[missing_items] <- NA_real_
    
    # Rearrange the items to match that of the original
    cd_dat2 <- cbind(cd_dat[,names(cd_dat_org)], group_n = cd_dat$group_n, standardized_type = cd_dat$standardized_type, standardized_score = cd_dat$standardized_score, z = cd_dat$z)
    
    # Save this new fixed dataset into a list 
    fixed_items_data[[iii]] <- cd_dat2
  }
  # Add the binded datasets into the binded data list
  binded_data[[ii]] <- do.call(rbind, fixed_items_data)
  names(binded_data)[ii] <- names(data_list)[ii]
  
}


#####
########## Part 4 Quality Control Before Saving
#####

cat("We have scored data from these datasets:", paste0(names(binded_data), collapse = ", "))

for(ii in 1:length(binded_data)) {
  current_dat <- binded_data[[ii]]
  current_name <- names(binded_data)[ii]
  cat("For",current_name,": \n")
  cat("There are",nrow(current_dat),"and",length(current_dat),"variables \n")
  cat("There are",length(unique(current_dat$Child_ID)), "unique IDs in this dataset \n")
  cat("There are", sum(!is.na(current_dat$standardized_score)) ,current_dat$standardized_type[1] ," values with",sum(is.na(current_dat$standardized_score)),"missing values \n")
  cat("The correlation between", current_dat$standardized_type[1], "and z is:", round(cor(current_dat$standardized_score, current_dat$z),2),"\n\n")
  
}


#####
########## Part 5: If everything checks out save the data
######

for(ii in 1:length(binded_data)) {
  current_dat <- binded_data[[ii]]
  current_name <- names(binded_data)[ii]
  
  # Save the data
  write.xlsx(x = current_dat, file = paste0(save.pathway,current_name,".xlsx"))
  
}

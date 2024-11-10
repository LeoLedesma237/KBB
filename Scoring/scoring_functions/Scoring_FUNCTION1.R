# The purpose of this function is to take in a dataset and return the following variables

# - The number of CDKs for each row
# - The number of NRG for each row
# - The number of NAs for each row (After account for the stop rules)
# - The number of items given for each row
# - A performance measure value

scoring_function1 <- function(items, stopRuleNum) {
  
  # Extract current Items (these are df)
  Items <- items
  
  # Set the stop rule num
  StopRuleNum = stopRuleNum
  
  # Part 1: Identifying the frequency of Child did not know (CDK) and no response given (NRG)
  # 777 (CDK), 888 (NRG)
  CDK_Num <- rowSums(mutate(Items,across(everything(), ~ str_count(., "777"))), na.rm = T)
  NRG_Num <- rowSums(mutate(Items,across(everything(), ~ str_count(., "888"))), na.rm = T)
  
  # Convert 777 and 888 into zeroes
  Items <- data.frame(do.call(cbind,lapply(Items, function(x) ifelse(x == "777", "0", x))))
  Items <- data.frame(do.call(cbind,lapply(Items, function(x) ifelse(x == "888", "0", x))))
  
  # Part 1: Stop Rule
  # Identify if the stop rule has been met
  # The following code is thanks to ChatGPT
  StopRuleMet_list <- list()
  StopRuleIndx <- list()
  StopRuleViolated_list <- list()
  
  for(ii in 1:nrow(Items)) {
    
    # Current row
    currentRow <- as.numeric(t(Items[ii,]))
    
    # Convert the row into one element
    StopRule <- rep(0, StopRuleNum)
    StopRule_len <- length(StopRule)
    
    # Identify where the stop rule has taken place (if it has)
    StopRule_Position <- which(sapply(1:(length(currentRow) - StopRule_len + 1), function(i) {
      all(currentRow[i:(i + StopRule_len - 1)] == StopRule)
    }))
    
    if(length(StopRule_Position) != 0) {
      # Record that the stop rule has been met
      StopRuleMet_list[[ii]] <- "Yes"
      
      # Save the item number the stop rule applied
      StopRuleIndx[[ii]] <- StopRule_Position[1]
      
      # Create a vector of the values after the stop rule
      currentRowAfter <- currentRow[StopRule_Position[1]:length(currentRow)]
      
      if(sum(currentRowAfter,na.rm = T) == 0) {
        # Record no stop rule violated
        StopRuleViolated_list[[ii]] <- "No"
      
        } else {
        # Record a stop rule violation
        StopRuleViolated_list[[ii]] <- "Violated"
        
      }
      
    } else {
      # Record that the stop rule has not been met
      StopRuleMet_list[[ii]] <- "No"
      
      # Save the last value of the item for the stoprule indx
      StopRuleIndx[[ii]] <- length(currentRow)
      
      # Recording a blank for stop rule violated
      StopRuleViolated_list[[ii]] <- "-"
    }
    
  }
  
  # Record information from the Stop Rule
  StopRuleMet <- do.call(c,StopRuleMet_list)
  StopRuleIndx <- do.call(c, StopRuleIndx)
  StopRuleViolated <- do.call(c,StopRuleViolated_list)
  
  # Create list to save the following information
  NAsNum_list <- list()
  Performance_list <- list()
  
  # Score the following information based on the StopRuleIndx
  for(ii in 1:nrow(Items)) {
  
    # Current row
    currentRow <- as.numeric(t(Items[ii,]))
    
    # Obtain the current Stop Rule Indx
    currentStopIndx <- StopRuleIndx[ii]
    
    # Number of NAs in the data (before stop rule)
    NAsNum_list[[ii]] <- sum(is.na(currentRow[1:currentStopIndx]))
    
    # Performance before the stop rule
    Performance_list[[ii]] <- sum(currentRow[1:currentStopIndx], na.rm = T)
    
  }
  
  # Save the information from above
  NAsNum <- do.call(c,NAsNum_list)
  Performance <- do.call(c, Performance_list)

  # Save this information in the original dataset given
  items$CDK <- CDK_Num
  items$NRG <- NRG_Num
  items$SR_Met <- StopRuleMet
  items$SR_item <- StopRuleIndx
  items$SR_Viola <- StopRuleViolated
  items$NA_num <- NAsNum
  items$Performance <- Performance
  
  # return the completed dataframe
  return(items)

}



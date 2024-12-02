# The purpose of this function is to take in an unscored dataset and returned scored items

# - It will leave CDKs untouched
# - It will leave NRG untouched
# - It will leave NA's untouched
# - Any other values will be replaced by a 0 or 1 depending on if it matches the answer key

scoring_function0 <- function(items, AnswerKey) {
  
  # Create a function to score each element 
  element_scoring_fun <- function(x,y) {
    
    if(is.na(x)) {
    return(NA)
    
  } else if(x == 777) {
    return(777)
    
  } else if(x == 888) {
    return(888)
    
  } else if(x == y) {
    return(1)
    
  } else {
    return(0)
    
  }
  }
  
  # Add this function within a function that would work for a row
  row_scoring_fun <- function(x, y) {
    mapply(element_scoring_fun, x, y)
  }
  
  # Now apply this row function to every row in the dataset
  Scored_items <- t(apply(items, 1, function(row) row_scoring_fun(row, AnswerKey)))
  
  # Return the scored items as a dataframe
  Scored_items_df <- data.frame(Scored_items)
  
   return(Scored_items_df)

}

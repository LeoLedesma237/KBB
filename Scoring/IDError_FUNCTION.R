# This is a script that creates a function to report on errors for IDs


# Load the final ID tracker
Final_ID_Tracker <- read_excel(paste0(DataLocation, "FINAL_DS/Screener/Matched_Siblings/FINAL_ID_Tracker.xlsx"))


#dataset.name = "booga"
#x <- c(1:999, NA, NA, 1000,1,1,15,3,9,9,9,9,9, -10)
#reference <- 1:1001 

check_id_errors <- function(dataset.name, x) {
  
  # Name of the dataset
  dataset <- dataset.name
  
  # Reference variable
  reference <- Final_ID_Tracker$ID
  
  # Total IDs present
  ID.num.M1 <- paste(dataset, ": There are data from ",length(unique(x[!is.na(x)]))," unique IDs",sep="")
  
  # Data that still needs collecting
  needs.data <- setdiff(reference, x)
  needs.data <- needs.data[!is.na(needs.data)]
  
  if(length(needs.data) > 0) {
    needs.data.M2 <- paste(dataset, ": ", length(needs.data), " IDs needs their data collected", sep="")
  } else {
    needs.data.M2 <- paste(dataset, ": Only correct IDs are present", sep="")
  }
  
  # Identify the IDs that are duplicate
  x.frequency <- table(x)
  
  # Identify duplicates
  duplicates <- x.frequency[x.frequency >1]
  
  # How many times is the duplicate present
  duplicate <-  names(duplicates)
  duplicate.frequency <- unname(duplicates)
  
  # Create a new variable with this info
  if(length(duplicate.frequency) > 0) {
  duplicate.M3 <- paste(dataset, ": ID ",duplicate, " is present ",duplicate.frequency," times", sep="")
  } else {
    duplicate.M3 <- paste(dataset, ": has no duplicate IDs", sep="")
  }
  # Get the IDs that are not supposed to be here
  difference <- setdiff(x,reference)
  difference <- difference[!is.na(difference)]
  
  if(length(difference) > 0) {
  badId.M4 <- paste(dataset, ": ", "ID ",difference, " is not a real ID", sep="")
  } else {
    badId.M4 <- paste(dataset, ": Only correct IDs are present", sep="")
  }
  # Obtain the number of NA's
  NA.sum <- sum(is.na(x))
  
  if(NA.sum > 0) {
  Nas.M5 <- paste(dataset, ": ", "There are ",sum(is.na(x))," NAs for IDs", sep ="")
  } else{
    Nas.M5 <- paste(dataset, ": There are no missing IDs")
  }
  # Create a dataset called notes
  notes <- data.frame(notes = c(ID.num.M1,
                                needs.data.M2,
                                duplicate.M3,
                                badId.M4,
                                Nas.M5))

  # Return notes
  notes
  
}



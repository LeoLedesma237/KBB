# Here we will have three functions to score Triangle data from children 7_18
# The three functions were designed to score items of different time thresholds
# Function1 scores items where if they meet one threshold, the response is considered correct
# Function2 scores items where there are two thresholds, meeting the more stringent threshold results in a bonus point
# Function3 scores items where there are three thresholds, meeting the most stringent threshold results in two bonus points
# meeting the next threshold results in one bonus point,

# Creating Function 1
# Threshold1 must be scalar
time_scoring_fun1 <- function(item, time, threshold1) {
  # return NA if either is missing
  na_idx <- is.na(item) | is.na(time)
  
  # default score: if item != 0, then (time ??? threshold1) ??? 1, else 0
  score <- ifelse(item != 0, 
                  ifelse(time > threshold1, 0, 1), 0)
  score[na_idx] <- NA
  
  # if either field contains 777 or 888, return that code instead
  special_idx <- (item %in% c(777,888)) | (time %in% c(777,888))
  score[special_idx] <- ifelse(
    item[special_idx] %in% c(777,888),
    item[special_idx],
    time[special_idx]
  )
  
  score
}


# a little helper that takes:  item (0/1), time (numeric), thresh (length-2)
time_scoring_fun2 <- function(item, time, thresh) {
  # short???circuit if special codes present
  if (item %in% c(777,888)) return(item)
  if (time %in% c(777,888)) return(time)
  
  if (is.na(item) || is.na(time)) {
    return(NA_integer_)
  }
  if (item == 0) {
    return(0L)
  }
  # item != 0 from here
  lo <- min(thresh); hi <- max(thresh)
  if (time > hi)      0L
  else if (time > lo) 1L
  else                 2L
}


time_scoring_fun3 <- function(item, time, thr) {
  # short???circuit if special codes present
  if (item %in% c(777,888)) return(item)
  if (time %in% c(777,888)) return(time)
  
  if (is.na(item) || is.na(time)) return(NA_integer_)
  if (item == 0) return(0L)
  lo <- thr[1]; mid <- thr[2]; hi <- thr[3]
  if      (time > hi)  0L
  else if (time > mid) 1L
  else if (time > lo)  2L
  else                  3L
}

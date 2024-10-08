# Load in packages
library(tidyverse)

# Load in the data
setwd("~/KBB_new_2/1_screener/final_data")
All.Children <- read_excel("All Children.xlsx")

# Select variables of interest
All.Children2 <- select(All.Children, Evaluator_ID, Child_age, Date_of_Evaluation, Screener.Type, Screener.Test)

# Keep the incorrect screener entries
Incorrect.Screeners <- All.Children2[grepl(pattern = "Incorrect", All.Children2$Screener.Test),]



# View the results
table(Incorrect.Screeners$Evaluator_ID)
view(Incorrect.Screeners)

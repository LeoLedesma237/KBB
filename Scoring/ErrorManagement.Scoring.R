# This is the script is to organize the CSV outputs of all the scripts


# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr) # str_count()

# Set the pathway for where the CSV Ouputs are located
report.assessment.pathway <- paste0(DataLocation,"REPORTS/Individual/")

# Extract the file names (Children)
all.assessments <- list.files(path=report.assessment.pathway)

# Create a vector for the pathways
all.pathways <- c(rep(report.assessment.pathway, length(all.assessments)))

# Create a dataset from both csv file names and their pathways
data <- data.frame(cbind(all.assessments, all.pathways))

# Create a full pathway
data$full.pathway <- paste(data$all.pathways, "/" ,data$all.assessments, sep ="")

# Read in each file
all.data.list <- list()

for(ii in 1:nrow(data)) {
  
  all.data.list[[ii]] <- read.csv(data$full.pathway[ii])
  
}

# Bind the read in files into one document
combined.data <- unlist(do.call(c, all.data.list))

# Split the data into two variables
combined.data2 <- data.frame(do.call(rbind, str_split(combined.data, pattern = ":")))

# Rename the combined data2
names(combined.data2) <- c("Dataset", "Errors")

# Create a save pathway
save.pathway.CSV <- paste(DataLocation,"REPORTS/",
                          "Main_Notes.csv", sep="")

# Save the scored data
write_csv(x= combined.data2, file = save.pathway.CSV)

# Remove all global environment objects to declutter
rm(list = ls()[!(ls() %in% c("DataLocation", "combined.data2"))])

# Set a pause time for 1 second
Sys.sleep(1)
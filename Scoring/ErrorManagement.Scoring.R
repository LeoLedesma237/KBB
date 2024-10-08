# This is the script is to organize the CSV outputs of all the scripts
# We are starting from the Root Directory /KBB
Root.Folder <- "C:/Users/lledesma.TIMES/Documents/KBB"
setwd(Root.Folder)

# Load in Packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr) # str_count()

# Set the pathway for where the CSV Ouputs are located
children.assessment.pathway <- "~/KBB/2_behavioral_assessments/Children/Processed_Notes"
adult.assessment.pathway <- "~/KBB/2_behavioral_assessments/Adults/Processed_Notes"

# Extract the file names (Children)
children.assessments <- list.files(path=children.assessment.pathway)

# Extract the file names (Adults)
adults.assessments <- list.files(path = adult.assessment.pathway)

# Combine the filenames into one vector
all.assessments <- c(children.assessments, adults.assessments)

# Create a vector for the pathways
all.pathways <- c(rep(children.assessment.pathway, length(children.assessments)),
                  rep(adult.assessment.pathway, length(adults.assessments)))

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
save.pathway.CSV <- paste(Root.Folder,"/",
                          "2_behavioral_assessments", "/",
                          "All_Notes.csv", sep="")

# Save the scored data
write_csv(x= combined.data2, file = save.pathway.CSV)

# Remove all global environment objects to declutter
rm(list=ls())

# Load in the dataset
All.notes <- read.csv("~/KBB/2_behavioral_assessments/All_Notes.csv")

# Set the working directory to the scoring scripts
setwd("~/GitHub/LeoWebsite/KBB.Scripts/Scoring")

# Set a pause time for 1 second
Sys.sleep(1)
# Load in package
library(tidyverse)
library(robotoolbox)
library(openxlsx)
library(readxl)


# Set working directory to Root Folder
Root.Folder <- "C:/Users/lledesma.TIMES/Documents/KBB"
setwd(Root.Folder)

# API Token
token <- "947975196832610fca8a1673d54f0530c7cb8275"


# Setting up KoBo
kobo_setup(url = "https://kf.kobotoolbox.org",
           token = token)



# Kobo Settings (Quality Control)
kobo_settings()

# Viewing the data
all.data.l <- kobo_asset_list()

# Write the uids for all needed data

df.uid <- data.frame(

  ASEBA_YSR.iud = "a6qiRpZCN5Y79dPAXMtb4A",
  Atlantis.iud = "aSp3rJjnNFGrEN9TvTJE9Z",
  BRIEF2_SF.iud = "aTBTDSaspJjMyx8sWRDzbc",
  PatternReas.iud = "aSCbRS4rUwDPLaAGvaHztc",
  Triangles_LettrDig.iud = "aKBKpKnKssVquL3yxWqx8G",
  ZAT.iud = "aSoQexoZ8NAeBqTAm9sqQt",
  
  ASEBA_CBC.iud = "aKuaJqgcuCv5GAbmXdAbt6",
  BRIEF2_Parent.iud = "a8crqFHGueNo9WDa6D8kPa",
  HomeEnv.iud = "aMsdG3VCzxGbCutgMtkLjq",
  PSC.iud = "a4t7cAdJvHnmagPQArZ9yX",
  PSQ.iud = "aGdRnPPQeFAV64ha5fxxpP",
  VinelandII.iud = "aHP8oarPVKEfGAA3L87QRH",
  
  CFM2_4.iud = "aGvHGCmV9HF9yzsQqCjuJN",
  CFM5_17.iud = "a3xZkViikGeNqbuZC7hAzb"

)


# Transpose and rename df.iud
df.uid.t <- data.frame(t(df.uid))
names(df.uid.t) <- "uid"

# Add Child  Parent Assessment Label
df.uid.t$label <- c(rep("Children", 6), rep("Adults", 6), rep("Screener",2))

# Add Partial Pathway 
df.uid.t <- df.uid.t %>%
  mutate(raw.pathway = case_when(
  
  label == "Children" ~"2_behavioral_assessments/Children/Raw",
  label == "Adults" ~ "2_behavioral_assessments/Adults/Raw",
  label == "Screener" ~ "1_screener/Raw"
  
))


# Import all data into a list
all.data.list <- list()

for(ii in 1:nrow(df.uid.t)) {

  # Import the data
  all.data.list[[ii]] <- kobo_submissions(df.uid.t$uid[ii])
  
  # Name the list elements
  names(all.data.list)[ii] <- gsub(".iud" ,"", row.names(df.uid.t)[ii])
  
  # Check progress
  print(paste("Importing ",row.names(df.uid.t)[ii]," (",ii,"/",nrow(df.uid.t),")", sep=""))

}


# Saving the raw data in their respective raw_data folder
for(ii in 1:length(all.data.list)) {

  # Create The Save Pathway
  save.pathway = paste(Root.Folder,"/",
                       df.uid.t$raw.pathway[ii], "/",
                       names(all.data.list),"_Raw.xlsx", sep="")[ii]
  
  # Select the data to save
  data <- all.data.list[[ii]]
  
  # Save each data in its raw form
  write.xlsx(x= data, file = save.pathway)
  
  # Check progress
  print(paste("Saving ",row.names(df.uid.t)[ii]," (",ii,"/",nrow(df.uid.t),")", sep=""))
  

}

# Wait 2 seconds
Sys.sleep(2)

# Change the directory to the location of the scripts
setwd("~/GitHub/LeoWebsite/KBB.Scripts/Scoring")

# Score the data
source("PatternReasoning.Scoring.R")
source("TrianglesAndLetterDigitSpan.Scoring.R")
source("ReceptiveVocabulary.Scoring.R")
source("PediatricSymptomChecklist.Scoring.R")
source("PhysicalData.Scoring.R")

# Emerged Problems
source("ErrorManagement.Scoring.R")
print(All.notes)
# This script is designed to plot CPT data
# It will do it for each individual plot
# It will delete all previously created plots from the directory on where they are saved
# This is in case modifications had to be made to the data before replotting

# Three commands are coming in from MATLAB
# 1) The pathway to where the R packages in the local computer are saved
# 2) The root directory of the project (KBB)
# 3) The location we want the plots to be saved

# Testing the objects
#
# LibraryDir = 'C:/Users/lledesma.TIMES/AppData/Local/Programs/R/R-4.2.2/library'
# SpecDatDir = 'C:/Users/lledesma.TIMES/Documents/KBB/Data/MODIFIED_DS/04_CPT_Inscapes/03_ERP_CSVs/'
# PlotDir = 'C:/Users/lledesma.TIMES/Documents/KBB/Data/MODIFIED_DS/04_CPT_Inscapes/04_Plotted_CPT/'

# Retrieve command-line arguments
args <- commandArgs(trailingOnly = TRUE)

# Assign arguments to variables
LibraryDir <- args[1]
SpecDatDir <- args[2]
PlotDir <- args[3]

# Load in packages
.libPaths(LibraryDir)
library(readr)
library(tidyverse)
library(ggplot2)

# List all .png files (full paths)
png_files <- list.files(path = PlotDir, pattern = "\\.png$", full.names = TRUE)

# Delete all png files within
unlink(png_files) 

# Load in all of the filenames
all_CPT_files <- list.files(path = SpecDatDir)
all_CPT_fullname <- list.files(path = SpecDatDir, full.names = TRUE)

# Create a data frame that is able to organized this information for each subject
full_dat <- data.frame(
  all_files = all_CPT_files,
  all_files_pathways = all_CPT_fullname
)

# Add more variables of interest to the dataframe
full_dat <- full_dat %>%
  mutate(ID = sub("_.*", "", all_files),
         Conditions = sub(".*CPT_ERPs_(.*)\\.txt", "\\1", all_files),
         Task = case_when(
           grepl("1", Conditions) ~ "ISI 1",
           grepl("2", Conditions) ~ "ISI 2",
           grepl("4", Conditions) ~ "ISI 4",
           TRUE ~ "ERROR"
         ),
         Trial_Type = ifelse(grepl("Standard", Conditions), "Standard", "Deviant")) 

# Create a vector of unique IDs
unique_IDs <- unique(full_dat$ID)

# Save the read in datasets into a list
all_data_list <- list()

# Read in the files and combin them for each unique ID
for(ii in 1:nrow(full_dat)) {
  # Save the data by row
  current_row <- full_dat[ii,]
  # Read in the ERP .csv
  current_ERP <- read_tsv(current_row$all_files_pathways, show_col_types = FALSE)
  # Keep only Cz data
  current_ERP <- filter(current_ERP, time == "Cz")
  # Drop the channel variable
  current_ERP <- select(current_ERP, - time)
  # Drop columns with any NA values
  current_ERP <- current_ERP[, colSums(is.na(current_ERP)) == 0]
  # Simplify the names
  names(current_ERP) <- as.character(as.numeric(names(current_ERP)))
  # Add ID information
  current_ERP$ID <- current_row$ID
  # Add condition information
  current_ERP$Condition <- current_row$Conditions
  # Add Task information
  current_ERP$Task <- current_row$Task
  # Add Trial Type information
  current_ERP$Trial_Type <- current_row$Trial_Type
  # Save the data into a list
  all_data_list[[ii]] <- current_ERP
} 

# Bind the dataset into one
full_dat2 <- do.call(rbind,all_data_list)

for(ii in 1:length(unique_IDs)) {
  
  # Select data from one person
  current_ID <- unique_IDs[ii]
  one_person <- filter(full_dat2, ID == current_ID)
  
  # Make the data longer
  long_dat <- one_person %>%
    pivot_longer(cols = c("-100":"798"),
                 names_to = "Time",
                 values_to = "Amplitude") %>%
    mutate(Time = as.numeric(Time),
           Task = factor(Task, levels = c("ISI 1", "ISI 2", "ISI 4")))
  
  
  # Generate the Graphs
  long_dat %>%
    ggplot(aes(x= Time, y = Amplitude, color = Trial_Type)) +
    geom_hline(yintercept= 0, linewidth = 1, linetype="dashed", color = "black") +
    geom_vline(xintercept = 0, linewidth = 1, linetype="dashed", color = "black") + 
    geom_line(linewidth = 1) +
    geom_point(size = 0.75) +
    facet_wrap(~Task, nrow = 3) +
    scale_color_manual(values = c("Deviant" = "blue", "Standard" = "red")) + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),  # Center title, larger & bold
      axis.title.x = element_text(size = 10),  # Larger x-axis label
      axis.title.y = element_text(size = 10),  # Larger y-axis label
      axis.text.x = element_text(size = 10),   # Larger x-axis tick labels
      axis.text.y = element_text(size = 10)    # Larger y-axis tick labels
    )
  
  # Save the plot
  ggsave(paste0(PlotDir,current_ID,"_CPT.png"), plot = last_plot(), width = 3.4, height = 3)
}


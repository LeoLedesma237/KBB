# This script is designed to plot resting-state EEG data
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
# SpecDatDir = 'C:/Users/lledesma.TIMES/Documents/KBB/Data/MODIFIED_DS/01_Eyes_Open_Inscapes/03_Spectrum_Data_for_Plotting/'
# PlotDir = 'C:/Users/lledesma.TIMES/Documents/KBB/Data/MODIFIED_DS/01_Eyes_Open_Inscapes/04_Plotted_Spectrum/'

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
 
# Specify the names and full names of the cleaned rsEEG files
all_ch_hz_pow_names <- gsub(pattern = "_cleaned_dry_ch_hz_pow.txt", "", list.files(path = SpecDatDir))
all_ch_hz_pow_full_names <- list.files(path = SpecDatDir, full.names = TRUE)
  
# Load in each rsEEG file, plot it, save it 
for(ii in 1:length(all_ch_hz_pow_full_names)) {
  # Read in the rsEEG data
  rsEEG <- read_tsv(all_ch_hz_pow_full_names[ii], show_col_types = FALSE)
  # rsEEG <- filter(rsEEG, Channel == "Fp1")
  # Convert data into long format
  rsEEG_long <- rsEEG %>%
    pivot_longer(-Channel, names_to = "Hz", values_to = "Power") %>%
    mutate(Hz = as.numeric(Hz)) %>%
    filter(Hz <= 30)
  
  # Generate a plot
  rsEEG_plot <- rsEEG_long %>%
    ggplot(aes(x = Hz, y = Power, color = Channel)) +
    geom_line(linewidth = 1) +
    geom_point(size = 0.75) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = paste0(all_ch_hz_pow_names[ii], " Power Spectrum")) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),  # Center title, larger & bold
      axis.title.x = element_text(size = 10),  # Larger x-axis label
      axis.title.y = element_text(size = 10),  # Larger y-axis label
      axis.text.x = element_text(size = 10),   # Larger x-axis tick labels
      axis.text.y = element_text(size = 10)    # Larger y-axis tick labels
    )
  
  # Save the plot
  ggsave(paste0(PlotDir, all_ch_hz_pow_names[ii] ,".png"), plot = last_plot(), width = 3.4, height = 3)
}


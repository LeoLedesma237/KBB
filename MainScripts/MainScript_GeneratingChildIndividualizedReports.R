# This is the main script to generate several parent reports for the children that finished the study
# This is essentially a for loop take generates a parent report, one by one, for each child that needs one
# Additionally, a log file is generated indicating if the report was correctly generated or not
# Also- we generate an excel file indicating how many of the children that need a report had RAW Behavioral data


# Load in packages
library(readxl)
library(rmarkdown)

# Insert the name of the village (make sure excel file exists)
Village = 'Halumba' # Ex: 'Mangunza'


#########################                          ############################
######################                                 ########################
###################### REST OF THE SCRIPT IS AUTOMATIC ########################
######################                                 ########################
##########################                        #############################


# Specify the directories to run the Rmarkdown, load the excel file with IDs, and save the reports
MS_pw <- "~/GitHub/KBB/MainScripts/"
IDs_pw <- "C:/Users/lledesma.TIMES/Documents/KBB/Data/FINAL_DS/IndividualizedReports/"
output_dir <- paste0(IDs_pw, "Generated_Reports/",Village,"/")

# Load in the Child IDs
excel_name = paste0("KBB_IndividualizedReportIDs_",Village,".xlsx")
IDs_ds <- read_excel(paste0(IDs_pw,excel_name), sheet = "Results")

# Create factor IDs for our purposes
child_ids <- IDs_ds$`Child ID`

# Load in dataset with information on who has available RAW data (directly from Kobo)
containsRawData <- read_excel("~/KBB/Data/FINAL_DS/Behavioral/containsRawData.xlsx")

# Check how many of them have raw data available (directly from Kobo)
needReportcontainsRawData <- containsRawData %>%
  filter(Child_ID %in% child_ids)

# Create the village directory
dir.create(output_dir)

# Create a log file to track successful and error report generations
log_file <- file.path(output_dir, paste0("render_log_", Village,".txt"))
cat("Render Log -", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = log_file)

# Loop through each ID
for (id in child_ids) {
  # Define the output filename based on the ID
  output_filename <- paste0("child_report_", id, ".html")
  
  # Render the RMarkdown with the current ID as a parameter
  tryCatch({
  render(
    input = paste0(MS_pw,"Individualized_Reports_v4.Rmd"),  # Path to your RMarkdown file
    output_file = output_filename,  # Custom name for the HTML file
    output_dir = output_dir,
    params = list(id = id)  # Pass the ID to the RMarkdown params
  )
    # Create a line for success
    cat("Success: Generated report for ID:", id, "\n", file = log_file, append = TRUE)
  }, error = function(e) {
    # Create a line for error
    cat("Error for ID:", id, "-", conditionMessage(e), "\n", file = log_file, append = TRUE)

  })
}

# Save an excel of how many children had raw data available
write.xlsx(x = needReportcontainsRawData, file = paste0(output_dir, paste0(Village, "_containsRawData.xlsx")))

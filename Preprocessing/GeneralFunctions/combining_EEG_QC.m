% INPUT: 
% QC_CSV_pathway: directory to where the EEG files are (ex: 'C:/user/EEG/RAW_DATA/)
% Combined_QC_CSV_name: designated name for the combined csv excel file
% Combined_QC_CSV_pathway: the extension of the eeg file (ex: .vmrk)
%
% Description: This function binds all QC CSVs in a directory together
% and that saves that information where specified as an EXCEL file.

function output = combining_EEG_QC(QC_CSV_pathway,Combined_QC_CSV_name, Combined_QC_CSV_pathway)

% load the QC CSV directory stucct
csvFiles = dir(fullfile(QC_CSV_pathway, '*.csv'));

% Initialize an empty table to hold all the data
combinedData = table();

% Loop over each CSV file and load its contents
for i = 1:length(csvFiles)
    % Get the full path of the CSV file
    filePath = fullfile(csvFiles(i).folder, csvFiles(i).name);
    
    % Read the CSV file into a table
    currentData = readtable(filePath);
    
    % Concatenate the current data with the combined data
    combinedData = [combinedData; currentData];
end

% Sort the Table by date
combinedData = sortrows(combinedData, 'Date'); 

% Save the combined data in Reports
SaveFileName_Full = append(Combined_QC_CSV_pathway, Combined_QC_CSV_name);
writetable(combinedData, SaveFileName_Full);

end
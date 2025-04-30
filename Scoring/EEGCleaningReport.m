% Set Pathway to your ONR_MBAP GitHub Repository
KBB = 'C:\Users\KBB DATA ENTRY\Documents\GitHub\KBB\';

% Load in the 4 excel files with EEG data
EO = readtable(append(KBB,'Data\REPORTS\01_Eyes_Open_Inscapes_Report.xlsx'));
EC = readtable(append(KBB,'Data\REPORTS\02_Eyes_Closed_Report.xlsx'));
MMN = readtable(append(KBB,'Data\REPORTS\03_MMN_Inscapes_Report.xlsx'));
CPT = readtable(append(KBB,'Data\REPORTS\04_CPT_Inscapes_Report.xlsx'));

% Keep only the variables of interest and rename Percent_Remaining
EO2 = EO(:, {'Date', 'File_Name', 'Percent_Remaining'});
EO2.Properties.VariableNames{'Percent_Remaining'} = 'EO';

EC2 = EC(:, {'File_Name', 'Percent_Remaining'});
EC2.Properties.VariableNames{'Percent_Remaining'} = 'EC';

MMN2 = MMN(:, {'File_Name', 'Percent_Remaining'});
MMN2.Properties.VariableNames{'Percent_Remaining'} = 'MMN';

CPT2 = CPT(:, {'File_Name', 'Percent_Remaining'});
CPT2.Properties.VariableNames{'Percent_Remaining'} = 'CPT';

% Add a new column 'File_Number' to each table
EO2.File_Number = extractBefore(EO2.File_Name,'_');
EC2.File_Number = extractBefore(EC2.File_Name,'_');
MMN2.File_Number = extractBefore(MMN2.File_Name,'_');
CPT2.File_Number = extractBefore(CPT2.File_Name,'_');

% Merge the datasets by File_Number using outerjoin
mergedData = outerjoin(EO2, EC2, 'Keys', 'File_Number', 'MergeKeys', true);
mergedData = outerjoin(mergedData, MMN2, 'Keys', 'File_Number', 'MergeKeys', true);
mergedData = outerjoin(mergedData, CPT2, 'Keys', 'File_Number', 'MergeKeys', true);

% Remove duplicate rows caused by merging
mergedData = unique(mergedData, 'rows', 'stable');

% Reorder columns to match the desired output
mergedData = mergedData(:, {'Date', 'File_Number', 'EO', 'EC', 'MMN', 'CPT'});

% Display the final merged table
disp(mergedData);
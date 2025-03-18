% Set Pathway to your ONR_MBAP GitHub Repository
KBB = 'C:\Users\KBB DATA ENTRY\Documents\GitHub\KBB\';

% Set pathway to run EEGLAB
EEGLAB_Path = 'C:\Users\KBB DATA ENTRY\Documents\MATLAB\eeglab_current\eeglab2024.0';
addpath(EEGLAB_Path)
eeglab

% Set Pathway to customized functions
Functions_path = append(KBB,'\Preprocessing\GeneralFunctions\');
addpath(Functions_path)

% Set Pathway to KBB main scoring functions
GenFunctions_path = append(KBB,'\Scoring\')
addpath(GenFunctions_path)

% Must specify TIMES_Server (Dependent on Symbolik Link)
Data_Location = append(KBB,'Data\');


% Obtain the number of available cores
numCores = feature('numcores');
disp(['Number of available cores: ', num2str(numCores)]);

% Start the recruting these cores
parpool(numCores); % Start a pool with 8 workers

% Set up parameters (where the raw data is)
EO_pathway = append(Data_Location, 'RAW_DATA\01_Eyes_Open_Inscapes\'); 
EC_pathway = append(Data_Location, 'RAW_DATA\02_Eyes_Closed\'); 
MMN_pathway = append(Data_Location, 'RAW_DATA\03_MMN_Inscapes\'); 
CPT_pathway = append(Data_Location, 'RAW_DATA\04_CPT_Inscapes\'); 

% Set up parameters (where the cleaned data will be saved)
EO_savePathway = append(Data_Location, 'MODIFIED_DS\01_Eyes_Open_Inscapes\'); 
EC_savePathway = append(Data_Location, 'MODIFIED_DS\02_Eyes_Closed\');
MMN_savePathway = append(Data_Location, 'MODIFIED_DS\03_MMN_Inscapes\');
CPT_savePathway = append(Data_Location, 'MODIFIED_DS\04_CPT_Inscapes\');

% Set up parameters for saving QC measures
EO_CSVsavePathway = append(Data_Location, 'REPORTS\01_Eyes_Open_Inscapes\');
EC_CSVsavePathway = append(Data_Location, 'REPORTS\02_Eyes_Closed\');
MMN_CSVsavePathway = append(Data_Location, 'REPORTS\03_MMN_Inscapes\');
CPT_CSVsavePathway = append(Data_Location, 'REPORTS\04_CPT_Inscapes\');

% Set up the pathway where you want the combined QC measures to be saved
Combined_QC_savePathway = append(Data_Location, 'REPORTS\');

% Load in the EEG file names (not full pathway)
EO_EEG = load_EEG_names(EO_pathway, '.vhdr', 'no');
EC_EEG = load_EEG_names(EC_pathway, '.vhdr', 'no');
MMN_EEG = load_EEG_names(MMN_pathway, '.vhdr', 'no');
CPT_EEG = load_EEG_names(CPT_pathway, '.vhdr', 'no');

% Filter the incorrecct EEG file names from the correct ones
[EO_EEG, EO_error] = filter_bad_EEG_names(EO_EEG, '_EO.vhdr');
[EC_EEG, EC_error] = filter_bad_EEG_names(EC_EEG,'_EC.vhdr');
[MMN_EEG, MMN_error] = filter_bad_EEG_names(MMN_EEG, '_MMN.vhdr');
[CPT_EEG, CPT_error] = filter_bad_EEG_names(CPT_EEG, '_CPT.vhdr');

% Cleaning the EEG
CleaningRawEEG2(EO_pathway, EO_EEG, 'EO', EO_savePathway, EO_CSVsavePathway, 500);
CleaningRawEEG2(EC_pathway, EC_EEG, 'EC', EC_savePathway, EC_CSVsavePathway, 500);
CleaningRawEEG2(MMN_pathway, MMN_EEG, 'MMN', MMN_savePathway, MMN_CSVsavePathway, 250);
CleaningRawEEG2(CPT_pathway, CPT_EEG, 'CPT', CPT_savePathway, CPT_CSVsavePathway, 250);

% Take all the EEG QC Reports and save them inton one excel file
combining_EEG_QC(EO_CSVsavePathway, '01_Eyes_Open_Inscapes_Report.xlsx', Combined_QC_savePathway);
combining_EEG_QC(EC_CSVsavePathway, '02_Eyes_Closed_Report.xlsx', Combined_QC_savePathway);
combining_EEG_QC(MMN_CSVsavePathway, '03_MMN_Inscapes_Report.xlsx', Combined_QC_savePathway);
combining_EEG_QC(CPT_CSVsavePathway, '04_CPT_Inscapes_Report.xlsx', Combined_QC_savePathway);

% Release the recruited cores
delete(gcp('nocreate'))

% Generate a scoring report
EEGCleaningReport

% Print out status on EEG cleaning
{EO_error, EC_error, MMN_error, CPT_error}'
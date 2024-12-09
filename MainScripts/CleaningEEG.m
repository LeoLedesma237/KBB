% Set Pathway to your ONR_MBAP GitHub Repository
KBB = 'C:\Users\KBB DATA ENTRY\Documents\GitHub\KBB\';

% Set pathway to run EEGLAB
EEGLAB_Path = 'C:\Users\KBB DATA ENTRY\Documents\MATLAB\eeglab_current\eeglab2024.0';
addpath(EEGLAB_Path)
eeglab

% Set Pathway to customized functions
Functions_path = append(KBB,'\Preprocessing\GeneralFunctions\');
addpath(Functions_path)

% Must specify TIMES_Server (Dependent on Symbolik Link)
Data_Location = append(KBB,'Data\');

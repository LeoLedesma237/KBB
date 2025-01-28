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


% Obtain the number of available cores
numCores = feature('numcores');
disp(['Number of available cores: ', num2str(numCores)]);

% Start the recruting these cores
parpool(numCores); % Start a pool with 8 workers


%%%%% Cleaning the EEG
CleaningRawEEG(Data_Location, 1, 500)

CleaningRawEEG(Data_Location, 2, 500)

CleaningRawEEG(Data_Location, 3, 250)

CleaningRawEEG(Data_Location, 4, 250)



% Release the recruited cores
delete(gcp('nocreate'))


%%%%%% Delete After Testing %%%%%%%%
% Set Pathway to your ONR_MBAP GitHub Repository
%KBB = 'C:\Users\lledesma.TIMES\Documents\GitHub\KBB\';

% Set pathway to run EEGLAB
%EEGLAB_Path = 'C:\Users\lledesma.TIMES\Documents\MATLAB\eeglab2024.2';
%addpath(EEGLAB_Path)
%eeglab

% Set Pathway to customized functions
%Functions_path = append(KBB,'\Preprocessing\GeneralFunctions\');
%addpath(Functions_path)

% Must specify TIMES_Server (Dependent on Symbolik Link)
%Data_Location = append('C:\Users\lledesma.TIMES\Documents\KBB\Data\');


%%%%%%%%%%%%%%%%%%%% CONFIGURATION NEEDED %%%%%%%%%%%%%%%%%%%%%
% Set a KBB pathway - this is one of the few pathaways that changes across
% PCs
KBB = 'C:\Users\lledesma\Documents\';

% Set pathway to run EEGLAB
EEGLAB_Path = append(KBB, 'MATLAB\eeglab2024.2');
addpath(EEGLAB_Path)
eeglab


% Set Pathway to KBB main scoring functions
EEGFUN_path = append(KBB, 'GitHub\EEG\EEG_Cleaning\dry_ERP');
EEGFUN2_path = append(KBB, 'GitHub\EEG\EEG_Cleaning\dry_rsEEG');
EEGFUN3_path = append(KBB, 'GitHub\EEG\generalfun');
EEGFUN4_path = append(KBB, 'GitHub\ONR_MBAP\Preprocessing\CleaningEEG');
Functions_path = append(KBB, 'GitHub\KBB\Preprocessing\GeneralFunctions\');
GenFunctions_path = append(KBB, 'GitHub\KBB\Scoring\');
addpath(EEGFUN_path)
addpath(EEGFUN2_path)
addpath(EEGFUN3_path)
addpath(EEGFUN4_path)
addpath(GenFunctions_path)
addpath(Functions_path)

%%%%%%%%%% Copy Files from the Server to the Local PC %%%%%%%%%%%%%%%

% Set up parameters
%EEG_Type = {'.eeg', '.vhdr', '.vmrk'};
%EEG_Pathway = 'C:\Users\lledesma\Documents\KBB\Data\RAW_DATA\01_Eyes_Open_Inscapes\';
%EEG_save_path = 'C:\Users\lledesma\Documents\KBB\Data\MODIFIED_DS\01_Eyes_Open_Inscapes\';

% Copy and paste over the files
%copy_paste_EEG_files(EEG_Type, EEG_Pathway, EEG_save_path)

%%%%%%%%%%%%%%%%%%%%%%%% Clean the rsEEG data %%%%%%%%%%%%%%%%%%%%%%%
% Load in the dry-EEG preprocessing parameters (works for rsEEG and ERP)
DryEEGParameters 

% Set up some parameters for each condition
rsEEGDestination = {'01_Eyes_Open_Inscapes\', '02_Eyes_Closed\'}; 

for ii = 1:length(rsEEGDestination)

    % Set up pathways 
    EEG_Pathway = append(KBB, 'KBB\Data\RAW_DATA\', rsEEGDestination{ii});
    EEG_save_path = append(KBB, 'KBB\Data\MODIFIED_DS\', rsEEGDestination{ii});
    EEG_csv_save_path = append(KBB, 'KBB\Data\REPORTS\', rsEEGDestination{ii});
    batchSize = 15; 

    % Current three folders
    folders = {EEG_Pathway, EEG_save_path, EEG_csv_save_path};

    % Quality Control
    for k = 1:numel(folders)
        thisFolder = folders{k};
    
        if isfolder(thisFolder)
            fprintf('✓  Found:  %s\n', thisFolder);
        else
            warning('✗  Folder not found:  %s', thisFolder);
        end
    end

    % Load in .vhdr files in the Raw folder
    eegFiles = load_EEG_names(EEG_Pathway,'.vhdr','no');

    % filter bad EEG names and produce warning
    [eegFiles, errorMessage] = filter_bad_EEG_names(eegFiles, {'_EO.vhdr', '_EC.vhdr'})

    % Load in already processed files and remove them from eegFiles
    processed_eegFiles = load_EEG_names(EEG_save_path,'.set','no');
    eegFiles = eegFiles(~ismember(eegFiles, replace(processed_eegFiles, '_cleaned_dry.set','.vhdr')));

    % Clean the EEG data with the set parameters
    clean_dry_rseeg(eegFiles, ...
        EEG_Pathway, ...
        EEG_save_path , ...
        EEG_csv_save_path, ...
        preprocParams, ...
        batchSize)
end 

%%%%%%%%%%%%%%%%%%%%%%%% Clean the ERP data %%%%%%%%%%%%%%%%%%%%%%%
% Set up some parameters for each condition
ERPDestiation = {'03_MMN_Inscapes\', '04_CPT_Inscapes\'};

for ii = 1:length(ERPDestiation)

    % Set up pathways 
    EEG_Pathway = append(KBB, 'KBB\Data\RAW_DATA\', ERPDestiation{ii});
    EEG_save_path = append(KBB, 'KBB\Data\MODIFIED_DS\', ERPDestiation{ii});
    EEG_csv_save_path = append(KBB, 'KBB\Data\REPORTS\', ERPDestiation{ii});
    batchSize = 15; 

    % Current three folders
    folders = {EEG_Pathway, EEG_save_path, EEG_csv_save_path};

    % Quality Control
    for k = 1:numel(folders)
        thisFolder = folders{k};
    
        if isfolder(thisFolder)
            fprintf('✓  Found:  %s\n', thisFolder);
        else
            warning('✗  Folder not found:  %s', thisFolder);
        end
    end

    % Load in .vhdr files in the Raw folder
    eegFiles = load_EEG_names(EEG_Pathway,'.vhdr','no');

    % filter bad EEG names and produce warning
    [eegFiles, errorMessage] = filter_bad_EEG_names(eegFiles, {'_MMN.vhdr', '_CPT.vhdr'})

    % Load in already processed files and remove them from eegFiles
    processed_eegFiles = load_EEG_names(EEG_save_path,'.set','no');
    eegFiles = eegFiles(~ismember(eegFiles, replace(processed_eegFiles, '_cleaned_dry.set','.vhdr')));


    % Clean the EEG data with the set parameters (THIS FOR ERPS)
    clean_dry_erp(eegFiles, ...
        EEG_Pathway, ...
        EEG_save_path , ...
        EEG_csv_save_path, ...
        preprocParams, ...
        batchSize)
end 


%%%%%%%%%%%%% Combining Individual Reports into Main Ones %%%%%%%%%%%%%%%%%%
% Create an array to store the main_QC_Dry_name
main_QC_dry_name = {};

% Set parameters
Destinations = [rsEEGDestination ERPDestiation];

for ii = 1:length(Destinations)
    % Setting pathways to where reports are saved for each task 
    Name = replace(Destinations{ii},'\','');
    main_QC_dry_name{ii} = append(KBB, 'KBB\Data\REPORTS\', Name, '.xlsx');
    EEG_csv_save_path = append(KBB, 'KBB\Data\REPORTS\', Destinations{ii});
    
    % Organize the EEG QC reports
    T1 = organizing_QC(EEG_csv_save_path, 'dryEEG.csv');
    T2 = struct2log(preprocParams);
    
    % Save the table as an Excel file
    writetable(T1, main_QC_dry_name{ii});
    writecell(T2, main_QC_dry_name{ii}, 'Sheet', 'Sheet2');
end

%%%%%%%%%%%% Categorize Recordings as Good or Bad %%%%%%%%%%%%%%%%
% Create arrays to keep the outputs
Good_Recordings_raw = {};
Bad_Recordings_raw = {};
Quality = {};

for ii = 1:length(main_QC_dry_name)
    % Read in the main QC excel  
    current_main_QC_dry = readtable(main_QC_dry_name{ii});
    
    % Set parameters
    BadChnThres = 5;
    ASRIntrpThres = .30;
    AvgAmp2Thres = 500;
    
    % Identify the good and bad files 
    [Good_Recordings_raw{ii}, Bad_Recordings_raw{ii}, Quality{ii}] = check_dry_EEG_Quality(current_main_QC_dry, BadChnThres, ASRIntrpThres, AvgAmp2Thres);

end


% Combine all good quality recordings 
nonEmptyGood   = Good_Recordings_raw(~cellfun('isempty',Good_Recordings_raw)); 
All_Good_recordings   = vertcat(nonEmptyGood{:});

% Combine all the bad quality recordings 
nonEmptyBad   = Bad_Recordings_raw(~cellfun('isempty',Bad_Recordings_raw)); 
All_Bad_recordings   = vertcat(nonEmptyBad{:});

%%%%%%%%% Create a Comprehensive Table of Good and Bad Recordings %%%%%
% Pool lists & tag them 
allFiles   = [All_Good_recordings ; All_Bad_recordings];
isGoodFlag = [true(size(All_Good_recordings)); false(size(All_Bad_recordings))];   % 1 = good, 0 = bad

% Pull out the ID and the task from each filename
n = numel(allFiles);
IDs   = strings(n,1);
tasks = strings(n,1);

for k = 1:n
    noExt        = erase(allFiles{k},'.vhdr');       % drop extension
    lastUnders   = find(noExt=='_',1,'last');
    IDs(k)       = extractBefore(noExt, lastUnders); % e.g. '0919'
    tasks(k)     = extractAfter(noExt, lastUnders);  % e.g. 'MMN'
end

% create the empty table (rows = IDs, cols = task types)
taskNames  = ["EO","EC","MMN","CPT"];
uniqueIDs  = unique(IDs, 'stable');                  % keep original order
tbl = array2table( nan(numel(uniqueIDs), numel(taskNames)), ...
                   'VariableNames', taskNames, ...
                   'RowNames',     cellstr(uniqueIDs) );
for k = 1:n
    r = find(uniqueIDs == IDs(k));
    c = find(taskNames == tasks(k));
    tbl{r,c} = double(isGoodFlag(k));                % 1 = good, 0 = bad
end
disp(tbl)

% Save the output
tbl_name = 'Main_QS_Report.xlsx';
writetable(tbl, append(KBB, 'KBB\Data\REPORTS\', tbl_name), 'WriteRowNames', true);

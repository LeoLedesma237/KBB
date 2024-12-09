% 1. Set the pathway to the EEG data with markers that are synchronized to
% the EEG (originally called folder)
EEG_pathway = append(Data_Location, 'RAW_DATA\'); 

% 2. Set the pathway where the EEG with renamed markers will be saved
EEG_savePathway = append(Data_Location, 'MODIFIED_DS\'); 

% 3. Set the pathway to save the EEG Cleaning Outcome Tables (How much
% cleaning was done?)
CSV_savePathway = append(Data_Location, 'REPORTS\'); 

% % % % % % REMAINING CODE IS AUTOMATIC % % % % % % % % 
% % % % % % Part 1: Reading in all the files in specified folder % % % % %

% Define the folders to search
Condition_folders = {'01_Eyes_Open_Inscapes', 
                     '02_Eyes_Closed', 
                     '03_MMN_Inscapes',
                     '04_CPT_Inscapes'};

% Create a variable for the current condition pathway of interest
current_conditionPathway = append(EEG_pathway, Condition_folders{Condition});

% Extract the names of the files in this directory
allFiles = dir(current_conditionPathway);

% Identify the EEG files that ahve synchronized marker information
AllEEG_Condition = {allFiles(contains({allFiles.name}, ".set")).name};



% Create a variable for the current condition saved pathway
current_conditionSavedPathway = append(EEG_savePathway,Condition_folders{Condition})
Already_Processed = {dir(current_conditionSavedPathway).name};

% Remove files that have already been processed
AllEEG_Condition = AllEEG_Condition(~ismember(AllEEG_Condition, Already_Processed ));

% Problematic EEG files
filesToRemove = {'example1', 
                 'example2'};

% Remove problematic EEG files if any are present
eegFiles = AllEEG_Condition(~ismember(AllEEG_Condition, filesToRemove));


for ii = 1:length(eegFiles)

    % Load in the EEG File
    Current_eegFile = eegFiles{ii}

    %Import data - change the name of the ID
    EEG = pop_loadset('filename',Current_eegFile, ...
        'filepath', current_conditionPathway);
    [ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 0,'setname','ID#_Imported','gui','off');

    %Add channels
    EEG=pop_chanedit(EEG, 'lookup','C:\\Users\\lledesma.TIMES\\Documents\\MATLAB\\eeglab2022.0\\plugins\\dipfit\\standard_BEM\\elec\\standard_1005.elc');
    [ALLEEG EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);
    
    %Aux and Sample Counter removal
    EEG = pop_select( EEG, 'nochannel',{'AUX_1','AUX_2','AUX_3','SampleCounter'});
    
    %TRACKING: starting channel number and how long the EEG recording is
    StartingChannels = EEG.nbchan;
    
    %Filter the data 1 - 30 Hz
    EEG = pop_eegfiltnew(EEG, 'locutoff',1,'hicutoff',30);
    [ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'setname','Filtered','gui','off');
    

    %Checks for channels that need interpolation
    EEG1 = pop_clean_rawdata(EEG, 'FlatlineCriterion',5,'ChannelCriterion',0.8,'LineNoiseCriterion',4,'Highpass','off','BurstCriterion','off','WindowCriterion','off','BurstRejection','off','Distance','Euclidian');
    
    % The following if and else statement is IDs that have/do not have channels
    % that need interpolation. If they need interpolation they go into the if
    % statement. If not then they are directed to the code in the else.
    
    if length(fieldnames(EEG1.etc)) > 3
        
        %Returns the names of the channels that need to be interpolated
        Bad_Channels = find(EEG1.etc.clean_channel_mask == 0);
    
        %TRACKING: Number of channels that were interpolated
        Num_Interpolation = length(Bad_Channels);
    
        % Inteporlate the channels based on the object that we saved
        EEG = pop_interp(EEG, Bad_Channels', 'spherical');
    
    
    else
    
        % Returns zero for Bad_Channels
        Bad_Channels = 0;

        %TRACKING: Number of channels that were NOT interpolated
        Num_Interpolation = 0;
    
    end

    % Convert Bad Channels into a string variable
    BadChannelsStr = sprintf('%g, ', Bad_Channels); % Create a comma-separated string
    BadChannelsStr(end-1:end) = []; % Remove the trailing comma and space

    % Rereference to TP9 and TP10
    EEG = pop_reref( EEG, [10 21] );
    [ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 3,'setname','Re-referenced','gui','off'); 

    % Keep Sample at 500 Hz
    EEG = pop_resample( EEG, 500);
    [ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 4,'setname','500 Sampling Rate','gui','off');

    PCA_number = StartingChannels - Num_Interpolation - 2 % The minus two represents re-referencing to TP9 and TP10

    % Run ICA
    EEG = pop_runica(EEG, 'icatype', 'runica', 'extended',1,'pca',PCA_number,'interrupt','on');
    [ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 5,'setname','ICA-weights','gui','off');

    % Run MARA new function
    EEG = processMARA_single(EEG);

    % Run MARA
    [ALLEEG,EEG,CURRENTSET] = processMARA(ALLEEG,EEG,CURRENTSET)
    EEG = eeg_checkset( EEG );
    [ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 6,'setname','ran MARA','gui','off');

    % Choosing a threshold of 50 probability to identify component as an
    % artifact
    EEG.reject.gcompreject = zeros(size(EEG.reject.gcompreject)); 
    EEG.reject.gcompreject(EEG.reject.MARAinfo.posterior_artefactprob > 0.50) = 1;

    % Record number of components flagged for rejection
    Rejected_Component = find(EEG.reject.gcompreject == 1);
    RejectedComponentNum = length(Rejected_Component);
    
    % Reject the flagged components
    EEG = pop_subcomp(EEG, Rejected_Component, 0);
    
    % Convert components_to_reject into a string variable
    CompRejsStr = sprintf('%g, ', Rejected_Component); % Create a comma-separated string
    CompRejsStr(end-1:end) = []; % Remove the trailing comma and space


    % Create a table with the outputs of the cleaning process
    Output_Table = table( ...
        {Current_eegFile}, ...
        {Condition_folders{Condition}},...
        StartingChannels,...
        Num_Interpolation, ...
        {BadChannelsStr},...
        RejectedComponentNum,...
        {CompRejsStr},...
        'VariableNames', { ...
        'File_Name',...
        'GnG_Condition',...
        'Channel_Num',...
        'Interpolated_Chan_Num',...
        'Interpolated_Channels',...
        'Rejected_Components_Num',...
        'Rejected_Components'})

    % Save the file
    writetable(Output_Table, append(CSV_savePathway,strrep(Current_eegFile, ".set", ".csv")));

    % Saving the EEG data
    Save_FileName = Current_eegFile
    EEG = pop_saveset(EEG, 'filename',Save_FileName, ...
        'filepath',current_conditionSavedPathway);
    

end
% INPUT: 
% EEGPathway: Pathway that contains the EEG files are (ex: 'C:/user/EEG/RAW_DATA/')
% EEGFile: A cell with the EEG filenames (0001_EO.vhdr)
% Condition: Experimental condition (ex: 'EO'; 'EC'; 'MMN'; 'CPT')
% SavePathway: Pathway where the cleaned EEG data will be saved to
% CSVSavePathway: Pathway where the EEG cleaning QC report will be saved to
% SamplingRate : Used to downsample the data (ex: 500; 250)
%
%
% Description: This function will load and clean raw EEG data from BVR.
% It will also produce QC report for each file. 

function output = CleaningRawEEG2(EEGPathway, EEGFile, Condition, SavePathway, CSVSavePathway, SamplingRate)

    %%%%%
    %%%%%%%% Part 1: Removing Processed Files From EEG Cleaning Pipeline 
    %%%%
    
    % Create a variable for the current condition saved pathway
    Already_Processed = {dir(SavePathway).name};
    
    % Remove files that have already been processed
    EEGneedsProcessing = EEGFile(~ismember(EEGFile, strrep(Already_Processed,'.set' ,'.vhdr')));
    
    % Problematic EEG files
    filesToRemove = {'example1', 
                     'example2'};
    
    % Remove problematic EEG files if any are present
    eegFiles = EEGneedsProcessing(~ismember(EEGneedsProcessing, filesToRemove));
    
    %%%%%
    %%%%%%%% Part 2: Setting up parameters for the QC report 
    %%%%

    % Set N, the number of iterations to do
    N = length(eegFiles);
    
    % Variables to be saved for each iteration
    FileDate = cell(1,N);
    InitialSec = zeros(1,N);
    StartingChannels = zeros(1,N);
    AfterASRSec = zeros(1,N);
    rank1 = zeros(1,N);
    Num_Interpolation = zeros(1,N);
    BadChannelsString = cell(1,N);
    rank2 = zeros(1,N);
    PCA_number = zeros(1,N);
    RejectedComponentNumber = zeros(1,N);
    CompRejsString = cell(1,N);
    RemainingSec = zeros(1,N);
    Percent_Remaining = zeros(1,N);

    %%%%%
    %%%%%%%% Part 3: Cleaning the EEG data
    %%%%
    
    parfor ii = 1:N
        try
            
            % Load in the EEG File
            Current_eegFile = eegFiles{ii}
       
            %Import data - change the name of the ID
            EEG = pop_loadbv(EEGPathway, ...
                Current_eegFile);
    
            % Obtain date information from the current file
            fileInfo = dir(fullfile(EEGPathway, Current_eegFile));
        
            % Store date in FileDate, use indexed assignment
            FileDate{ii} = {fileInfo.date};

            % Create the existence of this variable
            MMN2_starts_inx = {};
    
            % % % % Changing EEG.data size depending on condition % % % %
            if strcmp(Condition, 'MMN')
                % Remove dumb 'R  2' marker - 
                EEG.event(strcmp({EEG.event.type}, 'R  2')) = [];
    
                % Quick QC incase markers not present (recording is short)
                MMN2_starts_inx = find(strcmp({EEG.event.type}, 'S  4'), 1);
    
                % Identify start and stop latencies of the MMN
                MMN1_starts = EEG.event(2).latency; % Skips boundary
                MMN2_starts = EEG.event(MMN2_starts_inx).latency;
                MMN1_ends = EEG.event(MMN2_starts_inx -1).latency;
                MMN2_ends = EEG.event(end).latency;
        
                % Use the latency information to keep meaningful data
                % Extract MMN1 and MM2 (with some wiggle room (wr))
                wr = EEG.srate;
                EEG1 = pop_select(EEG, 'point', [MMN1_starts-wr MMN1_ends+wr]);
                EEG2 = pop_select(EEG, 'point', [MMN2_starts-wr MMN2_ends+wr]);
                
                % Merge both segments
                EEG = pop_mergeset(EEG1, EEG2);
    
            elseif strcmp(Condition, 'CPT')
                % Idnetify start and stop latencies of the CPT
                CPT_starts = EEG.event(2).latency; % Skips boundary
                CPT_ends = EEG.event(end).latency
    
                % Use the latency information to keep meaningful data
                wr = EEG.srate;
                EEG = pop_select(EEG, 'point', [CPT_starts-wr CPT_ends+wr]);
                
                
            end
    
            % Save the intial length of the EEG recording
            EEG_size = size(EEG.data);
            Remaining_Samples = EEG_size(2);
            InitialSec(ii) = (Remaining_Samples/EEG.srate);
        
            %TRACKING: starting channel number and how long the EEG recording is
            StartingChannels(ii) = EEG.nbchan;
            
            %Filter the data 1 - 30 Hz
            EEG = pop_eegfiltnew(EEG, 'locutoff',1,'hicutoff',30);
            
            % Run ASR (same code as in flagging bad channels)
            EEG = pop_clean_rawdata(EEG, 'FlatlineCriterion','off','ChannelCriterion','off','LineNoiseCriterion','off','Highpass','off','BurstCriterion',20,'WindowCriterion','off','BurstRejection','on','Distance','Euclidian');
        
            % Segmentation after ASR
            EEG_size = size(EEG.data);
            Remaining_Samples = EEG_size(2);
            AfterASRSec(ii) = (Remaining_Samples/EEG.srate);
        
            % Save the EEG rank of the data
            rank1(ii) = rank(EEG.data);
        
            %Checks for channels that need interpolation and runs ASR
            EEG1 = pop_clean_rawdata(EEG, 'FlatlineCriterion',5,'ChannelCriterion',0.8,'LineNoiseCriterion',4,'Highpass','off','BurstCriterion',20,'WindowCriterion','off','BurstRejection','on','Distance','Euclidian');
            
            % The following if and else statement is IDs that have/do not have channels
            % that need interpolation. If they need interpolation they go into the if
            % statement. If not then they are directed to the code in the else.
            if  sum(ismember(fieldnames(EEG1.etc), 'clean_channel_mask')) == 1
                
                %Returns the names of the channels that need to be interpolated
                Bad_Channels = {find(EEG1.etc.clean_channel_mask == 0)};
            
                %TRACKING: Number of channels that were interpolated
                Num_Interpolation(ii) = length(Bad_Channels{1});
            
                % Inteporlate the channels based on the object that we saved
                EEG = pop_interp(EEG, Bad_Channels{1}', 'spherical');
            
            else
            
                % Returns zero for Bad_Channels
                Bad_Channels = {0};
        
                %TRACKING: Number of channels that were NOT interpolated
                Num_Interpolation(ii) = 0;
            
            end
        
            % Convert Bad Channels into a string variable
            BadChannelsStr = sprintf('%g, ', Bad_Channels{1}); % Create a comma-separated string
            BadChannelsStr(end-1:end) = []; % Remove the trailing comma and space
        
            % Save the Bad Channel information
            BadChannelsString{ii} = {BadChannelsStr};
        
            % Rereference to TP9 and TP10
            EEG = pop_reref( EEG, [10 21] );
        
            % Downsample if necessary
            EEG = pop_resample( EEG, SamplingRate);
        
            % Save the EEG rank of the data
            rank2(ii) = rank(EEG.data);
        
            % Calculate the number needed for PCA
            PCA_number(ii) = StartingChannels(ii) - Num_Interpolation(ii) - 2 % The minus two represents re-referencing to TP9 and TP10
        
            % Run ICA
            EEG = pop_runica(EEG, 'icatype', 'runica', 'extended',1,'pca',PCA_number(ii),'interrupt','on');
        
            % Run MARA new function
            EEG = processMARA_single(EEG);
        
            % Choosing a threshold of 50 probability to identify component as an
            % artifact
            EEG.reject.gcompreject = zeros(size(EEG.reject.gcompreject)); 
            EEG.reject.gcompreject(EEG.reject.MARAinfo.posterior_artefactprob > 0.50) = 1;
        
            % Record number of components flagged for rejection
            Rejected_Component = find(EEG.reject.gcompreject == 1);
            RejectedComponentNum = length(Rejected_Component);
        
            % Save this value
            RejectedComponentNumber(ii) = RejectedComponentNum;
            
            % Reject the flagged components
            EEG = pop_subcomp(EEG, Rejected_Component, 0);
            
            % Convert components_to_reject into a string variable
            CompRejsStr = sprintf('%g, ', Rejected_Component); % Create a comma-separated string
            CompRejsStr(end-1:end) = []; % Remove the trailing comma and space
        
            % Save the Bad Component information
            CompRejsString{ii} = {CompRejsStr};
        
            % Segmentation Rejection (100 microVolts)
            threshold_volt = 100;
            
            % Find columns to delete
            columnsToDelete = any(EEG.data >= threshold_volt | EEG.data <= threshold_volt*-1, 1);
            
            % Delete the selected columns
            EEG.data(:, columnsToDelete) = [];
            
            % Save the length of the EEG recording after segmentation rejection
            EEG_size = size(EEG.data);
            Remaining_Samples = EEG_size(2);
            RemainingSec(ii) = (Remaining_Samples/EEG.srate);
    
            % Obtain the percentage of the recording remaining
            Percent_Remaining(ii) = round(RemainingSec(ii)/InitialSec(ii)*100,2);
        
            % Create a table with the outputs of the cleaning process
            Output_Table = table( ...
                {FileDate{ii}'},...
                {Current_eegFile}, ...
                {Condition},...
                InitialSec(ii)',...
                AfterASRSec(ii)',...
                rank1(ii)',...
                StartingChannels(ii)',...
                Num_Interpolation(ii)', ...
                {BadChannelsString{ii}'},...
                rank2(ii),...
                PCA_number(ii),...
                RejectedComponentNumber(ii),...
                {CompRejsString{ii}'},...
                RemainingSec(ii),...
                Percent_Remaining(ii),...
                {'-'},...
                'VariableNames', { ...
                'Date',...
                'File_Name',...
                'EEG_Condition',...
                'Start_Recording_Sec',...
                'Recording_After_ASR_Sec',...
                'EEG_Rank1',...
                'Channel_Num',...
                'Interpolated_Chan_Num',...
                'Interpolated_Channels',...
                'EEG_Rank2',...
                'PCA_Number',...
                'Rejected_Components_Num',...
                'Rejected_Components',...
                'Remaining_Recording_Sec',...
                'Percent_Remaining',...
                'Error'})
        
            % Save the file
            writetable(Output_Table, append(CSVSavePathway,strrep(Current_eegFile, ".vhdr", ".csv")));
        
            % Saving the EEG data
            Save_FileName = strrep(Current_eegFile,'.vhdr','.set')
            EEG = pop_saveset(EEG, 'filename',Save_FileName, ...
                'filepath', SavePathway);
    
        catch ME
    
            % Save the error message as an object
            ErrorMessage = ME.message;
    
            % Save results only if the value hasn't already been set
            if isempty(MMN2_starts_inx)
                ErrorMessage = 'Marker S  4 not present in the data'
            end
            if isempty(FileDate{ii})
                FileDate{ii} = '-';
            end
            if isempty(InitialSec(ii))
                InitialSec(ii) = 0;
            end
            if isempty(AfterASRSec(ii))
                AfterASRSec(ii) = 0;
            end
            if isempty(rank1(ii))
                rank1(ii) = 0;
            end
            if isempty(StartingChannels(ii))
                StartingChannels(ii) = 0;
            end
            if isempty(Num_Interpolation(ii))
                Num_Interpolation(ii) = 0;
            end
            if isempty(BadChannelsString{ii})
                BadChannelsString{ii} = '-';
            end
            if isempty(rank2(ii))
                rank2(ii) = 0;
            end
            if isempty(PCA_number(ii))
                PCA_number(ii) = 0;
            end
            if isempty(RejectedComponentNumber(ii))
                RejectedComponentNumber(ii) = 0;
            end
            if isempty(CompRejsString{ii})
                CompRejsString{ii} = '-';
            end
            if isempty(RemainingSec(ii))
                RemainingSec(ii) = 0;
            end
            if isempty(Percent_Remaining(ii))
                Percent_Remaining(ii) = 0;
            end
            % Create the output table
            Output_Table = table( ...
                {FileDate{ii}'},...
                {Current_eegFile}, ...
                {Condition},...
                InitialSec(ii)',...
                AfterASRSec(ii)',...
                rank1(ii)',...
                StartingChannels(ii)',...
                Num_Interpolation(ii)', ...
                {BadChannelsString{ii}'},...
                rank2(ii),...
                PCA_number(ii),...
                RejectedComponentNumber(ii),...
                {CompRejsString{ii}'},...
                RemainingSec(ii),...
                Percent_Remaining(ii),...
                {ErrorMessage}',...
                'VariableNames', { ...
                'Date',...
                'File_Name',...
                'EEG_Condition',...
                'Start_Recording_Sec',...
                'Recording_After_ASR_Sec',...
                'EEG_Rank1',...
                'Channel_Num',...
                'Interpolated_Chan_Num',...
                'Interpolated_Channels',...
                'EEG_Rank2',...
                'PCA_Number',...
                'Rejected_Components_Num',...
                'Rejected_Components',...
                'Remaining_Recording_Sec',...
                'Percent_Remaining',...
                'Error'})
    
            % Save the file
            writetable(Output_Table, append(CSVSavePathway,'\' ,strrep(Current_eegFile, "vhdr", ".csv")));
        
        end
        
    end

end

% INPUT: 
% eegfiles: eeg files without full pathway (.set)
% EEG_Pathway: The directory to where the cleaned ERP files are
% EEG_Bin_Path: The location you want the Bin (trial type) info to be saved
% EEG_Bin_Name: The name of the Bin file (.txt)
% EEG_ERP_csv_save_path: The location you want the EEG QC CSV to be saved
%
% Description: This code will process MMN data from the KBB Project by
% renaming the markers to numbers, aggregating markers together to generate
% ERPs and then saving that information into a .csv file, each ERP will be
% saved in their own .csv file accordingly.

% For testing purposes, delete later
% eegFiles = '1141_CPT_cleaned_dry.set'
% EEG_Pathway = 'C:\Users\lledesma.TIMES\Documents\KBB\Data\MODIFIED_DS\04_CPT_Inscapes\01_Cleaned_ERP_Data\'
% EEG_Bin_Path = 'C:\Users\lledesma.TIMES\Documents\KBB\Data\MODIFIED_DS\04_CPT_Inscapes\02_Bin_Information\'
% EEG_Bin_Name = 'CPT_Bin.txt'
% EEG_ERP_csv_save_path = 'C:\Users\lledesma.TIMES\Documents\KBB\Data\MODIFIED_DS\04_CPT_Inscapes\03_ERP_CSVs\'

function CPT_ProcessingERP(eegFiles, EEG_Pathway, EEG_Bin_Path, EEG_Bin_Name, EEG_ERP_csv_save_path)
    
    % Load in an ERP dataset
    EEG = pop_loadset('filename', eegFiles, ...
                      'filepath', EEG_Pathway);

    % Check the number of markers
    pop_squeezevents(EEG)

    % We need to delete all 'R  2' that have the same latency as a stimulus
    stimulus_idx = strcmp({EEG.event.code}, 'Stimulus');
    stimulus_latencies = [EEG.event(stimulus_idx).latency];
    r2_idx = strcmp({EEG.event.type}, 'R  2') & ismember([EEG.event.latency], stimulus_latencies);
    EEG.event(r2_idx) = [];

    % Check the number of markers
    pop_squeezevents(EEG)

    % We only care about S 1, S 2, S 3, S 4, S 5, S 6, R 1, R 2
    EEG  = pop_creabasiceventlist( EEG , 'AlphanumericCleaning', 'on', ...
            'BoundaryNumeric', { -99   1   2   3   4  5  6  11  12}, ...
            'BoundaryString', { ...
            'boundary' ...
            'S  1' ... % Cross
            'S  2' ...
            'S  3' ... % Cross
            'S  4' ...
            'S  5' ... % Cross
            'S  6' ...
            'R  1' ... % Hit
            'R  2'}); % Not Hit

    % Check the number of markers
    pop_squeezevents(EEG)
    
    % Creating a Bin and save it as a text file 
    full_bin_name = append(EEG_Bin_Path, EEG_Bin_Name);
    fileID = fopen(full_bin_name, 'w');
    
    % Write bin 1 section
    fprintf(fileID, 'bin 1\n');
    fprintf(fileID, 'Standard_1_ISI\n');
    fprintf(fileID, '.{2}{11}\n\n'); % Standard followed by hit (correct)
    
    % Write bin 2 section
    fprintf(fileID, 'bin 2\n');
    fprintf(fileID, 'Standard_2_ISI\n');
    fprintf(fileID, '.{4}{11}\n\n'); % Standard followed by hit (correct)

    % Write bin 3 section
    fprintf(fileID, 'bin 3\n');
    fprintf(fileID, 'Standard_4_ISI\n');
    fprintf(fileID, '.{6}{11}\n\n'); % Standard followed by hit (correct)
    
    % Write bin 4 section
    fprintf(fileID, 'bin 4\n');
    fprintf(fileID, 'Deviant_1_ISI\n');
    fprintf(fileID, '.{1}{12}\n\n'); % Deviant followed by not hit (correct)
    
    % Write bin 5 section
    fprintf(fileID, 'bin 5\n');
    fprintf(fileID, 'Deviant_2_ISI\n');
    fprintf(fileID, '.{3}{12}\n\n'); % Deviant followed by not hit (correct)

    % Write bin 6 section
    fprintf(fileID, 'bin 6\n');
    fprintf(fileID, 'Deviant_4_ISI\n');
    fprintf(fileID, '.{5}{12}\n\n'); % Deviant followed by not hit (correct)
    
    % Close the file
    fclose(fileID);
    
    % Load in the Color and Shape Bin
    EEG  = pop_binlister( EEG , 'BDF', full_bin_name,...
                'IndexEL',  1, 'SendEL2', 'EEG', 'Voutput', 'EEG' );
    
    % Extract the epochs -100 to 800 ms
    segmented_EEG = pop_epochbin( EEG , [-100.0  800.0],  'pre');
    
    % Compute averaged ERPs
    ERP = pop_averager( segmented_EEG , 'Criterion', 'all', 'DQ_custom_wins', 0, 'DQ_flag', 1, 'DQ_preavg_txt', 0, 'ExcludeBoundary', 'on', 'SEM', 'on' );
    
    % Export the ERPs as a text file for specified trials
    pop_export2text(ERP, append(EEG_ERP_csv_save_path, erase(eegFiles, '_cleaned_dry.set'),'_ERPs.txt'), ...
        [1 2 3 4 5 6], 'electrodes', 'on', 'precision',  4, 'time', 'on',...
        'timeunit',  0.001, 'transpose', 'on' );

end


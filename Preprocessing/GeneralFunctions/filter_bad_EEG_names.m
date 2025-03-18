% INPUT: 
% EEG_names: EEG pathway and file names together (ex: 'C:/user/EEG/001_GnG.eeg')
% desired_extension: the correct file name for the task (ex: _EO; _EC;
% _MMN; _CPT)
%
% OUTPUT
% correctEEG: The EEG files named correctly
% removedFILE: Incorrect EEG file names
%
% Description: This function separates correctly named from incorrectly
% named EEG filenames.

function [correctEEG, outputMessage] = filter_bad_EEG_names(EEG_names, desired_extension)
    
    % Use contains to check if each file name has the desired extension
    correctEEG = EEG_names(contains(EEG_names, desired_extension));
    
    % Get the removed file names
    outputMessage = EEG_names(~contains(EEG_names, desired_extension));

    if isempty(outputMessage)

        % make a statement that everything was successful
        outputMessage = append('All ',desired_extension, ' files were named correctly');

    else
        % Add a warning that these files need to be fixed
        outputMessage = append('Warning: at least one EEG file was not named correctly! Must be fixed for them to be cleaned');

    end
       
end
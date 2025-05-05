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

    % allow a single string or a cell array of strings
    if ischar(desired_extension) || isstring(desired_extension)
        desired_extension = {desired_extension};
    end

    keep = false(size(EEG_names));
    for k = 1:numel(desired_extension)
        keep = keep | contains(EEG_names, desired_extension{k});
    end

    correctEEG     = EEG_names(keep);
    outputMessage  = EEG_names(~keep);

    if isempty(outputMessage)
        outputMessage = 'All files were named correctly';
    else
        outputMessage = 'Warning: at least one EEG file was not named correctly! Must be fixed for them to be cleaned';
    end
end
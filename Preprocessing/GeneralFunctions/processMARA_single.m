function EEG = processMARA_single(EEG, options)

    if isempty(EEG.chanlocs)
        error('No channel locations. Aborting MARA.');
    end

    if nargin < 2 || isempty(options)
        options = [0, 0, 0, 0, 0];
    end

    %% Filter the data
    if options(1) == 1
        disp('Filtering data');
        [EEG, LASTCOM] = pop_eegfilt(EEG);
    end

    %% Run ICA
    if options(2) == 1
        disp('Running ICA');
        [EEG, LASTCOM] = pop_runica(EEG);
    end

    %% Check if ICA components are present
    [EEG, LASTCOM] = eeg_checkset(EEG, 'ica');
    if LASTCOM < 0
        disp('No ICA components present. Aborting classification.');
        return;
    end

    %% Classify artifactual components with MARA
    [artcomps, MARAinfo] = MARA(EEG);
    EEG.reject.MARAinfo = MARAinfo;
    disp('MARA marked the following components for rejection:');
    disp(artcomps);

    if isempty(EEG.reject.gcompreject)
        EEG.reject.gcompreject = zeros(1, size(EEG.icawinv, 2));
    end
    EEG.reject.gcompreject(artcomps) = 1;

    %% Display components to label them for rejection
    if options(3) == 1
        if isempty(artcomps)
            answer = questdlg('MARA identified no artifacts. Do you want to visualize components?', ...
                              'No artifacts identified', 'Yes', 'No', 'No');
            if strcmp(answer, 'No')
                return;
            end
        end
        [EEG, LASTCOM] = pop_selectcomps_MARA(EEG);
        if options(4) == 1
            pop_visualizeMARAfeatures(EEG.reject.gcompreject, EEG.reject.MARAinfo);
        end
    end

    %% Automatically remove artifacts
    if options(5) == 1 && ~isempty(artcomps)
        try
            [EEG, LASTCOM] = pop_subcomp(EEG);
            disp('Artifact rejection completed.');
        catch
            error('Error during artifact rejection.');
        end
    end

end

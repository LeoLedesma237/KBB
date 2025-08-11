% For more information visit: https://github.com/Private-Projects237/EEG/wiki/Developing-a-dry%E2%80%90EEG-cleaning-pipeline
% Load these dryEEG parameters
preprocParams = struct();

% === File Naming ===
preprocParams.fileExt.eeg = '_cleaned_dry.set'; % Suffix for processed EEG file
preprocParams.fileExt.qc = '_QC_dry.csv';        % Suffix for QC report

% == For ERPs === (not necessary for rsEEG)
preprocParams.markerNames = {'S  1', 'S  2', 'S  3', 'S  4', 'S  5', 'S  6'};
preprocParams.markerLatThresh = 1000;
preprocParams.baselineLat = 100; % The very first marker type!
preprocParams.postStiLat = 900; % The very last marker type!

% === Filtering ===
preprocParams.filt.low = 1;
preprocParams.filt.high = 30;

% === Bad Channel Detection ===
preprocParams.badCh.corrThresh = 0.70; % Main bad channel detection parameter
preprocParams.badCh.LineNoiThresh = 4;
preprocParams.badCh.WinLen = 10;

% === Variance-Based Bad Channel Detection ===
preprocParams.badCh.varWinLen = 5;  % Window length for variance detection
preprocParams.badCh.varSDThresh = 4; % Standard deviation threshold

% === ASR ===
preprocParams.ASR.cutoff = 5; % Main segment rejection/reconstruction parameter
preprocParams.ASR.winLen = 0.5; % Leaving it at 0.5 for simplicity
preprocParams.ASR.stpSz = []; 
preprocParams.ASR.maxDim = 2/3;
preprocParams.ASR.ref_maxBadChn = 0.075;
preprocParams.ASR.ref_tol = [-3.5 5,5];
preprocParams.ASR.ref_wndlen = 1;

% === ASR Burts ===
preprocParams.ASR.burstCrit = 20; 

% === Re-referencing ===
preprocParams.reRef.chan = [10, 20]; % Mastoids

% === Downsampling ===
preprocParams.down.rate = 500; % High sampling rate results in good ICA

% === ICA ===
preprocParams.ICA.ext = 1;
preprocParams.ICA.lrate = 5e-5; % Highly recommended
preprocParams.ICA.steps = 2000; % Same here too 
preprocParams.ICA.stopTol = 1e-7;

% === IC Label ===
preprocParams.ICL.thresh = struct('eye', 0.8, 'muscle', 0.8, 'heart', 0.6, 'line', 0.8, 'chan', 0.8);

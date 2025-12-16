% This script will be preprocessing resting-state EEG (rsEEG) and ERP data from the
% KBB Project. This pipeline will be using a tested and approved pipeline
% that was used in the ONR Neuroimaging Pilot. In other words- this
% pipeline was designed to be robust to eyes open and eyes closed
% conditions in both wet and dry EEG. A pipeline for cleaning ERP data is 
% still under development. For the KBB project, each subject
% should at at most 4 EEG recordings: one eyes open rsEEG, one eyes closed
% rsEEG, one MMN task, and one CPT task. To preprocess this data, we
% will be using custom functions along with well known functions from
% FieldTrip. To run this code, you will need:
% a) to have FieldTrip installed
% b) to have Parallel Computing Toolbox installed
% c) to have Signal Processing Toolbox installed
% d) to have NoiseTools Toolbox installed
% e) to have access to Leo's EEG github repository
% f) to have downloaded the ColorBrewer functions
%
% Additionally, for conveniece (on this User's end), absolute pathways were
% used to specify raw and processed data locations. These will have to be
% modified if the code is to be used on a different account/PC.
%
% Preprocessing steps (rsEEG):
% 1) Robust detrending
% 2) Removing bad channels
% 3) High-pass filtering (Removes/attenuates extreme sweat artifacts)
% 4) Rejecting bad trials 
% 5) Rejecting bad trials + Fixing partially bad trials (high frq mostly)
% 6) Interpolating removed channels (from step 2)
% 7) Robust referencing (Whole head)
% 8) ICA/Blink Component Removal
% 9) Remove line noise (US = 60 Hz)
% 10) Rejecting bad trials + Fixing partially bad trials (low frq mostly)


% Clear all information from Workspace and Command Window
clc; clear;

% Add paths to custom funtions and start Field Trip
addpath 'C:\Users\lledesma.TIMES\Documents\MATLAB\fieldtrip-20250821';
addpath 'C:\Users\lledesma.TIMES\Documents\GitHub\EEG\FieldTripFun';
addpath 'C:\Users\lledesma.TIMES\Documents\MATLAB\NoiseTools';
addpath(genpath('C:\Users\lledesma.TIMES\Documents\GitHub\EEG\FieldTripFun'));

% Set up pathways to load the raw data
EORaw_pathway = 'C:\Users\lledesma.TIMES\Documents\KBB\Data\RAW_DATA\01_Eyes_Open_Inscapes';
ECRaw_pathway = 'C:\Users\lledesma.TIMES\Documents\KBB\Data\RAW_DATA\02_Eyes_Closed';

% Set up pathways to save processed data (.mat files)
eegProcessed_pathway = 'C:\Users\lledesma.TIMES\Documents\KBB\Data\MODIFIED_DS\EEG';

% Set up pathway to store a CSV with all preprocessed information (.summary info)
CSV_pathway = 'C:\Users\lledesma.TIMES\Documents\KBB\Data\REPORTS\EEG';

% Set up pathways to save processing reports (.htmls) and PNGs
HTML_pathway   = fullfile(CSV_pathway, "html");   
PNG_pathway    = fullfile(HTML_pathway, "PNGs");
ERRORS_pathway = fullfile(HTML_pathway, "errors");

% Raw, processed, and error file extensions
raw_ext = '.vhdr';

beforeICA_ext = '_beforeICA.mat';
error_beforeICA_ext = strrep(beforeICA_ext, '.mat', '_failed.mat');
error_beforeICA_ext_csv = strrep(beforeICA_ext, '.mat', '_failed.csv');

procc_ext = '_preproc.mat';
error_procc_ext = strrep(procc_ext, '.mat', '_failed.mat');
error_procc_ext_csv = strrep(procc_ext, '.mat', '_failed.csv');

%%%%%%%%%%%%%%%%%%%%                        %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%                        %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%      REST OF THE CODE IS AUTOMATIC      %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%                        %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%                        %%%%%%%%%%%%%%%%%%%%

% Load in FieldTrip functions
ft_defaults;

% Load configuration for plot visualizations
[cfg_view, cfg_time, cfg_fft, cfg_topo, cfg_sum, cfg_stack, cfg_chntrfft] = ft_visualizationcfgs();

% Create configuration for saving PNGs (Will be used later for generating report)
% Specify parameters for saving PNGs 
cfg_saveplots = [];
cfg_saveplots.visibleplots = 'no';
cfg_saveplots.saveplots    = 'yes';
cfg_saveplots.main         = 'no'; % Are they end results summary plots or not
cfg_saveplots.skip         = 8; % Number of starting summary plots
cfg_saveplots.plotfolder   = PNG_pathway; % This gets updated within the for loop

% Introduce the cfg_saveplots structure within our visualization plot functions
cfg_time.saveplots  = cfg_saveplots;
cfg_fft.saveplots   = cfg_saveplots;
cfg_topo.saveplots  = cfg_saveplots;
cfg_stack.saveplots = cfg_saveplots;
cfg_corchans.saveplots = cfg_saveplots; % This configuration is typically optional
cfg_chntrfft.saveplots = cfg_saveplots;

% Create directories if they already don't exist (for outputs)
mkdir(eegProcessed_pathway);
mkdir(CSV_pathway);
mkdir(HTML_pathway);
mkdir(PNG_pathway);
mkdir(ERRORS_pathway);

% Load in file names (wet & dry) split by pending and already processed
cfg = [];
cfg.inputdir1      = EORaw_pathway;
cfg.inputdir2      = ECRaw_pathway;
cfg.outputdir      = eegProcessed_pathway;
cfg.inputpattern1  = ['*', raw_ext];
cfg.inputpattern2  = ['*', raw_ext]; 
cfg.outputpattern  = ['*', beforeICA_ext];
cfg.fullname       = 'yes'; % Returns the pathway (makes easier to load data)

% Return files that have and have not been processed
[pendingRawNames, processedRawNames] = ft_notyetprocessed(cfg);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Setting Up Patallel Processing Parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete(gcp('nocreate'));        % Kills any old pool (12 workers or whatever)
tic;                                        
myCluster = parcluster('local');
delete(myCluster.Jobs);         % Clears old crash files
myCluster.NumWorkers = 2;       % More than this will cause par for to fail
parpool(myCluster);             % Starts the cluster with specified number of workers
fprintf('Successfully started clean pool with %d workers\n', myCluster.NumWorkers);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% PART 1 %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Prepare the batches (number of files processed at a time)
N = length(pendingRawNames);
batchSize = 30;
numBatches = ceil(N / batchSize);


% Set up a for loop first that sets up batches for parfor loop
for batch = 1:numBatches
    startIdx = (batch-1)*batchSize + 1;
    endIdx   = min(batch*batchSize, N);
    
    % Extract current batch of indices
    batchIndices = startIdx:endIdx;

   % Run parfor loop 
    parfor iter = batchIndices
        try
            % For each worker to produce the same results each time
            rng(iter);
    
            % Prepare names for files to be loaded and saved
            one_raw_full = pendingRawNames{iter}; 
            [~, name, ext] = fileparts(one_raw_full);
            one_raw = [name ext]; % regular name
            one_partpreproc = [name beforeICA_ext];
            error_mat = fullfile(eegProcessed_pathway, [name error_beforeICA_ext]);
            error_csv = fullfile(ERRORS_pathway, [name error_beforeICA_ext_csv]);       
            
            % Update the pathway to save PNGs and create it (delete if already present)
            % (This code can cause errors if Image opened in R Studio)!
            one_PNG_pathway = fullfile(PNG_pathway, name);
            if exist(one_PNG_pathway, 'dir'); rmdir(one_PNG_pathway, 's'); end
            mkdir(one_PNG_pathway);

            % Same as above delete error file if they already exists
            if isfile(error_mat); delete(error_mat); end 
            if isfile(error_csv); delete(error_csv); end 
           
            % Load in the EEG data (full name)
            cfg = [];
            cfg.dataset = one_raw_full; % Point to .vhdr
            cfg.continuous = 'yes';
            data_noisy = ft_preprocessing(cfg);
           
            % Segment the EEG data (for plotting purposes)
            cfg = [];
            cfg.length = 2; % seconds
            cfg.overlap = 0; % non-overlapping
            data_noisy_segmented = ft_redefinetrial(cfg, data_noisy);
           
           
            %%%%%%%%%%%%%% BEFORE STARTING THE PROCESSING PIPELINE %%%%%%%%%%%%%%
        
            % Save plotting configuration into new configurations (needed for parallel processing)
            cfg_time2     = cfg_time;
            cfg_fft2      = cfg_fft;
            cfg_topo2     = cfg_topo;
            cfg_sum2      = cfg_sum;
            cfg_stack2    = cfg_stack;
            cfg_corr2     = cfg_corchans;
            cfg_chntrfft2 = cfg_chntrfft;     
       
            % Update each plot configuration to include new save PNG pathway (needed for parallel processing)
            cfg_time2.saveplots.plotfolder = one_PNG_pathway;
            cfg_fft2.saveplots.plotfolder = one_PNG_pathway;
            cfg_topo2.saveplots.plotfolder = one_PNG_pathway;
            cfg_sum2.saveplots.plotfolder = one_PNG_pathway;
            cfg_stack2.saveplots.plotfolder = one_PNG_pathway;
            cfg_corr2.saveplots.plotfolder = one_PNG_pathway;
            cfg_chntrfft2.saveplots.plotfolder = one_PNG_pathway;
        
            % Add electrode positions into the data (10-05 template)
            elec_file = which('standard_1005.elc');
            data_noisy.elec = ft_read_sens(elec_file);
           
            % Define neighbors using the standard 10-05 template
            cfg = [];
            cfg.method = 'distance'; % or 'triangulation'
            cfg.layout = 'EEG1005.lay'; % Your layout file
            neighbours = ft_prepare_neighbours(cfg);
           
            % Generate the configuration structure
            data_noisy = ft_inittracker(data_noisy);
           
            %%% PLOTS: BEFORE DETRENDING %%%                                             
            cfg_stack2.trial = 7; ft_plotstack(cfg_stack2, data_noisy_segmented);  
            cfg_stack2.trial = 'all'; ft_plotstack(cfg_stack2, data_noisy);
        
            %%%%%%%%%%%%%%%%%%% PART 1: ROBUST DETREND THE DATA %%%%%%%%%%%%%%%%%%%%
            
            % Use custom robust detrending function
            cfg = [];
            cfg.robustdetrend.concatenate  = 'yes';
            cfg.robustdetrend.order        = 10;
            cfg.robustdetrend.demean       = 'yes';
            cfg.robustdetrend.log          = 'yes';
             
            % Robust Detrend the EEG data; % data = data_noisy
            data_detrended = ft_robustdetrend(cfg, data_noisy);
            stage = 'Part 1: Robust detrending was successful';
            temp_dat = data_detrended;
            
            % Segment the EEG data for plotting purposes
            cfg = [];
            cfg.length  = 2;        % segment length in seconds
            cfg.overlap = 0;        % 0 for non-overlapping (100% = fully overlapping)
            data_detrended_segmented = ft_redefinetrial(cfg, data_detrended);
            
            %%% PLOTS: AFTER DETRENDING AND TEMP SEGMENTING %%%
            ft_plotchannelavg(cfg_time2, data_detrended_segmented);
            ft_plotchannelfft(cfg_fft2, data_detrended_segmented);
            cfg_stack2.trial = 'all'; ft_plotstack(cfg_stack2, data_detrended);
            ft_corchans(cfg_corr2, data_detrended);
        
            %%%%%%%%%%%%%%%%%%%%%% RECORD EEG VARIANCE %%%%%%%%%%%%%%%%%%%%%%%
            
            % Create a configuration structure
            cfg = [];
            cfg.eegvarsum.log = 'yes';
            
            % Add variance information into the data
            data_detrended = ft_eegvarsum(cfg, data_detrended);
        
            %%%%%%%%%%%%%% PART 2: HIGH-PASS FILTERING THE DATA %%%%%%%%%%%%%%%
            
            % Create a structure to high-pass filter the data (rmv low frq)
            cfg = [];
            cfg.highpassfilt.hpfilter   = 'yes';
            cfg.highpassfilt.hpfreq     = 0.5; % 1 Hz is very slightly better
            cfg.highpassfilt.hpfiltord  = 4;
            cfg.highpassfilt.hpfiltype  = 'but';
            cfg.highpassfilt.hpfiltdir  = 'twopass';
            cfg.highpassfilt.log        = 'yes';
        
            cfg.highpassfilt.fullrecordingplots = 'yes';
        
            cfg.saveplots = cfg_saveplots; 
            cfg.saveplots.plotfolder = one_PNG_pathway;
        
            % High-pass filter the EEG
            data_filt = ft_highpassfilter(cfg, data_detrended);
            stage = 'Part 2: High-Pass Filtering was successful';
            temp_dat = data_filt;
            
            %%%%%%%%%%%%% PART 3: IDENTIFY AND REMOVE BAD CHANNELS %%%%%%%%%%%%%%
            
            % Create a structure to identify and remove bad channels
            cfg = [];
            cfg.removebadchann.concatenate = 'yes'; % combines all trials into one
            cfg.removebadchann.mthresh1 = 5; % 5x larger than median chan var to be an electrode pop
            cfg.removebadchann.mthresh2 = 0.10; % less than 10% of the median chan var to be flat
            cfg.removebadchann.zthresh  = 4; % 4 robust SD higher freq than other channels 
            cfg.removebadchann.highfrqbp  = [30 140]; % high freq range
            cfg.removebadchann.rmvchanplot = 'yes'; 
            cfg.removebadchann.log = 'yes';
            cfg.removebadchann.intpmatrixupdt = 'yes'; % Creates intpmatrix

            % Drops detected bad trials from channel variance calculation
            cfg.removebadchann.muscleprotection = 'yes';
            cfg.removebadchann.highfrqbp  = [30 140]; % high freq range
        
            % Attenuates peaks before channel variance calculation
            cfg.removebadchann.peakprotection = 'yes';
            cfg.removebadchann.blinkchans = {'Fp1', 'Fp2', 'Fz', 'F4', 'F3'}; 
            cfg.removebadchann.attenblnkplot   = 'yes';
        
            cfg.saveplots = cfg_saveplots; 
            cfg.saveplots.plotfolder = one_PNG_pathway;
            
            % Remove the bad channels
            [data_rmvchan, bad_chans] = ft_removebadchann(cfg, data_filt);
            stage = 'Part 3: Removing bad channels was successful';
            temp_dat = data_rmvchan;
        
            %%%%%%%% PART 4: REMOVE VERY CONTAMINATED TRIALS %%%%%%%%%%
            
            % Create a structure to delete bad segments
            cfg = [];
            cfg.rejectbadseg.seglength   = 2 ; % Segment length in seconds
            cfg.rejectbadseg.mthresh     = 4 ; % 4 x larger than median to be an artifact
            cfg.rejectbadseg.highfrqbp   = [30 140]; % High frq bands 
            cfg.rejectbadseg.zthresh     = 4;  % robust z score threshold for high frq pwr to detect bad trials
            
            % Specify saving trials if var is due to a few bad channels
            cfg.rejectbadseg.savetrials  = 'yes'; % Saves high var trials if due to small # of channels
            cfg.rejectbadseg.chanprop    = 0.10; % Number of channels to inspect how they contribute to trial var
            cfg.rejectbadseg.zchanpropvar = 5; % Threshold for how much chanprop can contribute to trial var

            % Indicate whether we want to see plots of deleted trials
            cfg.rejectbadseg.badsegplot     = 'yes'; % Create a plot of deleted segments
            cfg.rejectbadseg.indvbadsegplot = 'yes'; % Print out individual trials (rejected)
            cfg.rejectbadseg.onegoodplot    = 'yes'; % Produces a good plot for comparison
    
            cfg.rejectbadseg.log = 'yes';
            cfg.rejectbadseg.intpmatrixupdt = 'yes'; % Updates intpmatrix
        
            cfg.rejectbadseg.peakprotection = 'yes';
            cfg.rejectbadseg.blinkchans = {'Fp1', 'Fp2', 'Fz', 'F4', 'F3'}; 
            cfg.rejectbadseg.attenblnkplot   = 'no'; % We don't need to see them again
        
            cfg.saveplots = cfg_saveplots; 
            cfg.saveplots.plotfolder = one_PNG_pathway;
            
            % Remove bad trials
            data_rejseg = ft_rejectbadsegments(cfg, data_rmvchan);
            stage = 'Part 4: Removing bad trials was successful';
            temp_dat = data_rejseg;
    
            %%%%%%%% PART 5: INTERPOLATE CHANNELS IN NOISY SEGMENTS %%%%%%%%%%  
            
            % QC: These trials have electrode pops within them
            %cfg= [];
            %cfg.time = [77, 164];
            %ft_findtrials(cfg, data_rejseg)
            
            % Create a structure to interpolate noisy channels within segments
            cfg = [];
            cfg.chansegmentrepair.zthresh1 = 4; % Robust z-score threshold (general)
            cfg.chansegmentrepair.zthresh2 = 4; % Robust z-score threshold (band-pass)
            cfg.chansegmentrepair.regfrqbp = [30 140]; % Band-Pass filtering high Frequency 
            cfg.chansegmentrepair.type = 'all'; % Use all channel x trials for calculating robust z-score 
            cfg.chansegmentrepair.intmatrixplot = 'yes';
            cfg.chansegmentrepair.afterintplot = 'yes';
            cfg.chansegmentrepair.messages = 'off';
            cfg.chansegmentrepair.log = 'yes';
            cfg.chansegmentrepair.intpmatrixupdt = 'yes'; % Updates intpmatrix

            % Include information from neighbouring channels
            cfg.chansegmentrepair.neighbours = neighbours;

            % Specifiy we want to delete trials with too many bad channels
            cfg.chansegmentrepair.rmvtrials   = 'yes';
            cfg.chansegmentrepair.badchanprop =  0.20;
            cfg.chansegmentrepair.indvbadsegplot =  'yes';

            % Include peak protection just in case
            cfg.chansegmentrepair.peakprotection = 'yes';
            cfg.chansegmentrepair.blinkchans = {'Fp1', 'Fp2', 'Fz', 'F4', 'F3'}; 
            cfg.chansegmentrepair.attenblnkplot   = 'no'; % We don't need to see them again
    
            cfg.saveplots = cfg_saveplots; 
            cfg.saveplots.plotfolder = one_PNG_pathway;
            
            % Interpolate noisy segments 
            data_segfixed = ft_chansegmentrepair(cfg, data_rejseg);
            stage = 'Part 5: Interpolating channels in bad trials was successful. If failed after this it is likely due to many channels deleted in the recording';
            temp_dat = data_segfixed;
    
            %%%%%%%%%%%%%% PART 6: INTERPOLATE REMOVED CHANNELS %%%%%%%%%%%%%%
            
            % Interpolate bad channels using `ft_channelrepair()`
            cfg = [];
            cfg.chaninterp.badchannel     = bad_chans; % from `ft_removebadchann()`
            cfg.chaninterp.method         = 'weighted';
            cfg.chaninterp.neighbours     = neighbours;
            cfg.chaninterp.orig_labels    = data_noisy.label;
            cfg.chaninterp.elec_file      = which('standard_1005.elc');
            cfg.chaninterp.intpchanplot   = 'yes';
            cfg.chaninterp.log            = 'yes';  
    
            cfg.saveplots = cfg_saveplots; 
            cfg.saveplots.plotfolder = one_PNG_pathway;
    
            % Interpolate the channels
            data_intchanns = ft_channelinterpolate(cfg, data_segfixed);
            stage = 'Part 6: Interpolating channels globally was sucessful';
            temp_dat = data_intchanns;
    
            %%%%%%%%%%%%%%%%%%% PART 7: ROBUST RE-REFERENCING %%%%%%%%%%%%%%%%%%
            
            % Create a structure to do robust referencing
            cfg = [];
            cfg.robustreference.thresh =  4; % 4 SD noise from the mean for sample to be weighted 0
            cfg.robustreference.heatmap = 'yes'; % Creates z-score heat map to show amplitude noise 
            cfg.robustreference.padding = 100;  % Converts 100 samples left/right of 0 to 0 
            cfg.robustreference.channelplot = 'yes';
            cfg.robustreference.log = 'yes';
    
            cfg.saveplots = cfg_saveplots; 
            cfg.saveplots.plotfolder = one_PNG_pathway;
            
            % Run robust referencing
            [data_referenced, mn1] = ft_robustreference(cfg, data_intchanns);
            stage = 'Part 7: Robust rereferencing was sucessful';
            temp_dat = data_referenced;
    
            %%%%%%%%%%%%% SAVE PARTIALLY PROCESSED DATA REFERENCING %%%%%%%%%%%%%
            
            % Save the 'data' object as a .mat file
            full_name = fullfile(eegProcessed_pathway, one_partpreproc);     
            save(full_name, "-fromstruct", data_referenced);                       
            
      catch ME
            fprintf('Iteration %d crashed: %s\n', iter, ME.message);
            % Optional: rethrow if you want the whole parfor to stop
            %rethrow(ME);

            fid = fopen(error_csv, 'a');
            msg = strrep(ME.message, '"', '""');
            fprintf(fid, '%d,"%s","%s"\n', iter, stage, msg);
            fclose(fid);

            % Save the 'temp_dat' object as a .mat file
            full_name = fullfile(eegProcessed_pathway, one_partpreproc);     
            save(error_mat, "-fromstruct", temp_dat);
            
        end
    
    end

    fprintf('Finished batch %d/%d (files %d to %d)\n', batch, numBatches, startIdx, endIdx);
end

totalTime = toc;                            % STOP TIMER
fprintf('\n=== FINISHED ===\nEntire parfor (including pool startup) took %.2f minutes (%.1f hours)\n', ...
        totalTime/60, totalTime/3600);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% PART 2 %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load in file names (wet & dry) split by pending and already processed
cfg = [];
cfg.inputdir1      = eegProcessed_pathway;
cfg.inputdir2      = eegProcessed_pathway;
cfg.outputdir      = eegProcessed_pathway;
cfg.inputpattern1  = ['*', beforeICA_ext];
cfg.inputpattern2  = ['*', beforeICA_ext]; 
cfg.outputpattern  = ['*', procc_ext];
cfg.fullname       = 'yes'; % Returns the pathway (makes easier to load data)

% Return files that have and have not been processed
[pendingRawNames, processedRawNames] = ft_notyetprocessed(cfg);


% Start parallel preprocessing
delete(gcp('nocreate'));        % Kills any old pool (12 workers or whatever)
tic;                                        
myCluster = parcluster('local');
delete(myCluster.Jobs);         % Clears old crash files
myCluster.NumWorkers = 2;       % More than this will cause parfor to fail (ICA)
parpool(myCluster);             % Starts the cluster with specified number of workers
fprintf('Successfully started clean pool with %d workers\n', myCluster.NumWorkers);


% Prepare the batches (number of files processed at a time)
N = length(pendingRawNames);
batchSize = 30;
numBatches = ceil(N / batchSize);


% Set up a for loop first that sets up batches for parfor loop
for batch = 1:numBatches
    startIdx = (batch-1)*batchSize + 1;
    endIdx   = min(batch*batchSize, N);
    
    % Extract current batch of indices
    batchIndices = startIdx:endIdx;

   % Run parfor loop 
    parfor iter = batchIndices
        try
            % For each worker to produce the same results each time
            rng(iter);
    
            % Load in the .mat file containing partially processed EEG data
            one_raw_full = pendingRawNames{iter}; 
            data_referenced = load(one_raw_full);
    
            % Recreate the one_PNG_pathway
            [~, name, ext] = fileparts(one_raw_full);
            [~, beforeICA, ext2] = fileparts(beforeICA_ext);
            name = strrep(name, beforeICA, '');
            one_PNG_pathway = fullfile(PNG_pathway, name);
            error_mat = fullfile(eegProcessed_pathway, [name error_procc_ext]);
            error_csv = fullfile(ERRORS_pathway, [name error_procc_ext_csv]);

            % Create the name to save the processed data
            one_partpreproc = [name procc_ext];
    
            %%%%%%%%%%%%%% BEFORE STARTING THE PROCESSING PIPELINE %%%%%%%%%%%%%%
        
            % Save plotting configuration into new configurations (needed to parallele process)
            cfg_time2     = cfg_time;
            cfg_fft2      = cfg_fft;
            cfg_topo2     = cfg_topo;
            cfg_sum2      = cfg_sum;
            cfg_stack2    = cfg_stack;
            cfg_corr2     = cfg_corchans;
            cfg_chntrfft2 = cfg_chntrfft;     
       
            % Update each plot configuration to include new save PNG pathway (needed to parallel process)
            cfg_time2.saveplots.plotfolder = one_PNG_pathway;
            cfg_fft2.saveplots.plotfolder = one_PNG_pathway;
            cfg_topo2.saveplots.plotfolder = one_PNG_pathway;
            cfg_sum2.saveplots.plotfolder = one_PNG_pathway;
            cfg_stack2.saveplots.plotfolder = one_PNG_pathway;
            cfg_corr2.saveplots.plotfolder = one_PNG_pathway;
            cfg_chntrfft2.saveplots.plotfolder = one_PNG_pathway;
            
            % Find newest linenoise file
            d = dir(fullfile(one_PNG_pathway,'*linenoisermv*.png'));
            if ~isempty(d)
                cutoff = max([d.datenum]);
            
                % Delete any chantrials file created after that
                d2 = dir(fullfile(one_PNG_pathway,'*chantrials*.png'));
                for k = 1:numel(d2)
                    if d2(k).datenum > cutoff
                        delete(fullfile(one_PNG_pathway, d2(k).name));
                    end
                end
            end
            pause(0.02);
    
            % Delete the following PNGs
            delete(fullfile(one_PNG_pathway, '*comp*.png'));
            delete(fullfile(one_PNG_pathway, '*linenoise*.png'));
            delete(fullfile(one_PNG_pathway, '*main*.png'));

            % Same as above delete error file if it already exists
            if isfile(error_mat); delete(error_mat); end 
            if isfile(error_csv); delete(error_csv); end 
            
            % Add electrode positions into the data (10-05 template)
            elec_file = which('standard_1005.elc');
            data_referenced.elec = ft_read_sens(elec_file);
           
            % Define neighbors using the standard 10-05 template
            cfg = [];
            cfg.method = 'distance'; % or 'triangulation'
            cfg.layout = 'EEG1005.lay'; % Your layout file
            neighbours = ft_prepare_neighbours(cfg);
    
            %%%%%%%%% PART 8: ICA AND ARTIFACT COMPONENT REMOVAL %%%%%%%%%%%
            % Create a structure to run ICA
            cfg = [];
            cfg.performingica.method = 'runica';
            cfg.performingica.log = 'yes';
            
            % Run ICA and get components
            rng(157, 'twister'); [comp, data_referenced, x_rank] = ft_runicaica(cfg, data_referenced);
            stage = 'Part 8: ICA was sucessful';
            temp_dat = data_referenced;
            
            % Create a variable for minimum available components
            availComp = min(x_rank, 20);
    
            % Create a configuration to create topography maps of components
            cfg = []; 
            cfg.component  = 1:availComp;
            cfg.layout     = 'EEG1005.lay';
            cfg.zilim      = 'maxabs';
            cfg.warningmsg = 'off';
    
            cfg.saveplots = cfg_saveplots; 
            cfg.saveplots.plotfolder = one_PNG_pathway;
    
            % Generate the topography plot
            ft_plotcomptop(cfg, comp)

            % Create a configuration structure to identify blink components
            cfg = [];
            cfg.compblinks.compnum = availComp;
            cfg.compblinks.zthresh = 3.5; % Change to 3.5 from 3
            cfg.compblinks.min_dur = 0.1;
            cfg.compblinks.max_dur = 0.5;
            cfg.compblinks.min_blinks = 10;
            cfg.compblinks.eyechannels = {'Fp1','Fp2', 'Fz'};
            cfg.compblinks.thresh = .15;
            cfg.compblinks.blinkcompplot = 'yes'
    
            cfg.compblinks.data = data_referenced; % Insert data for logging purposes
    
            cfg.saveplots = cfg_saveplots; 
            cfg.saveplots.plotfolder = one_PNG_pathway;
    
            % Identify the blink components (an ICs with large amplitudes)
            [blinkcomp, data_referenced] = ft_findcompblinks(cfg, comp);
            
            % Reject artifact blink components
            cfg = [];
            cfg.rejcomp.blinkcomp = blinkcomp;
            cfg.rejcomp.preproc = data_referenced.cfg.preproc;
            cfg.rejcomp.log = 'yes';
            
            % Reject components from the data
            data_rmvcmp = ft_rejcomponents(cfg, comp);
            stage = 'Part 8.1: Rejecting Blink Components was sucessful';
            temp_dat = data_rmvcmp;
    
    
            %%%%%%%%%%%%% PART 9: REMOVE LINE NOISE %%%%%%%%%%%%%%%%
            % We wrote code that is fast and not as good as others but worth
            % it- better approaches will cause MATLAB to be extremly slow and
            % crash!
    
            % Create a strucuture to remove line noise
            cfg = [];
            cfg.rmvlinenoise.dftfilter = 'yes';
            cfg.rmvlinenoise.dftfreq = [60 120 180]; 
            cfg.rmvlinenoise.dftreplace = 'zero'; % Makes a wider band
            cfg.rmvlinenoise.dftbandwidth = 1; % Keep at 1; making it larger causes problems
            cfg.rmvlinenoise.log = 'yes';

            % Introduce optional more broad line noise removal
            cfg.rmvlinenoise.bsfilter   = 'yes';
            cfg.rmvlinenoise.bsfreq     = [59 61]; % or [59 61] 
            cfg.rmvlinenoise.bsfiltord  = 4; % Butterworth 4th order

            % Generate the plot
            cfg.rmvlinenoise.rmvlineplot = 'yes';
    
            cfg.saveplots = cfg_saveplots; 
            cfg.saveplots.plotfolder = one_PNG_pathway;
            
            % Remove line noise (USA)
            data_rmvlinenoise = ft_removelinenoise(cfg, data_rmvcmp);
            stage = 'Part 9: Removing Line Noise was sucessful';
            temp_dat = data_rmvlinenoise;
    
            %%%%%%%% PART 10: INTERPOLATE CHANNELS IN NOISY SEGMENTS (AGAIN) %%%%%%%%%%
  
            % Create a structure to interpolate noisy channels within segments
            cfg = [];
            cfg.chansegmentrepair.zthresh1 = 4; % Robust z-score threshold
            cfg.chansegmentrepair.zthresh2 = 4; % Robust z-score threshold
            cfg.chansegmentrepair.regfrqbp = [1 8]; % Band-Pass filtering low Frequency 
            cfg.chansegmentrepair.type = 'all'; % Use all channel x trials for calculating robust z-score 
            cfg.chansegmentrepair.intmatrixplot = 'yes';
            cfg.chansegmentrepair.afterintplot = 'yes';
            cfg.chansegmentrepair.messages = 'off';
            cfg.chansegmentrepair.log = 'yes';
            cfg.chansegmentrepair.intpmatrixupdt = 'yes'; % Updates intpmatrix

            % Include information from neighbouring channels
            cfg.chansegmentrepair.neighbours = neighbours;

            % Specifiy we want to delete trials with too many bad channels
            cfg.chansegmentrepair.rmvtrials   = 'yes';
            cfg.chansegmentrepair.badchanprop =  0.20;
            cfg.chansegmentrepair.indvbadsegplot =  'yes';

            % Do not allow peak protection
            cfg.chansegmentrepair.peakprotection = 'no';
            cfg.chansegmentrepair.blinkchans = []; 
            cfg.chansegmentrepair.attenblnkplot   = 'no'; % We don't need to see them again
    
            cfg.saveplots = cfg_saveplots; 
            cfg.saveplots.plotfolder = one_PNG_pathway;
            
            % Interpolate noisy segments 
            data_clean = ft_chansegmentrepair(cfg, data_rmvlinenoise);
            stage = 'Part 10: Interpolating bad channels within trials was sucessful; errors past this are related to plots- most likely because the EEG data is very bad';
            temp_dat = data_clean;
    
            %%%%%%%%%%%%%%%%%%%%%% RECORD EEG VARIANCE %%%%%%%%%%%%%%%%%%%%%%%
            
            % Create a configuration structure
            cfg = [];
            cfg.eegvarsum.log = 'yes';
            
            % Add variance information into the data
            data_clean = ft_eegvarsum(cfg, data_clean);
            temp_dat = data_clean;
            

            %%%%%%%%%%%%%%% ADD INTPMATRIX INFO INTO SUMMARY %%%%%%%%%%%%%%%%
           
            % Extract the channel labels as a string array
            chanLabels = string(data_clean.cfg.preproc.labels);  

            % Calculate the proportion of trials interpolated for each channel
            intpmatrix = data_clean.cfg.preproc.intpmatrix;
            channel_intp_prop = round(mean(intpmatrix, 2), 3);
            propValues = channel_intp_prop(:); 
            assert(length(chanLabels) == length(propValues), 'Number of channels and values must match!');
            
            % Add the new fields to the existing structure
            for i = 1:length(chanLabels)
                fieldName = append('int_', chanLabels(i));    
                data_clean.cfg.preproc.summary.(fieldName) = propValues(i);
            end

            % Add total interpolation measure
            data_clean.cfg.preproc.summary.int_total = round(mean(intpmatrix(:)), 3);
            data_clean.cfg.preproc.summary.endTrialnum = size(intpmatrix,2);
            temp_dat = data_clean;

        
            %%%%%%%%%%%%%%  SUMMARY STATISTICS OF OUR DATA %%%%%%%%%%%%%%
    
            % Remove the skip to save these as summary plots
            cfg_time2.saveplots.skip = 0;
            cfg_fft2.saveplots.skip = 0;
            cfg_topo2.saveplots.skip = 0;
            cfg_sum2.saveplots.skip = 0;
            cfg_stack2.saveplots.skip = 0;
            cfg_corr2.saveplots.skip = 0;
            cfg_chntrfft2.saveplots.skip = 0;
    
            % Assert that these are main plots
            cfg_time2.saveplots.main = 'yes';
            cfg_fft2.saveplots.main = 'yes';
            cfg_topo2.saveplots.main = 'yes';
            cfg_sum2.saveplots.main = 'yes';
            cfg_stack2.saveplots.main = 'yes';
            cfg_corr2.saveplots.main = 'yes';
            cfg_chntrfft2.saveplots.main = 'yes';
    
            % All our custom plots
            data_clean = ft_plotchannelavg(cfg_time2, data_clean);
            data_clean = ft_plotchannelfft(cfg_fft2, data_clean);
            
            % Create the topography comprehensive plot
            [topo_dat, data_clean] = ft_plottopoprep(data_clean);
            ft_plottopo(cfg_topo2, topo_dat);
            
            % Plot all the channels stacked together
            ft_plotstack(cfg_stack2, data_clean);
            
            % Plot the correlations
            ft_corchans(cfg_corr2, data_clean);
            
            % Plot the power spectra of the cleaned data
            ft_plotchantrialfft(cfg_chntrfft2, data_clean)


            %%%%%%%%%%%%%% SAVE THE PREPROCESSED EEG DATA %%%%%%%%%%%%%%%%%
            
            % Save the 'data' object as a .mat file
            full_name = fullfile(eegProcessed_pathway, one_partpreproc);     
            save(full_name, "-fromstruct", data_clean);   
                  
    
        catch ME
            fprintf('Iteration %d crashed: %s\n', iter, ME.message);
            % Optional: rethrow if you want the whole parfor to stop
            %rethrow(ME);

            fid = fopen(error_csv, 'a');
            msg = strrep(ME.message, '"', '""');
            fprintf(fid, '%d,"%s","%s"\n', iter, stage, msg);
            fclose(fid);

            % Save the 'temp_dat' object as a .mat file
            full_name = fullfile(eegProcessed_pathway, one_partpreproc);     
            save(error_mat, "-fromstruct", temp_dat);
    
        end
    
    end
    

    fprintf('Finished batch %d/%d (files %d to %d)\n', batch, numBatches, startIdx, endIdx);
end

totalTime = toc;                            % ← STOP TIMER
fprintf('\n=== FINISHED ===\nEntire parfor (including pool startup) took %.2f minutes (%.1f hours)\n', ...
        totalTime/60, totalTime/3600);



%%%%%%%%%%% LOAD ALL DATA SAVE VAR NAMES %%%%%%%%%%%%%%%

% .mat file directory
file_path = eegProcessed_pathway;

% Load in file names
all_files_dir = dir(fullfile(file_path, '*preproc.mat'));
all_files = {all_files_dir.name};

% Load in file names that failed (both beforeICA and preproc)
all_failed_files_dir = dir(fullfile(file_path, '*failed.mat'));
all_failed_files = {all_failed_files_dir.name};

% Combine all files into one vector
all_files = [all_files all_failed_files];

% Load in the .mat file variable names
all_columns = cell(numel(all_files),1);   % pre-allocate for parfor


parfor i = 1:numel(all_files)
    cols = {};
    try
        % Add full file pathway
        full_file = fullfile(file_path, all_files{i});

        % Load in the .mat file
        loaded = load(full_file);

        % If .summary field is present, extract var names
        if isfield(loaded.cfg.preproc, 'summary')
            cols = fieldnames(loaded.cfg.preproc.summary);
        end
    catch
        % skip bad files
    end
    % Save var names within a cell array
    all_columns{i} = cols;
end


% Create a cell array of all variable names present in the data
all_columns = unique(vertcat(all_columns{:}));



%%%%%%%%%%% PART 16: BUILD A SUMMARY TABLE %%%%%%%%%%%%%%%

nFiles = length(all_files);
summary_rows = cell(nFiles,1); % pre-allocate for parfor

parfor ii = 1:nFiles
    % keep subject strng from file name
    current_file = all_files{ii};
    subject = strrep(current_file, procc_ext, '');
   
    % Create a table will all var names- each cell is NaN
    row_data = repmat({NaN}, 1, length(all_columns));
    row_table = cell2table(row_data, 'VariableNames', all_columns);
    % Add subject information into the table
    row_table.subject = {subject};
   
    % Try to fill real values
    try
        % Add pathway to the files
        full_file = fullfile(file_path, current_file);
        % load in the .mat file (only cfg to save RAM)
        loaded = load(full_file, 'cfg');
        % Extract the .summary information
        summary = loaded.cfg.preproc.summary;
        % Extract the field names
        summary_fieldnames = fieldnames(summary)'; % cell array
        
        % Create a for loop and insert each cell in the summary table
        for k = 1:length(summary_fieldnames)          
            name = summary_fieldnames{k};               
            value = summary.(name);
           
            % Cell handeling
            if iscell(value)
                row_table.(name) = {strjoin(value, ',')};
            % Integer handeling
            elseif isnumeric(value) && numel(value) > 1
                row_table.(name) = {mat2str(value)};
            % Other
            else
                row_table.(name) = {value};
            end
        end
    catch
        % keep NaN if failed
    end
    
    summary_rows{ii} = row_table;
end

% Expand the row of the summary table each iteration → done after parfor
summary_table = vertcat(summary_rows{:});

% Print the summary table
summary_table

% Save table
T_filename = 'EEG_Preprocessing_Summary_Statistics.csv';
T_full_name = fullfile(CSV_pathway, T_filename);
writetable(summary_table, T_full_name)


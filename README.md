## Repository Experiment Programming and Data Pre-processing

Run opensesame on Anaconda: 
"conda activate opensesame-py3";
"opensesame"

**OpenSesame script with Python inline scripts inside**
  1. Show average accuracy to participants as feedback: LangLearning_Adult.osexp
  2. Oddball and ERP trigger
  
**R script for behaviroal data-washing and glme statistics**
(based on data files from OpenSesame): glme.r
  
**R script for plotting bar chart with jittered raw data and significance annotation**
  
  
**EEG**
  1. configuration file for biosemi: 1024 hz online recording, auto-unpause, 50 hz online notch filtering (not included yet). 
  2. locolization file of biosemi 32 channel (use it in EEGLAB)
  3. Matlab script: in a study set of 18 participants, first run a 50 hz notch filtering, and then a 0.01-30 hz passband filtering. 


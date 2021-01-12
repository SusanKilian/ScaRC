% McDermott
% 5-28-2009
% FDS_verification_script.m
%
% Modified version for ScaRC
%

close all
clear all

restoredefaultpath
addpath 'scripts'

% Scripts to run prior to dataplot for ScaRC

disp('ns2d_scarc...');                           ns2d_scarc

% Dataplot and scatplot options

Dataplot_Inputs_File = 'FDS_verification_dataplot_inputs_scarc.csv';
Working_Dir = '../../Verification/';
Manuals_Dir = '../../Manuals/';

% Run dataplot and scatplot scripts

[saved_data,drange] = dataplot(Dataplot_Inputs_File, Working_Dir, Working_Dir, Manuals_Dir); %, [99:123]);

% Special cases

%disp('turb_model_scarc...');                    turb_model_scarc
disp('ribbed_channel_scarc...');                ribbed_channel_scarc
disp('shunn_mms_error_scarc...');               shunn_mms_error_scarc

display('verification_scarc scripts completed successfully!')

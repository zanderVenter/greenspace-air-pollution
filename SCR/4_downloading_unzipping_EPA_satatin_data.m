% This script downloads pre-generated daily summary data files for air quality
% data collected at outdoor monitors across the US from the EPA Air Data website.

% Clear workspace
clc;
clear;

% Defining the codes for different air quality components on the EPA Air Data webpage
% Ozone: 44201; NO2: 42602; PM2.5: 88101; PM10: 81102
components = {'44201', '42602', '88101', '81102'};

% Initialize a cell array to keep track of any missed downloads
missedDownloads = cell(1,1);
missedCount = 1;

% Loop through each component
for ii = 1:length(components)
    % Loop through each year
    for year = 2010:2023
        pause(1); % Pause to prevent overwhelming the server
        % Construct the download URL
        url = strcat('https://aqs.epa.gov/aqsweb/airdata/daily_', string(components{ii}), '_', string(year), '.zip');
        % Define the download location/folder
        downloadLocation = strcat('.../EPA_', string(components{ii}), '_daily_observations');
        
        % Attempt to download and unzip the file
        try
            unzip(url, downloadLocation);
        catch
            % If download fails, record the missed link
            missedDownloads{missedCount} = url;
            missedCount = missedCount + 1;
            pause(0.5); % Pause before retrying
        end
    end
end

% Display missed links, if any
if missedCount > 1
    disp('Missed Links:');
    disp(missedDownloads(~cellfun('isempty', missedDownloads)));
end

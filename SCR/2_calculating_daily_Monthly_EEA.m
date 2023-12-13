% Script to extract and process EEA air quality station data for various
% air pollutant components: 'pm10','pm2.5','o3','no2','no2'
% Data is summarized on a daily and monthly basis for the years 2010-2023

% Clear workspace
clc;
clear;

% Defining required components
Component = {'pm10','pm2.5','o3','no2','no2'};

% Loop through each component
for ii = 1:length(Component)

    % Loop through each year
    for Year = 2022:2023
        % Define the folder path for the year (folder where the data from
        % Downloading_unizipping_EEA_satatin_data.m are saved)
        dataFolder = strcat('...\observations/', string(Year), '_Unzipped');
        files = dir(fullfile(dataFolder, '*.csv'));

        % Initialize tables for daily and monthly data
        dailyData = cell(1,1);
        monthlyData = cell(1,1);

        % Process each file
        for i = 1:length(files)
            filename = fullfile(dataFolder, files(i).name);
            dataTable = readtable(filename, 'FileType', 'text', 'Delimiter', ',');

            % Skip empty tables
            if isempty(dataTable)
                continue
            end

            % Filter for component data and summary type
            dataTable = dataTable(dataTable.variable == string(Component{ii}) & dataTable.summary == 1, :);
            dataTable = standardizeMissing(dataTable, 'NA', 'DataVariables', "value");
            dataTable = rmmissing(dataTable);

            % Skip if table is empty after filtering
            if height(dataTable) == 0
                continue
            end

            % Convert date strings to datetime objects
            dataTable.date = strrep(string(dataTable.date), 'T', ' ');
            dataTable.DATE = datetime(dataTable.date, 'InputFormat', 'yyyy-MM-dd HH:mm:ssZ', 'TimeZone', 'UTC');

            % Extract site information and prepare the table
            siteName = string(dataTable.site(1));
            dataTable = table2timetable(dataTable(:, {'DATE', 'value'}));

            % Convert cell values to numbers if necessary
            if isa(dataTable.value, 'cell')
                dataTable.value = cellfun(@str2num, dataTable.value);
            end

            % Aggregate data on a daily basis
            dailyTable = retime(dataTable, 'daily', 'mean');
            dailyTable.value = round(dailyTable.value, 3);
            dailyTable.site = repmat(siteName, height(dailyTable), 1);
            dailyData{i} = dailyTable;

            % Aggregate data on a monthly basis
            monthlyTable = retime(dataTable, 'monthly', 'mean');
            monthlyTable.value = round(monthlyTable.value, 3);
            monthlyTable.site = repmat(siteName, height(monthlyTable), 1);
            monthlyData{i} = monthlyTable;
        end

        % Aggregate and write the aggregated daily data to a file
        dailyData = vertcat(dailyData{:});
        dailyData = timetable2table(dailyData);
        dailyFileName = strcat('...\EEA_',string(Component{ii}),'_daily_stats_', string(Year), '_microg_m3.txt');
        writetable(dailyData, dailyFileName, 'Delimiter', ',', 'WriteVariableNames', true);

        % Aggregate and write the aggregated monthly data to a file
        monthlyData = vertcat(monthlyData{:});
        monthlyData = timetable2table(monthlyData);
        monthlyFileName = strcat('...\EEA_',string(Component{ii}),'_montly_stats_', string(Year), '_microg_m3.txt');
        writetable(monthlyData, monthlyFileName, 'Delimiter', ',', 'WriteVariableNames', true);
    end
end

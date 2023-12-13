% This script downloads air quality station data from the EEA website for the years 2010 to 2023.

% The  site description of EEA station data is available at:
% "http://aq-data.ricardo-aea.com/R_data/saqgetr/helper_tables/sites_table.csv.gz"


% Clear workspace
clc;
clear;

% Create a cell array to store missed links
Missed = cell(1, 1);
count = 1;

% Loop through each year
for year = 2010:2023
    % URL of the webpage
    url = sprintf('http://aq-data.ricardo-aea.com/R_data/saqgetr/observations/%d/', year);

    % Read the content of the webpage
    try
        content = webread(url);
    catch
        disp(['Unable to read content from URL for the year ', num2str(year)]);
        continue; % Skip to the next iteration
    end

    % Regular expression to find all links
    % Adjust the pattern if necessary to match the specific format of the links
    pattern = '<a href="([^"]*.csv.gz)">';

    % Extract links
    links = regexp(content, pattern, 'tokens');

    % Check if any links were found
    if isempty(links)
        disp(['No links found on the page for the year ', num2str(year)]);
    else
        % Loop through each link and download the file
        for i = 1:length(links)
            link = links{i}{1};
            fullLink = fullfile(url, link);
            try
                % Specify the download location
                downloadLocation = fullfile('...\observations', ...
                                            [num2str(year) '_Unzipped']);

                % Download the file and unzip it
                gunzip(fullLink, downloadLocation);

            catch
                % If download fails, record the missed link
                Missed{count} = fullLink;
                count = count + 1;
                pause(.5);
            end
        end
    end
end

% Display missed links, if any
if count > 1
    disp('Missed Links:');
    disp(Missed(~cellfun('isempty', Missed)));
end



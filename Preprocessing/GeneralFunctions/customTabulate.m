function T = customTabulate(data)
    % Check if the input is numeric or categorical
    if isnumeric(data) || islogical(data)
        uniqueValues = unique(data);
    elseif iscategorical(data) || ischar(data) || isstring(data)
        uniqueValues = unique(data, 'stable'); % Keep original order for categorical/text
    else
        error('Unsupported data type. Use numeric, categorical, string, or logical arrays.');
    end

    % Preallocate results
    numUnique = numel(uniqueValues);
    frequencies = zeros(numUnique, 1);

    % Calculate frequencies
    for i = 1:numUnique
        frequencies(i) = sum(data == uniqueValues(i));
    end

    % Calculate percentages
    percentages = (frequencies / numel(data)) * 100;

    % Combine into output table
    T = [uniqueValues(:), frequencies, percentages];
end

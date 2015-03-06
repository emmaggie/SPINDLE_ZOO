if exist('OCTAVE_VERSION') ~= 0  
    pkg load parallel
end

inputs = 1:100;  
numCores = nproc()

% assumes that processInput is defined in a separate function file
[result] = pararrayfun (numCores, @processInput, inputs)
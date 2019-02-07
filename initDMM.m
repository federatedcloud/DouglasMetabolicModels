
% Genearal COBRA SETUP:

initialDir = pwd;
gurobi_home = getenv('GUROBI_HOME');
cd(gurobi_home);
cd('matlab');
gurobi_setup;

% TODO: initCobraToolbox

cd(initialDir);

% Repo Specific Bits:

addpath('Utils');

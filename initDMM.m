
% Genearal COBRA SETUP:

global DMMDIR;
DMMDIR = pwd;
gurobi_home = getenv('GUROBI_HOME');
cd(gurobi_home);
cd('matlab');
gurobi_setup;

% TODO: initCobraToolbox

cd(DMMDIR);

% Repo Specific Bits:

addpath('Utils');

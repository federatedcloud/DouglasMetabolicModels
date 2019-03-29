% TODO: 
outDirectory = strjoin({currentGitSha}, '_');
system(strjoin({'mkdir -p', outDirectory}));

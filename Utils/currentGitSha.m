function gitsha = currentGitSha
  [status, out] = system('git rev-parse HEAD');
  assert(status == 0);
  gitsha = strtrim(out);
end

{ fetchgit }:
rec {
  version = "26.0.0-20251223";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/gnatdoc.git";
    ref = "refs/tags/master";
    rev = "60140b00d2d1f63ae80328b29db8944be90b7773";
  };
}

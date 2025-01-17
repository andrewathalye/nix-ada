{ fetchgit }:
rec {
  version = "25.0.0";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/gnatdoc.git";
    ref = "refs/tags/v25.0.0";
    rev = "510bd4247b8374deba3c68959359140e97ddee7f";
  };
}

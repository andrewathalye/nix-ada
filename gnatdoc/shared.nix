{ fetchgit }:
rec {
  version = "24.2-20240528-git";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/gnatdoc.git";
    ref = "master";
    rev = "1c7ba55cdc69cf169be1d12ca3c81751668c0117";
  };
}

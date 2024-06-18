{ fetchgit }:
rec {
  version = "24.2";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/gnatdoc.git";
    ref = version;
    rev = "52e560cb16e9f832eae4071dd3cff0127e43682b";
  };
}

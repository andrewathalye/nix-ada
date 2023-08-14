{ lib
, fetchPypi
, python3
}:

python3.pkgs.buildPythonPackage rec {
  pname = "types-gdb";
  version = "12.1.4.4";

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-BmXMe+RpNZ7pXLZEJUG7NQlDN6fPtQIPeBcZf7AJoC4=";
  };
} 

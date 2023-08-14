{ lib
, buildPythonPackage
, fetchzip
}:

buildPythonPackage rec {
  pname = "gnat-gdb-scripts";
  version = "20221017-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/gnat-gdb-scripts/archive/58d926ac625a3cd2c3597d66a4da20a8745cc039.zip";
    sha256 = "G49givvBAXvk2hepMxaCO2dpx/e6CC5ln7bfERKFOZI=";
  };

  doCheck = false;
}

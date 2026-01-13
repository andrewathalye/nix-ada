{ lib
, fetchzip

# Python deps
, python3Packages
, e3-core
}:

with python3Packages;
buildPythonPackage rec {
  pname = "e3-testsuite";
  version = "25.0-20230717-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/e3-testsuite/archive/131fd0ba9896d844ba109186e751f8c2f53d9843.zip";
    sha256 = "PPVJHJ1K/vxWI+Db9GgRqg1lteAVQle01KHEI1utTaU=";
  };

  pyproject = true;
  build-system = [ setuptools ];
  doCheck = false;
  propagatedBuildInputs = [ e3-core ];
}

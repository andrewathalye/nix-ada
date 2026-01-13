{ lib
, python3Packages
, libadalang
}:

with python3Packages;
buildPythonPackage {
  pname = "libadalang-python";
  version = libadalang.version;

  src = libadalang.out + "/python";
  pyproject = true;
  build-system = [ setuptools ];

  nativeBuildInputs = [ libadalang ];
  propagatedBuildInputs = [
    libadalang
  ];

  doCheck = false;
}

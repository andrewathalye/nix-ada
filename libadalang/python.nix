{ lib
, buildPythonPackage
, libadalang
}:

buildPythonPackage {
  pname = "libadalang-python";
  version = libadalang.version;

  src = libadalang.out + "/python";

  nativeBuildInputs = [ libadalang ];
  propagatedBuildInputs = [
    libadalang
  ];

  doCheck = false;
}

{ lib
, buildPythonPackage
, fetchzip
, adasat

# Python deps
, mako
, pygments
, autopep8
, coverage
, docutils
, flake8
, funcy
, mccabe
, mypy
, pexpect
, ptyprocess
, pycodestyle
, pyflakes
, pytest
, railroad-diagrams
, sphinx-rtd-theme
, types-docutils
, yapf
, pyyaml

# Local python deps
, types-gdb
, e3-core
, e3-testsuite
, gnat-gdb-scripts
}:

buildPythonPackage rec {
  pname = "langkit";
  version = "23.0.0-20230802-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/langkit/archive/32b9cd735c7236abfae8c474af88f89ea6664de0.zip";
    sha256 = "OjLB10nC4vnapXGyxuVM11QlO3PX5bT1BYrgrUHWoJA=";
  };

  pythonDeps = [ mako pygments autopep8 coverage docutils flake8 funcy mccabe mypy pexpect ptyprocess pycodestyle pyflakes pytest railroad-diagrams sphinx-rtd-theme types-docutils yapf pyyaml ];

  ourDeps = [ e3-core e3-testsuite gnat-gdb-scripts ];

  propagatedBuildInputs = [
    adasat
    pythonDeps
    ourDeps
  ];

  doCheck = false;
}

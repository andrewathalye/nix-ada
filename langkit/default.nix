{ lib
, buildPythonPackage
, fetchgit
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
, prettier-ada

# Local python deps
, types-gdb
, e3-core
, e3-testsuite
, gnat-gdb-scripts
}:

buildPythonPackage rec {
  pname = "langkit";
  version = "24.2-20240618";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/langkit.git";
    ref = "master";
    rev = "833549c4c664b99bd348e1e3e4ba1b29e114d6a1";
  };

  pythonDeps = [ mako pygments autopep8 coverage docutils flake8 funcy mccabe mypy pexpect ptyprocess pycodestyle pyflakes pytest railroad-diagrams sphinx-rtd-theme types-docutils yapf pyyaml ];

  ourDeps = [ e3-core e3-testsuite gnat-gdb-scripts ];

  propagatedBuildInputs = [
    adasat
    prettier-ada
    pythonDeps
    ourDeps
  ];

  doCheck = false;
}

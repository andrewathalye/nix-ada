{ lib
, fetchgit

# Python deps
, python3Packages

# Local python deps
, types-gdb
, e3-core
, e3-testsuite
, gnat-gdb-scripts
, prettier-ada
}:

with python3Packages;
buildPythonPackage rec {
  pname = "langkit";
  version = "26.0.0-20260109";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/langkit.git";
    ref = "master";
    rev = "bac25208b802112eb13f09967ddf4ee0e432aab6";
  };

  pyproject = true;
  build-system = [ setuptools ];

  pythonDeps = [ mako pygments autopep8 coverage docutils flake8 funcy importlib-metadata mccabe mypy pexpect ptyprocess pycodestyle pyflakes pytest railroad-diagrams sphinx-rtd-theme types-docutils yapf ];

  ourDeps = [ e3-core e3-testsuite gnat-gdb-scripts ];

  propagatedBuildInputs = [
    prettier-ada
    pythonDeps
    ourDeps
  ];

  doCheck = false;
}

{ lib
, buildPythonPackage
, fetchgit

# Python deps
, mako
, pygments
, autopep8
, coverage
, docutils
, flake8
, funcy
, importlib-metadata
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

# Local python deps
, types-gdb
, e3-core
, e3-testsuite
, gnat-gdb-scripts
, prettier-ada
}:

buildPythonPackage rec {
  pname = "langkit";
  version = "25.0.0-20250115";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/langkit.git";
    ref = "master";
    rev = "220bd77c0145db54d86ddfefcd66bffabbf1a925";
  };

  patches = [ ./diagnostics.py.patch ];

  pythonDeps = [ mako pygments autopep8 coverage docutils flake8 funcy importlib-metadata mccabe mypy pexpect ptyprocess pycodestyle pyflakes pytest railroad-diagrams sphinx-rtd-theme types-docutils yapf ];

  ourDeps = [ e3-core e3-testsuite gnat-gdb-scripts ];

  propagatedBuildInputs = [
    prettier-ada
    pythonDeps
    ourDeps
  ];

  doCheck = false;
}

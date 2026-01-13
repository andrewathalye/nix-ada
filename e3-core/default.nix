{ lib
, fetchzip

# python deps
, python3Packages
}:

with python3Packages;
buildPythonPackage rec {
  pname = "e3-core";
  version = "22.3.1";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/e3-core/archive/refs/tags/v22.3.1.zip";
    sha256 = "4StHOJldfeqApdF6D14Euzg9HvZ2e7G4/OQ0UrEbEIw=";
  };

  pyproject = true;
  build-system = [ setuptools ];
  patches = [ ./remove-ld.patch ];

  propagatedBuildInputs = [ colorama pyyaml python-dateutil requests requests-toolbelt tqdm stevedore setuptools psutil pytest coverage mock httpretty ];
}

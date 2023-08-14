{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, gtk3
, pkg-config
}:

stdenv.mkDerivation {
  pname = "gtkada";
  version = "23.0.0-20230726-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/gtkada/archive/b17054b92338d22db46ea0e2c63037b3e50190da.zip";
    sha256 = "I82n9+je2p/holsgnSrC80z3cyMtXbC2HW7msbnt1XM=";
  };

  nativeBuildInputs = [
    gprbuild
    gnat
    gtk3
    pkg-config
  ];

  propagatedBuildInputs = [
    gtk3
  ];

  patches = [ ./gpr-path.patch ];

}

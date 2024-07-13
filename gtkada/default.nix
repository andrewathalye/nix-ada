{ stdenv
, fetchgit
, gnat
, gprbuild
, glibc
, gtk3
, pkg-config
}:

stdenv.mkDerivation rec {
  pname = "gtkada";
  version = "24.2";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/gtkada.git";
    ref = version;
    rev = "96b4eceb0c088b07a91387da704547818d9192f0";
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

  # Fix a bug causing symbols to be recursively exported by all consumers
  # Skip tests
  buildPhase = ''
    sed -i 's/-Wl,--export-dynamic//g' gtkada_shared.gpr
    make build_library_type/static-pic relocatable tools
  '';

  # Also skips tests
  installPhase = ''
    make install/static-pic install/relocatable
    gprinstall -p -XLIBRARY_TYPE=relocatable --mode=usage --prefix=$out -Psrc/tools/tools.gpr
  '';

}

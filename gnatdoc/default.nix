{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, ada-markdown
, libadalang
, vss
}:

stdenv.mkDerivation {
  pname = "libgnatdoc";
  version = "23.0.0-20230809";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/gnatdoc/archive/8f93d1ab7d7e973d8d8c904805c1bec7aa061cbb.zip";
    sha256 = "2U8b0akqjSRsRQon00mBREZU7UOmy3FrNWVd+IJ4mUQ=";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  buildInputs = [
    ada-markdown
    libadalang
    vss
  ];

  dontConfigure = true;
  
  buildPhase = ''
    runHook preBuild
    
    gprbuild -j0 -Pgnat/libgnatdoc.gpr -XLIBRARY_TYPE=relocatable
    
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    gprinstall --prefix=$out -p -Pgnat/libgnatdoc.gpr -XLIBRARY_TYPE=relocatable

    runHook postInstall
  '';
}

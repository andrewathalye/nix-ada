{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
}:

stdenv.mkDerivation {
  pname = "alire";
  version = "1.2.2";
  
  src = fetchzip {
    url = "https://github.com/alire-project/alire/releases/download/v1.2.2/alr-1.2.2-full-sources.zip";
    sha256 = "rwNiSXOIIQR1I8wwp1ROVOfEChT6SCa5c6XnTRqekDc=";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  dontConfigure = true;
  
  buildPhase = ''
    runHook preBuild
    
    ALIRE_OS=linux gprbuild -j0 -p -P alr_env
    
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    ALIRE_OS=linux gprinstall -p --prefix=$out alr_env.gpr \
      --no-project \
      --no-manifest \
      --mode=usage

    runHook postInstall
  '';
}

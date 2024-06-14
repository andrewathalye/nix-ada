{ stdenv
, fetchzip
, gnat
, gprbuild
, glibc
, gnatcoll-core
, fswatch
}:

stdenv.mkDerivation {
  pname = "ada-libfswatch";
  version = "24.1-20220203";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/ada_libfswatch/archive/00fb794cd61f9f86e00151e8380886d361dba102.zip";
    sha256 = "s2Lzmf7/Iz8nzhTfZUhx/TH8D667NSCSnC2ePdIPVn0=";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  buildInputs = [
    gnatcoll-core
    fswatch
  ];

  patches = [ ./c_libfswatch.patch ];
  
  configurePhase = ''
    runHook preConfigure

    # Replace library path in GPR
    sed -i 's;@LIBFSWATCH_OUT_REPLACE@;'' + fswatch.out + '';' c_lib/c_libfswatch.gpr

    # Generate Ada spec
    mkdir generated
    cd generated
    gcc -c -fdump-ada-spec '' + fswatch.out + ''/include/libfswatch/c/libfswatch.h -D_TIMEZONE_DEFINED

    # Rename Ada spec
    sed -i 's/libfswatch_c_//g' *.ads
    for i in *.ads
    do
      mv $i $(echo $i | sed 's/libfswatch_c_//') 2>/dev/null || true
    done
    cd ..
    
    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild
    
    gprbuild -Pada_libfswatch -j0
    
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    gprinstall -p --prefix=$out -Pada_libfswatch    
    cp c_lib/c_libfswatch.gpr $out/share/gpr

    runHook postInstall
  '';
}

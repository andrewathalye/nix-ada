{ stdenv
, fetchzip
, gnat
, gprbuild
, gnatPackages
, fswatch
}:

with gnatPackages;
stdenv.mkDerivation {
  pname = "ada-libfswatch";
  version = "26.1-20240709";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/ada_libfswatch.git";
    ref = "26.1";
    rev = "838480d8fca344d9f8a78341113ceb4ed5cf2222";
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

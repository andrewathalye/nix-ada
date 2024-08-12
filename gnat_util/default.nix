{ stdenv
, gnat
, gprbuild
, fetchgit
, gmp
, mpfr
, libmpc
, isl
, which
, flex
, file
}:
stdenv.mkDerivation rec {
  pname = "gnat_util";
  version = "2021-gpl";

  nativeBuildInputs = [
    gnat
    gprbuild
    which
    flex
    file
  ];

  buildInputs = [
    gmp
    mpfr
    libmpc
    isl
  ];

  patches = [ ./fixinc.patch ];
  gnat_util_gpr = ./gnat_util.gpr;

  src = fetchgit {
    url = "https://github.com/andrewathalye/gnat-community-2021.git";
    hash = "sha256-wDQPkjTKk5HZEPs++PEBpzggoxLkeQaQefdN4wouZm4=";
  };

  hardeningDisable = [ "format" "pie" "fortify3" ];

  configurePhase = ''
    runHook preConfigure
    export CC=$(dirname $(which gnat))/gcc

    # Generate code for gnat_util
    mkdir build && cd build
    ../configure --enable-languages="c,ada" --disable-libada --disable-multilib --disable-bootstrap --disable-fixincludes

    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild
    export CC=$(dirname $(which gnat))/gcc

    # Build part of GCC
    make -j12 all-gcc

    mkdir ../gnat_util && cd ../gnat_util
    mkdir src

    # Copy base source tree
    for i in $(cat ../gcc/ada/MANIFEST.gnat_util)
    do
      if [[ -f ../gcc/ada/$i ]]
      then
        cp ../gcc/ada/$i src/
      fi
    done

    # Copy in generated code
    cp ../build/gcc/ada/sinfo-nodes.ads src
    cp ../build/gcc/ada/sinfo-nodes.adb src
    cp ../build/gcc/ada/einfo-entities.ads src
    cp ../build/gcc/ada/einfo-entities.adb src
    cp ../build/gcc/ada/seinfo.ads src
    cp ../build/gcc/ada/snames.ads src
    cp ../build/gcc/ada/snames.adb src

    # Rename
    mv src/sdefault_adb.gnat_util src/sdefault.adb

    # Create project and install
    cp $gnat_util_gpr gnat_util.gpr
    gprbuild gnat_util.gpr
    gprinstall -p -Pgnat_util --prefix=$out 

    runHook postBuild
  '';
}

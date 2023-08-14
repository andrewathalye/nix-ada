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
  version = "23.0.0-20230519-git";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/gnatdoc/archive/2166610d245c045cc0441322450a4c7b568f7725.zip";
    sha256 = "1HvbtYwJ6z3UoDq5qPTQ6G2FwNORP40ZYf9rl13H73E=";
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

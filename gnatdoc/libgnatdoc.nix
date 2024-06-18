{ stdenv
, fetchgit
, gnat
, gprbuild
, glibc
, ada-markdown
, libadalang
, vss
}:

let
   shared = import ./shared.nix { inherit fetchgit; };
in
stdenv.mkDerivation rec {
  pname = "libgnatdoc";
  version = shared.version;
  
  src = shared.src;
  
  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  propagatedBuildInputs = [
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

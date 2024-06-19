{ stdenv
, fetchgit
, gnat
, gprbuild
, glibc
, ada-markdown
, vss
, libgnatdoc
}:

let
   shared = import ./shared.nix { inherit fetchgit; };
in
stdenv.mkDerivation rec {
  pname = "gnatdoc";
  version = shared.version;
  
  src = shared.src;
  
  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  buildInputs = [
    ada-markdown
    libgnatdoc
    vss
  ];

  # Force the use of the systemwide libgnatdoc
  patchPhase = ''
    rm gnat/libgnatdoc.gpr
  '';

  dontConfigure = true;
  
  buildPhase = ''
    runHook preBuild
    
    gprbuild -j0 -Pgnat/gnatdoc.gpr -XLIBRARY_TYPE=relocatable
    
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share
    gprinstall --prefix=$out -p -Pgnat/gnatdoc.gpr -XLIBRARY_TYPE=relocatable
    cp -r share/gnatdoc $out/share/

    runHook postInstall
  '';
}

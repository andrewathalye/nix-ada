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
  version = "24.2";
  
  src = fetchzip {
    url = "https://github.com/AdaCore/gnatdoc/archive/refs/heads/{version}.zip";
    sha256 = "sha256-4nXTpTkyhRdUhm6mnDJAuNJOpcUJH066eaC7fWBmiSg=";
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

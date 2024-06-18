{ stdenv
, fetchgit
, gnat
, gprbuild
, glibc
, ada-markdown
, libadalang
, vss
}:

stdenv.mkDerivation rec {
  pname = "libgnatdoc";
  version = "24.2";
  
  src = fetchGit {
    url = "https://github.com/AdaCore/gnatdoc.git";
    ref = version;
    rev = "52e560cb16e9f832eae4071dd3cff0127e43682b";
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

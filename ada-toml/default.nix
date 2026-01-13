{ stdenv
, fetchgit
, gnat
, gprbuild
}:

let
   shared = import ./shared.nix { inherit fetchgit; };
in
stdenv.mkDerivation rec {
  pname = "ada-toml";
  version = "v0.5";
  
  src = fetchGit {
    url = "https://github.com/pmderodat/ada-toml.git";
    ref = "v0.5";
    rev = "e760110ad2b5b776a44dace31b8421532e429fbb";
  };
  
  nativeBuildInputs = [
    gprbuild
    gnat
  ];

  buildInputs = [
  ];

  dontConfigure = true;
  
  buildPhase = ''
    runHook preBuild
    
    gprbuild -Pada_toml -XLIBRARY_TYPE=relocatable -j0
    
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    gprinstall --prefix=$out -p -Pada_toml -XLIBRARY_TYPE=relocatable

    runHook postInstall
  '';
}

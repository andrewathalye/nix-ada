{ stdenv }:

stdenv.mkDerivation rec {
   pname = "alire-index";
   version = "1.3.0";

   src = builtins.fetchGit {
      url = "https://github.com/alire-project/alire-index.git";
      ref = "stable-" + version;
      rev = "be8abdacf723a08ca0bc87f82ce074daa34d3cc7";
   };

   settings_toml = ./settings.toml;
   index_toml = ./index.toml;

   skipConfigure = true;
   skipBuild = true;
   installPhase = ''
      mkdir -p $out/.config/alire/
      ln -s $settings_toml $out/.config/alire/settings.toml

      mkdir -p $out/.config/alire/indexes/community/
      ln -s $index_toml $out/.config/alire/indexes/community/index.toml
      ln -s $src $out/.config/alire/indexes/community/repo
   '';
}

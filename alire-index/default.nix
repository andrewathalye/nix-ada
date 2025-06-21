{ stdenv }:

stdenv.mkDerivation rec {
   pname = "alire-index";
   version = "1.4.0";

   src = builtins.fetchGit {
      url = "https://github.com/alire-project/alire-index.git";
      ref = "stable-" + version;
      rev = "c8f57b57d7d2833c1e499a35f060faf7f44e3530";
   };

   settings_toml = ./settings.toml;
   providers_toml = ./providers.toml;
   index_toml = ./index.toml;

   skipConfigure = true;
   skipBuild = true;
   installPhase = ''
      mkdir -p $out/.config/alire/
      ln -s $settings_toml $out/.config/alire/settings.toml

      mkdir -p $out/.config/alire/indexes/community/
      ln -s $providers_toml $out/.config/alire/indexes/
      ln -s $index_toml $out/.config/alire/indexes/community/index.toml
      ln -s $src $out/.config/alire/indexes/community/repo
   '';
}

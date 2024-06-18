{fetchgit}:
rec {
   version = "1.0.0";

   src = fetchGit {
      url = "https://github.com/onox/wayland-ada.git";
      ref = "master";
      rev = "f8a9fe05aa29e5e17d23951bc0d21ce4b84148a4";
   };
}

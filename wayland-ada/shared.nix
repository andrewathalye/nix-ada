{fetchzip}:
rec {
   src = fetchzip {
      url = "https://github.com/onox/wayland-ada/archive/refs/tags/v1.0.0.zip";
      sha256 = "QY7OQQLyi3f+dtM4thnWa5i15QtFnaqyK/7S6S127Bg=";
   };

   version = "1.0.0";
}

{}:
[
 # Use GNAT 14
 (final: prev: { gnatPackages = prev.gnat14Packages; })
 (final: prev: { gnat = prev.gnat14; })

 # Use bleeding-edge GNATCOLL-Core for File Index Hash Support
 (final: prev: { gnatcoll-core = ((prev.gnatcoll-core.overrideAttrs (final:
    {
      version = "25.0.0-20250113";
      src = fetchGit {
         url = "https://github.com/AdaCore/gnatcoll-core.git";
         ref = "master";
         rev = "713242c3f4c9a5ea97c94ec4c4a259cdadfd8785";
      };
 
      patches = [];
    }))); })

 # We need a version of gnatcoll-python that supports newer Python versions.
 (final: prev: { gnatcoll-python3 = ((prev.gnatcoll-python3.overrideAttrs (final:
    {
      version = "25.1";
      src = prev.fetchzip {
         url = "https://github.com/AdaCore/gnatcoll-bindings/archive/refs/heads/25.1.zip";
         hash = "";
      };
    })).override { python3 = prev.python3; }); }) # Override hardcoded Python version
]

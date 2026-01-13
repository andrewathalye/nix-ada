{}:
[
 # Use GNAT 15
 # Update packages
 (final: prev: {
  gnatPackages = prev.gnat15Packages.overrideScope (gfinal: gprev:
  {
    # Use bleeding-edge GNATCOLL-Core for File Index Hash Support
    gnatcoll-core = (gprev.gnatcoll-core.overrideAttrs (
    {
      version = "26.0.0-20260112";
      src = fetchGit {
         url = "https://github.com/AdaCore/gnatcoll-core.git";
         ref = "master";
         rev = "32967c2123f0cb7d36dfc5372ab6b4891e0fadcf";
      };
 
      patches = [];
    }));

    # We need a version of gnatcoll-python that supports newer Python versions.
    # Override hardcoded Python version
    gnatcoll-python3 = ((gprev.gnatcoll-python3.overrideAttrs (
    {
      version = "25.1";
      src = prev.fetchzip {
         url = "https://github.com/AdaCore/gnatcoll-bindings/archive/refs/heads/25.1.zip";
         hash = "sha256-s8VinVPm0syS8kdU1rswU+ePUBdTZvg72CBxtPc/zCs=";
      };
    })).override { python3 = prev.python3; });

    # gpr2 new version
    gpr2 = ((gprev.gpr2.overrideAttrs (
    {
      version = "26.0.0-20260112";
      src = fetchGit {
        url = "https://github.com/AdaCore/gpr";
        ref = "master";
        rev = "d34ee9879d7bd086a5989aa9cd78a8d9aa31c829";
      };

      # Bypass project unit error for users
      patches = [ ./gpr2/gpr2_no_standalone.patch ];
    })).override {
      gpr2kbdir = prev.fetchzip {
        url = "https://github.com/AdaCore/gprconfig_kb/archive/refs/tags/v25.0.0.zip";
        hash = "sha256-Oax3Aq+XHiMd823jtVUy43j4Sk7jVfD4cueDCLC0oSc=";
      } + "/db";
    });
  });})

  # Use GNAT 15
  (final: prev: { gnat = prev.gnatPackages.gnat; })
]

# We require GNAT 13 to match the current system compiler GCC 13
{}:

[
 (final: prev: { gnat12 = prev.gnat13; })
 (final: prev: { gnat = prev.gnat13; })

 # prettier-ada requires bleeding edge gnatcoll-core
 (final: prev: { gnatcoll-core = prev.gnatcoll-core.overrideAttrs (final:
    {
      version = "24.2-20240612-git";
      src = with prev.fetchigt; fetchGit {
        url = "https://github.com/AdaCore/gnatcoll-core.git";
        ref = "master";
        rev = "6c21f915662c95e1f693ca37abbfd25d43d94895";
      };
    }); })

 # We need a version of gnatcoll-python that supports newer Python versions.
 (final: prev: { gnatcoll-python3 = ((prev.gnatcoll-python3.overrideAttrs (final:
    {
      version = "24.2";
      src = prev.fetchzip {
         url = "https://github.com/AdaCore/gnatcoll-bindings/archive/refs/heads/24.2.zip";
         hash = "sha256-LoeZspeO5siSuIcA6iuRABlpfSlpJJuxnhSvlbIYfzE=";
      };
    })).override { python3 = prev.python3; }); }) # Override hardcoded Python version
]

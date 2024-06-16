# We require GNAT 13 to match the current system compiler GCC 13
{}:

[
 (final: prev: { gnat12 = prev.gnat13; })
 (final: prev: { gnat = prev.gnat13; })
]

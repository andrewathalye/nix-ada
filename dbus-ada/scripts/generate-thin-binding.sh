#!/bin/sh
set -Ceuvx

# This script once generated the current thin binding.
# Since then, the way GCC generates Ada sources and the dbus header
# may have changed, so running it again will most probably produce a
# different binding, requiring manual changes in the Ada code.
# It is provided to document the process in order to make next update
# or regeneration easier.

# Some files appear unused, but this actually depends on the
# architecture, so removing them is a risk for little benefit.

# If an argument is provided, it is used as the g++ executable.

thindir=thin
dbus_lib=dbus-1
dbus_includes="`pkg-config --cflags-only-I $dbus_lib | sed s/-I//g`"
dbus_h=`find $dbus_includes -name dbus.h`
CXX=${1:-g++-10}

if ! mkdir $thindir; then
    echo "The $thindir/ subdirectory exists, and may contain manual changes."
    echo "Please confirm by deleting or renaming it."
    exit 1
fi
cd $thindir

$CXX --dump-ada-spec $dbus_h `pkg-config --cflags $dbus_lib`

# Remove arch, path or version dependent prefixes.
for i in *dbus_dbus_*; do
    mv $i `echo $i | sed 's/.*dbus_dbus_/dbus_/'`
done
sed -i 's/[a-z0-9_]*dbus_dbus_/dbus_/g' *

# Remove arch or version dependent paths from comments.
# (for reproducible builds and readability)
sed -i 's|  -- /usr/.*/|  -- |' *

# Fix a warning about unused entities.
sed -i '/^with Interfaces\.C; use Interfaces\.C;$/d' \
    dbus_h.ads \
    dbus_memory_h.ads \
    dbus_pending_call_h.ads \
    dbus_shared_h.ads \
    dbus_syntax_h.ads \
    stdarg_h.ads

# On arm64 armel armhf,
# a reference to stdarg_h.Class_va_list is generated without stdarg_h.
# dbusada does not need this reference, remove it.
sed -i \
    -e '/^with stdarg_h;$/d' \
    -e '/^   function dbus_message_\(append\|get\)_args_valist$/{:l;N;/\n$/d;bl}' \
    dbus_message_h.ads

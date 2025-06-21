pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package dbus_arch_deps_h is

   DBUS_HAVE_INT64 : constant := 1;  --  /usr/lib/x86_64-linux-gnu/dbus-1.0/include/dbus/dbus-arch-deps.h:35
   --  unsupported macro: DBUS_INT64_CONSTANT(val) (_DBUS_GNUC_EXTENSION (val ##L))
   --  unsupported macro: DBUS_UINT64_CONSTANT(val) (_DBUS_GNUC_EXTENSION (val ##UL))

   DBUS_MAJOR_VERSION : constant := 1;  --  /usr/lib/x86_64-linux-gnu/dbus-1.0/include/dbus/dbus-arch-deps.h:51
   DBUS_MINOR_VERSION : constant := 12;  --  /usr/lib/x86_64-linux-gnu/dbus-1.0/include/dbus/dbus-arch-deps.h:52
   DBUS_MICRO_VERSION : constant := 20;  --  /usr/lib/x86_64-linux-gnu/dbus-1.0/include/dbus/dbus-arch-deps.h:53

   DBUS_VERSION_STRING : aliased constant String := "1.12.20" & ASCII.NUL;  --  /usr/lib/x86_64-linux-gnu/dbus-1.0/include/dbus/dbus-arch-deps.h:55
   --  unsupported macro: DBUS_VERSION ((1 << 16) | (12 << 8) | (20))

   subtype dbus_int64_t is long;  -- /usr/lib/x86_64-linux-gnu/dbus-1.0/include/dbus/dbus-arch-deps.h:36

   subtype dbus_uint64_t is unsigned_long;  -- /usr/lib/x86_64-linux-gnu/dbus-1.0/include/dbus/dbus-arch-deps.h:37

   subtype dbus_int32_t is int;  -- /usr/lib/x86_64-linux-gnu/dbus-1.0/include/dbus/dbus-arch-deps.h:42

   subtype dbus_uint32_t is unsigned;  -- /usr/lib/x86_64-linux-gnu/dbus-1.0/include/dbus/dbus-arch-deps.h:43

   subtype dbus_int16_t is short;  -- /usr/lib/x86_64-linux-gnu/dbus-1.0/include/dbus/dbus-arch-deps.h:45

   subtype dbus_uint16_t is unsigned_short;  -- /usr/lib/x86_64-linux-gnu/dbus-1.0/include/dbus/dbus-arch-deps.h:46

end dbus_arch_deps_h;

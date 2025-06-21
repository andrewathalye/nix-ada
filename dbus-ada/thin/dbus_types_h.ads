pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with dbus_arch_deps_h;
with Interfaces.C.Strings;

package dbus_types_h is

   subtype dbus_unichar_t is dbus_arch_deps_h.dbus_uint32_t;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:33

   subtype dbus_bool_t is dbus_arch_deps_h.dbus_uint32_t;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:35

   --  skipped anonymous struct anon_anon_1

   type DBus8ByteStruct is record
      first32 : aliased dbus_arch_deps_h.dbus_uint32_t;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:123
      second32 : aliased dbus_arch_deps_h.dbus_uint32_t;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:124
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:125

   --  skipped anonymous struct anon_anon_2

   type DBusBasicValue_array849 is array (0 .. 7) of aliased unsigned_char;
   type DBusBasicValue (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            bytes : aliased DBusBasicValue_array849;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:139
         when 1 =>
            i16 : aliased dbus_arch_deps_h.dbus_int16_t;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:140
         when 2 =>
            u16 : aliased dbus_arch_deps_h.dbus_uint16_t;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:141
         when 3 =>
            i32 : aliased dbus_arch_deps_h.dbus_int32_t;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:142
         when 4 =>
            u32 : aliased dbus_arch_deps_h.dbus_uint32_t;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:143
         when 5 =>
            bool_val : aliased dbus_bool_t;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:144
         when 6 =>
            i64 : aliased dbus_arch_deps_h.dbus_int64_t;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:145
         when 7 =>
            u64 : aliased dbus_arch_deps_h.dbus_uint64_t;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:146
         when 8 =>
            eight : aliased DBus8ByteStruct;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:147
         when 9 =>
            dbl : aliased double;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:148
         when 10 =>
            byt : aliased unsigned_char;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:149
         when 11 =>
            str : Interfaces.C.Strings.chars_ptr;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:150
         when others =>
            fd : aliased int;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:151
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;  -- /usr/include/dbus-1.0/dbus/dbus-types.h:152

end dbus_types_h;

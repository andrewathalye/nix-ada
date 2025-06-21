pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
limited with dbus_errors_h;
with dbus_types_h;

package dbus_address_h is

   type DBusAddressEntry is null record;   -- incomplete struct

   function dbus_parse_address
     (address : Interfaces.C.Strings.chars_ptr;
      entry_result : System.Address;
      array_len : access int;
      error : access dbus_errors_h.DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-address.h:44
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_parse_address";

   function dbus_address_entry_get_value (c_entry : access DBusAddressEntry; key : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/dbus-1.0/dbus/dbus-address.h:49
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_address_entry_get_value";

   function dbus_address_entry_get_method (c_entry : access DBusAddressEntry) return Interfaces.C.Strings.chars_ptr  -- /usr/include/dbus-1.0/dbus/dbus-address.h:52
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_address_entry_get_method";

   procedure dbus_address_entries_free (entries : System.Address)  -- /usr/include/dbus-1.0/dbus/dbus-address.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_address_entries_free";

   function dbus_address_escape_value (value : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/dbus-1.0/dbus/dbus-address.h:57
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_address_escape_value";

   function dbus_address_unescape_value (value : Interfaces.C.Strings.chars_ptr; error : access dbus_errors_h.DBusError) return Interfaces.C.Strings.chars_ptr  -- /usr/include/dbus-1.0/dbus/dbus-address.h:59
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_address_unescape_value";

   procedure dbus_clear_address_entries (pointer_to_entries : System.Address)  -- /usr/include/dbus-1.0/dbus/dbus-address.h:75
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_clear_address_entries";

end dbus_address_h;

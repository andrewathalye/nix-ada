pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C.Strings;
limited with dbus_errors_h;
with dbus_types_h;

package dbus_syntax_h is

   function dbus_validate_path (path : Interfaces.C.Strings.chars_ptr; error : access dbus_errors_h.DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-syntax.h:38
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_validate_path";

   function dbus_validate_interface (name : Interfaces.C.Strings.chars_ptr; error : access dbus_errors_h.DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-syntax.h:41
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_validate_interface";

   function dbus_validate_member (name : Interfaces.C.Strings.chars_ptr; error : access dbus_errors_h.DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-syntax.h:44
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_validate_member";

   function dbus_validate_error_name (name : Interfaces.C.Strings.chars_ptr; error : access dbus_errors_h.DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-syntax.h:47
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_validate_error_name";

   function dbus_validate_bus_name (name : Interfaces.C.Strings.chars_ptr; error : access dbus_errors_h.DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-syntax.h:50
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_validate_bus_name";

   function dbus_validate_utf8 (alleged_utf8 : Interfaces.C.Strings.chars_ptr; error : access dbus_errors_h.DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-syntax.h:53
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_validate_utf8";

end dbus_syntax_h;

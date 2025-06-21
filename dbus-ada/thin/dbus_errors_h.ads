pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;
with System;
with dbus_types_h;

package dbus_errors_h is

   --  unsupported macro: DBUS_ERROR_INIT { NULL, NULL, TRUE, 0, 0, 0, 0, NULL }
   type DBusError;
   type DBusError is record
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:50
      message : Interfaces.C.Strings.chars_ptr;  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:51
      dummy1 : Extensions.Unsigned_1;  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:53
      dummy2 : Extensions.Unsigned_1;  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:54
      dummy3 : Extensions.Unsigned_1;  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:55
      dummy4 : Extensions.Unsigned_1;  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:56
      dummy5 : Extensions.Unsigned_1;  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:57
      padding1 : System.Address;  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:59
   end record
   with Convention => C_Pass_By_Copy,
        Pack => True;  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:48

   procedure dbus_error_init (error : access DBusError)  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:65
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_error_init";

   procedure dbus_error_free (error : access DBusError)  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:67
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_error_free";

   procedure dbus_set_error
     (error : access DBusError;
      name : Interfaces.C.Strings.chars_ptr;
      message : Interfaces.C.Strings.chars_ptr  -- , ...
      )  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:69
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_set_error";

   procedure dbus_set_error_const
     (error : access DBusError;
      name : Interfaces.C.Strings.chars_ptr;
      message : Interfaces.C.Strings.chars_ptr)  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:74
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_set_error_const";

   procedure dbus_move_error (src : access DBusError; dest : access DBusError)  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:78
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_move_error";

   function dbus_error_has_name (error : access constant DBusError; name : Interfaces.C.Strings.chars_ptr) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:81
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_error_has_name";

   function dbus_error_is_set (error : access constant DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-errors.h:84
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_error_is_set";

end dbus_errors_h;

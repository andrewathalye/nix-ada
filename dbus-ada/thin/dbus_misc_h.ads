pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with dbus_types_h;
limited with dbus_errors_h;

package dbus_misc_h is

   function dbus_get_local_machine_id return Interfaces.C.Strings.chars_ptr  -- /usr/include/dbus-1.0/dbus/dbus-misc.h:40
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_get_local_machine_id";

   procedure dbus_get_version
     (major_version_p : access int;
      minor_version_p : access int;
      micro_version_p : access int)  -- /usr/include/dbus-1.0/dbus/dbus-misc.h:43
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_get_version";

   function dbus_setenv (variable : Interfaces.C.Strings.chars_ptr; value : Interfaces.C.Strings.chars_ptr) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-misc.h:48
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_setenv";

   function dbus_try_get_local_machine_id (error : access dbus_errors_h.DBusError) return Interfaces.C.Strings.chars_ptr  -- /usr/include/dbus-1.0/dbus/dbus-misc.h:52
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_try_get_local_machine_id";

end dbus_misc_h;

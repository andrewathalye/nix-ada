pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with dbus_shared_h;
limited with dbus_errors_h;
limited with dbus_connection_h;
with dbus_types_h;
with Interfaces.C.Strings;
with dbus_arch_deps_h;

package dbus_bus_h is

   function dbus_bus_get (c_type : dbus_shared_h.DBusBusType; error : access dbus_errors_h.DBusError) return access dbus_connection_h.DBusConnection  -- /usr/include/dbus-1.0/dbus/dbus-bus.h:40
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_bus_get";

   function dbus_bus_get_private (c_type : dbus_shared_h.DBusBusType; error : access dbus_errors_h.DBusError) return access dbus_connection_h.DBusConnection  -- /usr/include/dbus-1.0/dbus/dbus-bus.h:43
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_bus_get_private";

   function dbus_bus_register (connection : access dbus_connection_h.DBusConnection; error : access dbus_errors_h.DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-bus.h:47
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_bus_register";

   function dbus_bus_set_unique_name (connection : access dbus_connection_h.DBusConnection; unique_name : Interfaces.C.Strings.chars_ptr) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-bus.h:50
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_bus_set_unique_name";

   function dbus_bus_get_unique_name (connection : access dbus_connection_h.DBusConnection) return Interfaces.C.Strings.chars_ptr  -- /usr/include/dbus-1.0/dbus/dbus-bus.h:53
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_bus_get_unique_name";

   function dbus_bus_get_unix_user
     (connection : access dbus_connection_h.DBusConnection;
      name : Interfaces.C.Strings.chars_ptr;
      error : access dbus_errors_h.DBusError) return unsigned_long  -- /usr/include/dbus-1.0/dbus/dbus-bus.h:55
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_bus_get_unix_user";

   function dbus_bus_get_id (connection : access dbus_connection_h.DBusConnection; error : access dbus_errors_h.DBusError) return Interfaces.C.Strings.chars_ptr  -- /usr/include/dbus-1.0/dbus/dbus-bus.h:59
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_bus_get_id";

   function dbus_bus_request_name
     (connection : access dbus_connection_h.DBusConnection;
      name : Interfaces.C.Strings.chars_ptr;
      flags : unsigned;
      error : access dbus_errors_h.DBusError) return int  -- /usr/include/dbus-1.0/dbus/dbus-bus.h:62
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_bus_request_name";

   function dbus_bus_release_name
     (connection : access dbus_connection_h.DBusConnection;
      name : Interfaces.C.Strings.chars_ptr;
      error : access dbus_errors_h.DBusError) return int  -- /usr/include/dbus-1.0/dbus/dbus-bus.h:67
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_bus_release_name";

   function dbus_bus_name_has_owner
     (connection : access dbus_connection_h.DBusConnection;
      name : Interfaces.C.Strings.chars_ptr;
      error : access dbus_errors_h.DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-bus.h:71
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_bus_name_has_owner";

   function dbus_bus_start_service_by_name
     (connection : access dbus_connection_h.DBusConnection;
      name : Interfaces.C.Strings.chars_ptr;
      flags : dbus_arch_deps_h.dbus_uint32_t;
      reply : access dbus_arch_deps_h.dbus_uint32_t;
      error : access dbus_errors_h.DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-bus.h:76
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_bus_start_service_by_name";

   procedure dbus_bus_add_match
     (connection : access dbus_connection_h.DBusConnection;
      rule : Interfaces.C.Strings.chars_ptr;
      error : access dbus_errors_h.DBusError)  -- /usr/include/dbus-1.0/dbus/dbus-bus.h:83
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_bus_add_match";

   procedure dbus_bus_remove_match
     (connection : access dbus_connection_h.DBusConnection;
      rule : Interfaces.C.Strings.chars_ptr;
      error : access dbus_errors_h.DBusError)  -- /usr/include/dbus-1.0/dbus/dbus-bus.h:87
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_bus_remove_match";

end dbus_bus_h;

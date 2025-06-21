pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with dbus_connection_h;
with System;
with Interfaces.C.Strings;
limited with dbus_errors_h;
with dbus_types_h;
with dbus_memory_h;
with dbus_arch_deps_h;

package dbus_server_h is

   type DBusServer is null record;   -- incomplete struct

   type DBusNewConnectionFunction is access procedure
        (arg1 : access DBusServer;
         arg2 : access dbus_connection_h.DBusConnection;
         arg3 : System.Address)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-server.h:48

   function dbus_server_listen (address : Interfaces.C.Strings.chars_ptr; error : access dbus_errors_h.DBusError) return access DBusServer  -- /usr/include/dbus-1.0/dbus/dbus-server.h:53
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_listen";

   function dbus_server_ref (server : access DBusServer) return access DBusServer  -- /usr/include/dbus-1.0/dbus/dbus-server.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_ref";

   procedure dbus_server_unref (server : access DBusServer)  -- /usr/include/dbus-1.0/dbus/dbus-server.h:58
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_unref";

   procedure dbus_server_disconnect (server : access DBusServer)  -- /usr/include/dbus-1.0/dbus/dbus-server.h:60
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_disconnect";

   function dbus_server_get_is_connected (server : access DBusServer) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-server.h:62
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_get_is_connected";

   function dbus_server_get_address (server : access DBusServer) return Interfaces.C.Strings.chars_ptr  -- /usr/include/dbus-1.0/dbus/dbus-server.h:64
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_get_address";

   function dbus_server_get_id (server : access DBusServer) return Interfaces.C.Strings.chars_ptr  -- /usr/include/dbus-1.0/dbus/dbus-server.h:66
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_get_id";

   procedure dbus_server_set_new_connection_function
     (server : access DBusServer;
      c_function : DBusNewConnectionFunction;
      data : System.Address;
      free_data_function : dbus_memory_h.DBusFreeFunction)  -- /usr/include/dbus-1.0/dbus/dbus-server.h:68
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_set_new_connection_function";

   function dbus_server_set_watch_functions
     (server : access DBusServer;
      add_function : dbus_connection_h.DBusAddWatchFunction;
      remove_function : dbus_connection_h.DBusRemoveWatchFunction;
      toggled_function : dbus_connection_h.DBusWatchToggledFunction;
      data : System.Address;
      free_data_function : dbus_memory_h.DBusFreeFunction) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-server.h:73
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_set_watch_functions";

   function dbus_server_set_timeout_functions
     (server : access DBusServer;
      add_function : dbus_connection_h.DBusAddTimeoutFunction;
      remove_function : dbus_connection_h.DBusRemoveTimeoutFunction;
      toggled_function : dbus_connection_h.DBusTimeoutToggledFunction;
      data : System.Address;
      free_data_function : dbus_memory_h.DBusFreeFunction) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-server.h:80
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_set_timeout_functions";

   function dbus_server_set_auth_mechanisms (server : access DBusServer; mechanisms : System.Address) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-server.h:87
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_set_auth_mechanisms";

   function dbus_server_allocate_data_slot (slot_p : access dbus_arch_deps_h.dbus_int32_t) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-server.h:91
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_allocate_data_slot";

   procedure dbus_server_free_data_slot (slot_p : access dbus_arch_deps_h.dbus_int32_t)  -- /usr/include/dbus-1.0/dbus/dbus-server.h:93
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_free_data_slot";

   function dbus_server_set_data
     (server : access DBusServer;
      slot : int;
      data : System.Address;
      free_data_func : dbus_memory_h.DBusFreeFunction) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-server.h:95
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_set_data";

   function dbus_server_get_data (server : access DBusServer; slot : int) return System.Address  -- /usr/include/dbus-1.0/dbus/dbus-server.h:100
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_server_get_data";

   procedure dbus_clear_server (pointer_to_server : System.Address)  -- /usr/include/dbus-1.0/dbus/dbus-server.h:116
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_clear_server";

end dbus_server_h;

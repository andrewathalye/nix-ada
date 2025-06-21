pragma Ada_2012;
pragma Style_Checks (Off);

with stddef_h;
with System;

package dbus_memory_h is

   --  arg-macro: function dbus_new (type, count)
   --    return (type*)dbus_malloc (sizeof (type) * (count));
   --  arg-macro: function dbus_new0 (type, count)
   --    return (type*)dbus_malloc0 (sizeof (type) * (count));
   function dbus_malloc (bytes : stddef_h.size_t) return System.Address  -- /usr/include/dbus-1.0/dbus/dbus-memory.h:43
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_malloc";

   function dbus_malloc0 (bytes : stddef_h.size_t) return System.Address  -- /usr/include/dbus-1.0/dbus/dbus-memory.h:48
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_malloc0";

   function dbus_realloc (memory : System.Address; bytes : stddef_h.size_t) return System.Address  -- /usr/include/dbus-1.0/dbus/dbus-memory.h:52
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_realloc";

   procedure dbus_free (memory : System.Address)  -- /usr/include/dbus-1.0/dbus/dbus-memory.h:55
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_free";

   procedure dbus_free_string_array (str_array : System.Address)  -- /usr/include/dbus-1.0/dbus/dbus-memory.h:61
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_free_string_array";

   type DBusFreeFunction is access procedure (arg1 : System.Address)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-memory.h:63

   procedure dbus_shutdown  -- /usr/include/dbus-1.0/dbus/dbus-memory.h:66
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_shutdown";

end dbus_memory_h;

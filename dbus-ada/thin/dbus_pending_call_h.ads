pragma Ada_2012;
pragma Style_Checks (Off);

with dbus_connection_h;
with System;
with dbus_memory_h;
with dbus_types_h;
limited with dbus_message_h;
with dbus_arch_deps_h;

package dbus_pending_call_h is

   --  unsupported macro: DBUS_TIMEOUT_INFINITE ((int) 0x7fffffff)
   DBUS_TIMEOUT_USE_DEFAULT : constant := (-1);  --  /usr/include/dbus-1.0/dbus/dbus-pending-call.h:42

   function dbus_pending_call_ref (pending : access dbus_connection_h.DBusPendingCall) return access dbus_connection_h.DBusPendingCall  -- /usr/include/dbus-1.0/dbus/dbus-pending-call.h:45
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_pending_call_ref";

   procedure dbus_pending_call_unref (pending : access dbus_connection_h.DBusPendingCall)  -- /usr/include/dbus-1.0/dbus/dbus-pending-call.h:47
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_pending_call_unref";

   function dbus_pending_call_set_notify
     (pending : access dbus_connection_h.DBusPendingCall;
      c_function : dbus_connection_h.DBusPendingCallNotifyFunction;
      user_data : System.Address;
      free_user_data : dbus_memory_h.DBusFreeFunction) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-pending-call.h:49
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_pending_call_set_notify";

   procedure dbus_pending_call_cancel (pending : access dbus_connection_h.DBusPendingCall)  -- /usr/include/dbus-1.0/dbus/dbus-pending-call.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_pending_call_cancel";

   function dbus_pending_call_get_completed (pending : access dbus_connection_h.DBusPendingCall) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-pending-call.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_pending_call_get_completed";

   function dbus_pending_call_steal_reply (pending : access dbus_connection_h.DBusPendingCall) return access dbus_message_h.DBusMessage  -- /usr/include/dbus-1.0/dbus/dbus-pending-call.h:58
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_pending_call_steal_reply";

   procedure dbus_pending_call_block (pending : access dbus_connection_h.DBusPendingCall)  -- /usr/include/dbus-1.0/dbus/dbus-pending-call.h:60
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_pending_call_block";

   function dbus_pending_call_allocate_data_slot (slot_p : access dbus_arch_deps_h.dbus_int32_t) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-pending-call.h:63
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_pending_call_allocate_data_slot";

   procedure dbus_pending_call_free_data_slot (slot_p : access dbus_arch_deps_h.dbus_int32_t)  -- /usr/include/dbus-1.0/dbus/dbus-pending-call.h:65
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_pending_call_free_data_slot";

   function dbus_pending_call_set_data
     (pending : access dbus_connection_h.DBusPendingCall;
      slot : dbus_arch_deps_h.dbus_int32_t;
      data : System.Address;
      free_data_func : dbus_memory_h.DBusFreeFunction) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-pending-call.h:67
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_pending_call_set_data";

   function dbus_pending_call_get_data (pending : access dbus_connection_h.DBusPendingCall; slot : dbus_arch_deps_h.dbus_int32_t) return System.Address  -- /usr/include/dbus-1.0/dbus/dbus-pending-call.h:72
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_pending_call_get_data";

   procedure dbus_clear_pending_call (pointer_to_pending_call : System.Address)  -- /usr/include/dbus-1.0/dbus/dbus-pending-call.h:88
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_clear_pending_call";

end dbus_pending_call_h;

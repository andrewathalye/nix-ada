pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with dbus_types_h;
with Interfaces.C.Strings;
limited with dbus_message_h;
with dbus_shared_h;
limited with dbus_errors_h;
with dbus_arch_deps_h;
with dbus_memory_h;

package dbus_connection_h is

   type DBusWatch is null record;   -- incomplete struct

   type DBusTimeout is null record;   -- incomplete struct

   type DBusPreallocatedSend is null record;   -- incomplete struct

   type DBusPendingCall is null record;   -- incomplete struct

   type DBusConnection is null record;   -- incomplete struct

   type DBusObjectPathVTable;
   subtype DBusWatchFlags is unsigned;
   DBUS_WATCH_READABLE : constant unsigned := 1;
   DBUS_WATCH_WRITABLE : constant unsigned := 2;
   DBUS_WATCH_ERROR : constant unsigned := 4;
   DBUS_WATCH_HANGUP : constant unsigned := 8;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:74

   type DBusDispatchStatus is 
     (DBUS_DISPATCH_DATA_REMAINS,
      DBUS_DISPATCH_COMPLETE,
      DBUS_DISPATCH_NEED_MEMORY)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:85

   type DBusAddWatchFunction is access function (arg1 : access DBusWatch; arg2 : System.Address) return dbus_types_h.dbus_bool_t
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:92

   type DBusWatchToggledFunction is access procedure (arg1 : access DBusWatch; arg2 : System.Address)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:98

   type DBusRemoveWatchFunction is access procedure (arg1 : access DBusWatch; arg2 : System.Address)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:104

   type DBusAddTimeoutFunction is access function (arg1 : access DBusTimeout; arg2 : System.Address) return dbus_types_h.dbus_bool_t
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:111

   type DBusTimeoutToggledFunction is access procedure (arg1 : access DBusTimeout; arg2 : System.Address)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:118

   type DBusRemoveTimeoutFunction is access procedure (arg1 : access DBusTimeout; arg2 : System.Address)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:124

   type DBusDispatchStatusFunction is access procedure
        (arg1 : access DBusConnection;
         arg2 : DBusDispatchStatus;
         arg3 : System.Address)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:129

   type DBusWakeupMainFunction is access procedure (arg1 : System.Address)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:136

   type DBusAllowUnixUserFunction is access function
        (arg1 : access DBusConnection;
         arg2 : unsigned_long;
         arg3 : System.Address) return dbus_types_h.dbus_bool_t
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:144

   type DBusAllowWindowsUserFunction is access function
        (arg1 : access DBusConnection;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : System.Address) return dbus_types_h.dbus_bool_t
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:154

   type DBusPendingCallNotifyFunction is access procedure (arg1 : access DBusPendingCall; arg2 : System.Address)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:163

   type DBusHandleMessageFunction is access function
        (arg1 : access DBusConnection;
         arg2 : access dbus_message_h.DBusMessage;
         arg3 : System.Address) return dbus_shared_h.DBusHandlerResult
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:170

   function dbus_connection_open (address : Interfaces.C.Strings.chars_ptr; error : access dbus_errors_h.DBusError) return access DBusConnection  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:174
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_open";

   function dbus_connection_open_private (address : Interfaces.C.Strings.chars_ptr; error : access dbus_errors_h.DBusError) return access DBusConnection  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:177
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_open_private";

   function dbus_connection_ref (connection : access DBusConnection) return access DBusConnection  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:180
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_ref";

   procedure dbus_connection_unref (connection : access DBusConnection)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:182
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_unref";

   procedure dbus_connection_close (connection : access DBusConnection)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:184
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_close";

   function dbus_connection_get_is_connected (connection : access DBusConnection) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:186
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_is_connected";

   function dbus_connection_get_is_authenticated (connection : access DBusConnection) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:188
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_is_authenticated";

   function dbus_connection_get_is_anonymous (connection : access DBusConnection) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:190
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_is_anonymous";

   function dbus_connection_get_server_id (connection : access DBusConnection) return Interfaces.C.Strings.chars_ptr  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:192
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_server_id";

   function dbus_connection_can_send_type (connection : access DBusConnection; c_type : int) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:194
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_can_send_type";

   procedure dbus_connection_set_exit_on_disconnect (connection : access DBusConnection; exit_on_disconnect : dbus_types_h.dbus_bool_t)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:198
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_exit_on_disconnect";

   procedure dbus_connection_flush (connection : access DBusConnection)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:201
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_flush";

   function dbus_connection_read_write_dispatch (connection : access DBusConnection; timeout_milliseconds : int) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:203
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_read_write_dispatch";

   function dbus_connection_read_write (connection : access DBusConnection; timeout_milliseconds : int) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:206
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_read_write";

   function dbus_connection_borrow_message (connection : access DBusConnection) return access dbus_message_h.DBusMessage  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:209
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_borrow_message";

   procedure dbus_connection_return_message (connection : access DBusConnection; message : access dbus_message_h.DBusMessage)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:211
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_return_message";

   procedure dbus_connection_steal_borrowed_message (connection : access DBusConnection; message : access dbus_message_h.DBusMessage)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:214
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_steal_borrowed_message";

   function dbus_connection_pop_message (connection : access DBusConnection) return access dbus_message_h.DBusMessage  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:217
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_pop_message";

   function dbus_connection_get_dispatch_status (connection : access DBusConnection) return DBusDispatchStatus  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:219
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_dispatch_status";

   function dbus_connection_dispatch (connection : access DBusConnection) return DBusDispatchStatus  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:221
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_dispatch";

   function dbus_connection_has_messages_to_send (connection : access DBusConnection) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:223
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_has_messages_to_send";

   function dbus_connection_send
     (connection : access DBusConnection;
      message : access dbus_message_h.DBusMessage;
      client_serial : access dbus_arch_deps_h.dbus_uint32_t) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:225
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_send";

   function dbus_connection_send_with_reply
     (connection : access DBusConnection;
      message : access dbus_message_h.DBusMessage;
      pending_return : System.Address;
      timeout_milliseconds : int) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:229
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_send_with_reply";

   function dbus_connection_send_with_reply_and_block
     (connection : access DBusConnection;
      message : access dbus_message_h.DBusMessage;
      timeout_milliseconds : int;
      error : access dbus_errors_h.DBusError) return access dbus_message_h.DBusMessage  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:234
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_send_with_reply_and_block";

   function dbus_connection_set_watch_functions
     (connection : access DBusConnection;
      add_function : DBusAddWatchFunction;
      remove_function : DBusRemoveWatchFunction;
      toggled_function : DBusWatchToggledFunction;
      data : System.Address;
      free_data_function : dbus_memory_h.DBusFreeFunction) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:239
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_watch_functions";

   function dbus_connection_set_timeout_functions
     (connection : access DBusConnection;
      add_function : DBusAddTimeoutFunction;
      remove_function : DBusRemoveTimeoutFunction;
      toggled_function : DBusTimeoutToggledFunction;
      data : System.Address;
      free_data_function : dbus_memory_h.DBusFreeFunction) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:246
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_timeout_functions";

   procedure dbus_connection_set_wakeup_main_function
     (connection : access DBusConnection;
      wakeup_main_function : DBusWakeupMainFunction;
      data : System.Address;
      free_data_function : dbus_memory_h.DBusFreeFunction)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:253
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_wakeup_main_function";

   procedure dbus_connection_set_dispatch_status_function
     (connection : access DBusConnection;
      c_function : DBusDispatchStatusFunction;
      data : System.Address;
      free_data_function : dbus_memory_h.DBusFreeFunction)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:258
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_dispatch_status_function";

   function dbus_connection_get_unix_user (connection : access DBusConnection; uid : access unsigned_long) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:263
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_unix_user";

   function dbus_connection_get_unix_process_id (connection : access DBusConnection; pid : access unsigned_long) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:266
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_unix_process_id";

   function dbus_connection_get_adt_audit_session_data
     (connection : access DBusConnection;
      data : System.Address;
      data_size : access dbus_arch_deps_h.dbus_int32_t) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:269
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_adt_audit_session_data";

   procedure dbus_connection_set_unix_user_function
     (connection : access DBusConnection;
      c_function : DBusAllowUnixUserFunction;
      data : System.Address;
      free_data_function : dbus_memory_h.DBusFreeFunction)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:273
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_unix_user_function";

   function dbus_connection_get_windows_user (connection : access DBusConnection; windows_sid_p : System.Address) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:278
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_windows_user";

   procedure dbus_connection_set_windows_user_function
     (connection : access DBusConnection;
      c_function : DBusAllowWindowsUserFunction;
      data : System.Address;
      free_data_function : dbus_memory_h.DBusFreeFunction)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:281
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_windows_user_function";

   procedure dbus_connection_set_allow_anonymous (connection : access DBusConnection; value : dbus_types_h.dbus_bool_t)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:286
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_allow_anonymous";

   procedure dbus_connection_set_route_peer_messages (connection : access DBusConnection; value : dbus_types_h.dbus_bool_t)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:289
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_route_peer_messages";

   function dbus_connection_add_filter
     (connection : access DBusConnection;
      c_function : DBusHandleMessageFunction;
      user_data : System.Address;
      free_data_function : dbus_memory_h.DBusFreeFunction) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:296
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_add_filter";

   procedure dbus_connection_remove_filter
     (connection : access DBusConnection;
      c_function : DBusHandleMessageFunction;
      user_data : System.Address)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:301
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_remove_filter";

   function dbus_connection_allocate_data_slot (slot_p : access dbus_arch_deps_h.dbus_int32_t) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:308
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_allocate_data_slot";

   procedure dbus_connection_free_data_slot (slot_p : access dbus_arch_deps_h.dbus_int32_t)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:310
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_free_data_slot";

   function dbus_connection_set_data
     (connection : access DBusConnection;
      slot : dbus_arch_deps_h.dbus_int32_t;
      data : System.Address;
      free_data_func : dbus_memory_h.DBusFreeFunction) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:312
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_data";

   function dbus_connection_get_data (connection : access DBusConnection; slot : dbus_arch_deps_h.dbus_int32_t) return System.Address  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:317
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_data";

   procedure dbus_connection_set_change_sigpipe (will_modify_sigpipe : dbus_types_h.dbus_bool_t)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:321
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_change_sigpipe";

   procedure dbus_connection_set_max_message_size (connection : access DBusConnection; size : long)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:324
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_max_message_size";

   function dbus_connection_get_max_message_size (connection : access DBusConnection) return long  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:327
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_max_message_size";

   procedure dbus_connection_set_max_received_size (connection : access DBusConnection; size : long)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:329
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_max_received_size";

   function dbus_connection_get_max_received_size (connection : access DBusConnection) return long  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:332
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_max_received_size";

   procedure dbus_connection_set_max_message_unix_fds (connection : access DBusConnection; n : long)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:335
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_max_message_unix_fds";

   function dbus_connection_get_max_message_unix_fds (connection : access DBusConnection) return long  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:338
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_max_message_unix_fds";

   procedure dbus_connection_set_max_received_unix_fds (connection : access DBusConnection; n : long)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:340
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_set_max_received_unix_fds";

   function dbus_connection_get_max_received_unix_fds (connection : access DBusConnection) return long  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:343
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_max_received_unix_fds";

   function dbus_connection_get_outgoing_size (connection : access DBusConnection) return long  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:346
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_outgoing_size";

   function dbus_connection_get_outgoing_unix_fds (connection : access DBusConnection) return long  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:348
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_outgoing_unix_fds";

   function dbus_connection_preallocate_send (connection : access DBusConnection) return access DBusPreallocatedSend  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:351
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_preallocate_send";

   procedure dbus_connection_free_preallocated_send (connection : access DBusConnection; preallocated : access DBusPreallocatedSend)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:353
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_free_preallocated_send";

   procedure dbus_connection_send_preallocated
     (connection : access DBusConnection;
      preallocated : access DBusPreallocatedSend;
      message : access dbus_message_h.DBusMessage;
      client_serial : access dbus_arch_deps_h.dbus_uint32_t)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:356
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_send_preallocated";

   type DBusObjectPathUnregisterFunction is access procedure (arg1 : access DBusConnection; arg2 : System.Address)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:368

   type DBusObjectPathMessageFunction is access function
        (arg1 : access DBusConnection;
         arg2 : access dbus_message_h.DBusMessage;
         arg3 : System.Address) return dbus_shared_h.DBusHandlerResult
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:375

   type DBusObjectPathVTable is record
      unregister_function : DBusObjectPathUnregisterFunction;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:387
      message_function : DBusObjectPathMessageFunction;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:388
      dbus_internal_pad1 : access procedure (arg1 : System.Address);  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:390
      dbus_internal_pad2 : access procedure (arg1 : System.Address);  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:391
      dbus_internal_pad3 : access procedure (arg1 : System.Address);  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:392
      dbus_internal_pad4 : access procedure (arg1 : System.Address);  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:393
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:385

   function dbus_connection_try_register_object_path
     (connection : access DBusConnection;
      path : Interfaces.C.Strings.chars_ptr;
      vtable : access constant DBusObjectPathVTable;
      user_data : System.Address;
      error : access dbus_errors_h.DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:397
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_try_register_object_path";

   function dbus_connection_register_object_path
     (connection : access DBusConnection;
      path : Interfaces.C.Strings.chars_ptr;
      vtable : access constant DBusObjectPathVTable;
      user_data : System.Address) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:404
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_register_object_path";

   function dbus_connection_try_register_fallback
     (connection : access DBusConnection;
      path : Interfaces.C.Strings.chars_ptr;
      vtable : access constant DBusObjectPathVTable;
      user_data : System.Address;
      error : access dbus_errors_h.DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:410
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_try_register_fallback";

   function dbus_connection_register_fallback
     (connection : access DBusConnection;
      path : Interfaces.C.Strings.chars_ptr;
      vtable : access constant DBusObjectPathVTable;
      user_data : System.Address) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:417
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_register_fallback";

   function dbus_connection_unregister_object_path (connection : access DBusConnection; path : Interfaces.C.Strings.chars_ptr) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:422
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_unregister_object_path";

   function dbus_connection_get_object_path_data
     (connection : access DBusConnection;
      path : Interfaces.C.Strings.chars_ptr;
      data_p : System.Address) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:426
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_object_path_data";

   function dbus_connection_list_registered
     (connection : access DBusConnection;
      parent_path : Interfaces.C.Strings.chars_ptr;
      child_entries : System.Address) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:431
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_list_registered";

   function dbus_connection_get_unix_fd (connection : access DBusConnection; fd : access int) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:436
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_unix_fd";

   function dbus_connection_get_socket (connection : access DBusConnection; fd : access int) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:439
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_connection_get_socket";

   procedure dbus_clear_connection (pointer_to_connection : System.Address)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:465
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_clear_connection";

   function dbus_watch_get_fd (watch : access DBusWatch) return int  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:481
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_watch_get_fd";

   function dbus_watch_get_unix_fd (watch : access DBusWatch) return int  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:485
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_watch_get_unix_fd";

   function dbus_watch_get_socket (watch : access DBusWatch) return int  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:487
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_watch_get_socket";

   function dbus_watch_get_flags (watch : access DBusWatch) return unsigned  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:489
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_watch_get_flags";

   function dbus_watch_get_data (watch : access DBusWatch) return System.Address  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:491
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_watch_get_data";

   procedure dbus_watch_set_data
     (watch : access DBusWatch;
      data : System.Address;
      free_data_function : dbus_memory_h.DBusFreeFunction)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:493
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_watch_set_data";

   function dbus_watch_handle (watch : access DBusWatch; flags : unsigned) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:497
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_watch_handle";

   function dbus_watch_get_enabled (watch : access DBusWatch) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:500
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_watch_get_enabled";

   function dbus_timeout_get_interval (timeout : access DBusTimeout) return int  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:510
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_timeout_get_interval";

   function dbus_timeout_get_data (timeout : access DBusTimeout) return System.Address  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:512
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_timeout_get_data";

   procedure dbus_timeout_set_data
     (timeout : access DBusTimeout;
      data : System.Address;
      free_data_function : dbus_memory_h.DBusFreeFunction)  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:514
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_timeout_set_data";

   function dbus_timeout_handle (timeout : access DBusTimeout) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:518
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_timeout_handle";

   function dbus_timeout_get_enabled (timeout : access DBusTimeout) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-connection.h:520
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_timeout_get_enabled";

end dbus_connection_h;

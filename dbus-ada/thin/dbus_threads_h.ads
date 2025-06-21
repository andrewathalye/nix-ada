pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with dbus_types_h;

package dbus_threads_h is

   type DBusMutex is null record;   -- incomplete struct

   type DBusCondVar is null record;   -- incomplete struct

   type DBusMutexNewFunction is access function return access DBusMutex
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:46

   type DBusMutexFreeFunction is access procedure (arg1 : access DBusMutex)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:48

   type DBusMutexLockFunction is access function (arg1 : access DBusMutex) return dbus_types_h.dbus_bool_t
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:50

   type DBusMutexUnlockFunction is access function (arg1 : access DBusMutex) return dbus_types_h.dbus_bool_t
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:52

   type DBusRecursiveMutexNewFunction is access function return access DBusMutex
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:61

   type DBusRecursiveMutexFreeFunction is access procedure (arg1 : access DBusMutex)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:64

   type DBusRecursiveMutexLockFunction is access procedure (arg1 : access DBusMutex)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:68

   type DBusRecursiveMutexUnlockFunction is access procedure (arg1 : access DBusMutex)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:72

   type DBusCondVarNewFunction is access function return access DBusCondVar
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:77

   type DBusCondVarFreeFunction is access procedure (arg1 : access DBusCondVar)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:80

   type DBusCondVarWaitFunction is access procedure (arg1 : access DBusCondVar; arg2 : access DBusMutex)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:92

   type DBusCondVarWaitTimeoutFunction is access function
        (arg1 : access DBusCondVar;
         arg2 : access DBusMutex;
         arg3 : int) return dbus_types_h.dbus_bool_t
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:101

   type DBusCondVarWakeOneFunction is access procedure (arg1 : access DBusCondVar)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:108

   type DBusCondVarWakeAllFunction is access procedure (arg1 : access DBusCondVar)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:114

   subtype DBusThreadFunctionsMask is unsigned;
   DBUS_THREAD_FUNCTIONS_MUTEX_NEW_MASK : constant unsigned := 1;
   DBUS_THREAD_FUNCTIONS_MUTEX_FREE_MASK : constant unsigned := 2;
   DBUS_THREAD_FUNCTIONS_MUTEX_LOCK_MASK : constant unsigned := 4;
   DBUS_THREAD_FUNCTIONS_MUTEX_UNLOCK_MASK : constant unsigned := 8;
   DBUS_THREAD_FUNCTIONS_CONDVAR_NEW_MASK : constant unsigned := 16;
   DBUS_THREAD_FUNCTIONS_CONDVAR_FREE_MASK : constant unsigned := 32;
   DBUS_THREAD_FUNCTIONS_CONDVAR_WAIT_MASK : constant unsigned := 64;
   DBUS_THREAD_FUNCTIONS_CONDVAR_WAIT_TIMEOUT_MASK : constant unsigned := 128;
   DBUS_THREAD_FUNCTIONS_CONDVAR_WAKE_ONE_MASK : constant unsigned := 256;
   DBUS_THREAD_FUNCTIONS_CONDVAR_WAKE_ALL_MASK : constant unsigned := 512;
   DBUS_THREAD_FUNCTIONS_RECURSIVE_MUTEX_NEW_MASK : constant unsigned := 1024;
   DBUS_THREAD_FUNCTIONS_RECURSIVE_MUTEX_FREE_MASK : constant unsigned := 2048;
   DBUS_THREAD_FUNCTIONS_RECURSIVE_MUTEX_LOCK_MASK : constant unsigned := 4096;
   DBUS_THREAD_FUNCTIONS_RECURSIVE_MUTEX_UNLOCK_MASK : constant unsigned := 8192;
   DBUS_THREAD_FUNCTIONS_ALL_MASK : constant unsigned := 16383;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:138

   --  skipped anonymous struct anon_anon_9

   type DBusThreadFunctions is record
      mask : aliased unsigned;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:154
      mutex_new : DBusMutexNewFunction;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:156
      mutex_free : DBusMutexFreeFunction;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:157
      mutex_lock : DBusMutexLockFunction;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:158
      mutex_unlock : DBusMutexUnlockFunction;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:159
      condvar_new : DBusCondVarNewFunction;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:161
      condvar_free : DBusCondVarFreeFunction;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:162
      condvar_wait : DBusCondVarWaitFunction;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:163
      condvar_wait_timeout : DBusCondVarWaitTimeoutFunction;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:164
      condvar_wake_one : DBusCondVarWakeOneFunction;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:165
      condvar_wake_all : DBusCondVarWakeAllFunction;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:166
      recursive_mutex_new : DBusRecursiveMutexNewFunction;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:168
      recursive_mutex_free : DBusRecursiveMutexFreeFunction;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:169
      recursive_mutex_lock : DBusRecursiveMutexLockFunction;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:170
      recursive_mutex_unlock : DBusRecursiveMutexUnlockFunction;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:171
      padding1 : access procedure;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:173
      padding2 : access procedure;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:174
      padding3 : access procedure;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:175
      padding4 : access procedure;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:176
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:178

   function dbus_threads_init (functions : access constant DBusThreadFunctions) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:181
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_threads_init";

   function dbus_threads_init_default return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-threads.h:183
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_threads_init_default";

end dbus_threads_h;

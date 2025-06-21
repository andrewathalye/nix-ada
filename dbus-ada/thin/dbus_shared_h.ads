pragma Ada_2012;
pragma Style_Checks (Off);


package dbus_shared_h is

   DBUS_SERVICE_DBUS : aliased constant String := "org.freedesktop.DBus" & ASCII.NUL;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:76

   DBUS_PATH_DBUS : aliased constant String := "/org/freedesktop/DBus" & ASCII.NUL;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:80

   DBUS_PATH_LOCAL : aliased constant String := "/org/freedesktop/DBus/Local" & ASCII.NUL;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:82

   DBUS_INTERFACE_DBUS : aliased constant String := "org.freedesktop.DBus" & ASCII.NUL;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:88

   DBUS_INTERFACE_MONITORING : aliased constant String := "org.freedesktop.DBus.Monitoring" & ASCII.NUL;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:90

   DBUS_INTERFACE_VERBOSE : aliased constant String := "org.freedesktop.DBus.Verbose" & ASCII.NUL;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:93

   DBUS_INTERFACE_INTROSPECTABLE : aliased constant String := "org.freedesktop.DBus.Introspectable" & ASCII.NUL;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:95

   DBUS_INTERFACE_PROPERTIES : aliased constant String := "org.freedesktop.DBus.Properties" & ASCII.NUL;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:97

   DBUS_INTERFACE_PEER : aliased constant String := "org.freedesktop.DBus.Peer" & ASCII.NUL;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:99

   DBUS_INTERFACE_LOCAL : aliased constant String := "org.freedesktop.DBus.Local" & ASCII.NUL;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:105

   DBUS_NAME_FLAG_ALLOW_REPLACEMENT : constant := 16#1#;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:108
   DBUS_NAME_FLAG_REPLACE_EXISTING : constant := 16#2#;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:109
   DBUS_NAME_FLAG_DO_NOT_QUEUE : constant := 16#4#;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:110

   DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER : constant := 1;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:113
   DBUS_REQUEST_NAME_REPLY_IN_QUEUE : constant := 2;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:114
   DBUS_REQUEST_NAME_REPLY_EXISTS : constant := 3;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:115
   DBUS_REQUEST_NAME_REPLY_ALREADY_OWNER : constant := 4;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:116

   DBUS_RELEASE_NAME_REPLY_RELEASED : constant := 1;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:119
   DBUS_RELEASE_NAME_REPLY_NON_EXISTENT : constant := 2;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:120
   DBUS_RELEASE_NAME_REPLY_NOT_OWNER : constant := 3;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:121

   DBUS_START_REPLY_SUCCESS : constant := 1;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:124
   DBUS_START_REPLY_ALREADY_RUNNING : constant := 2;  --  /usr/include/dbus-1.0/dbus/dbus-shared.h:125

   type DBusBusType is 
     (DBUS_BUS_SESSION,
      DBUS_BUS_SYSTEM,
      DBUS_BUS_STARTER)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-shared.h:61

   type DBusHandlerResult is 
     (DBUS_HANDLER_RESULT_HANDLED,
      DBUS_HANDLER_RESULT_NOT_YET_HANDLED,
      DBUS_HANDLER_RESULT_NEED_MEMORY)
   with Convention => C;  -- /usr/include/dbus-1.0/dbus/dbus-shared.h:71

end dbus_shared_h;

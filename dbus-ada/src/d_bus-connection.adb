--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2011-2015  Reto Buerki <reet@codelabs.ch>
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 2
--  of the License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
--  USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit,  or  you  link  this  unit  with  other  files  to  produce  an
--  executable   this  unit  does  not  by  itself  cause  the  resulting
--  executable to  be  covered by the  GNU General  Public License.  This
--  exception does  not  however  invalidate  any  other reasons why  the
--  executable file might be covered by the GNU Public License.
--

with Interfaces.C.Strings;

with dbus_bus_h;
with dbus_shared_h;
with dbus_errors_h;
with dbus_message_h;
with dbus_types_h;
with dbus_arch_deps_h;

package body D_Bus.Connection is

   package C renames Interfaces.C;

   Bus_Types : constant array (Bus_Type) of dbus_shared_h.DBusBusType
     := (Bus_Session => dbus_shared_h.DBUS_BUS_SESSION,
         Bus_System  => dbus_shared_h.DBUS_BUS_SYSTEM,
         Bus_Starter => dbus_shared_h.DBUS_BUS_STARTER);
   --  Mapping of Ada bus type enum to low-level D-Bus bus types.

   procedure Check (Result : access dbus_errors_h.DBusError);
   --  Check D-Bus error object and raise an exception if error is set. The
   --  D-Bus error object is freed before the exception is raised.

   procedure Add_Args
     (D_Message : access dbus_message_h.DBusMessage;
      Args      : Arguments.Argument_List_Type);
   --  Add arguments to low-level D-Bus message.

   -------------------------------------------------------------------------

   procedure Add_Args
     (D_Message : access dbus_message_h.DBusMessage;
      Args      : Arguments.Argument_List_Type)
   is
      D_Args : aliased dbus_message_h.DBusMessageIter;
   begin
      dbus_message_h.dbus_message_iter_init_append
        (message => D_Message,
         iter    => D_Args'Access);

      Arguments.Serialize
        (Args   => Args,
         D_Args => D_Args'Access);
   end Add_Args;

   -------------------------------------------------------------------------

   procedure Add_Match
     (Connection : Connection_Type;
      Rule       : String)
   is
      D_Err : aliased dbus_errors_h.DBusError;
      C_Str : C.Strings.chars_ptr := C.Strings.New_String
        (Str => Rule);
   begin
      dbus_bus_h.dbus_bus_add_match
        (connection => Connection.Thin_Connection,
         rule       => C_Str,
         error      => D_Err'Access);
      C.Strings.Free (Item => C_Str);
      Check (Result => D_Err'Access);
   end Add_Match;

   -------------------------------------------------------------------------

   function Call_Blocking
     (Connection   : Connection_Type;
      Destination  : String;
      Path         : Types.Obj_Path;
      Iface        : String;
      Method       : String;
      Timeout_Msec : Timeout_Type                 := Default_Timeout;
      Args         : Arguments.Argument_List_Type :=
        Arguments.Empty_Argument_List)
      return Arguments.Argument_List_Type
   is
      D_Msg   : access dbus_message_h.DBusMessage := null;
      D_Reply : access dbus_message_h.DBusMessage := null;
      D_Err   : aliased dbus_errors_h.DBusError;

      C_Dest   : C.Strings.chars_ptr;
      C_Path   : C.Strings.chars_ptr;
      C_Iface  : C.Strings.chars_ptr;
      C_Method : C.Strings.chars_ptr;
   begin
      C_Dest   := C.Strings.New_String (Str => Destination);
      C_Path   := C.Strings.New_String (Str => Types.To_String (Path));
      C_Iface  := C.Strings.New_String (Str => Iface);
      C_Method := C.Strings.New_String (Str => Method);

      D_Msg := dbus_message_h.dbus_message_new_method_call
        (bus_name => C_Dest,
         path     => C_Path,
         iface    => C_Iface,
         method   => C_Method);

      C.Strings.Free (C_Dest);
      C.Strings.Free (C_Path);
      C.Strings.Free (C_Iface);
      C.Strings.Free (C_Method);

      if D_Msg = null then
         raise D_Bus_Error with "Could not allocate message";
      end if;

      Add_Args (D_Message => D_Msg,
                Args      => Args);

      D_Reply := dbus_connection_h.dbus_connection_send_with_reply_and_block
        (connection           => Connection.Thin_Connection,
         message              => D_Msg,
         timeout_milliseconds => C.int (Timeout_Msec),
         error                => D_Err'Access);
      dbus_message_h.dbus_message_unref (message => D_Msg);
      D_Msg := null;

      if D_Reply = null then
         Check (Result => D_Err'Access);
      end if;

      declare
         use type dbus_types_h.dbus_bool_t;

         D_Args : aliased dbus_message_h.DBusMessageIter;
      begin
         if dbus_message_h.dbus_message_iter_init
           (message => D_Reply,
            iter    => D_Args'Access) = 0
         then
            dbus_message_h.dbus_message_unref (message => D_Reply);
            return Arguments.Empty_Argument_List;
         end if;

         return A : Arguments.Argument_List_Type do
            A := Arguments.Deserialize (D_Args => D_Args'Access);
            dbus_message_h.dbus_message_unref (message => D_Reply);
         end return;
      end;
   end Call_Blocking;

   -------------------------------------------------------------------------

   procedure Call_No_Reply
     (Connection   : Connection_Type;
      Destination  : String;
      Path         : Types.Obj_Path;
      Iface        : String;
      Method       : String;
      Args         : Arguments.Argument_List_Type :=
        Arguments.Empty_Argument_List)
   is
      use type dbus_types_h.dbus_bool_t;
      D_Msg : access dbus_message_h.DBusMessage := null;
      D_Res : dbus_types_h.dbus_bool_t;

      C_Dest   : C.Strings.chars_ptr;
      C_Path   : C.Strings.chars_ptr;
      C_Iface  : C.Strings.chars_ptr;
      C_Method : C.Strings.chars_ptr;
   begin
      C_Dest   := C.Strings.New_String (Str => Destination);
      C_Path   := C.Strings.New_String (Str => Types.To_String (Path));
      C_Iface  := C.Strings.New_String (Str => Iface);
      C_Method := C.Strings.New_String (Str => Method);

      D_Msg := dbus_message_h.dbus_message_new_method_call
        (bus_name => C_Dest,
         path     => C_Path,
         iface    => C_Iface,
         method   => C_Method);

      C.Strings.Free (C_Dest);
      C.Strings.Free (C_Path);
      C.Strings.Free (C_Iface);
      C.Strings.Free (C_Method);

      if D_Msg = null then
         raise D_Bus_Error with "Could not allocate message";
      end if;

      dbus_message_h.dbus_message_set_no_reply (D_Msg, 1);

      Add_Args (D_Message => D_Msg,
                Args      => Args);

      D_Res := dbus_connection_h.dbus_connection_send
        (connection => Connection.Thin_Connection,
         message => D_Msg,
         client_serial => null);

      dbus_message_h.dbus_message_unref (D_Msg);

      if D_Res /= 1 then
         raise D_Bus_Error with "Could not send message";
      end if;

      dbus_connection_h.dbus_connection_flush (Connection.Thin_Connection);
   end Call_No_Reply;

   -------------------------------------------------------------------------

   procedure Check (Result : access dbus_errors_h.DBusError)
   is
      use type dbus_types_h.dbus_bool_t;
   begin
      if dbus_errors_h.dbus_error_is_set (error => Result) = 1 then
         declare
            Error_Name   : constant String := C.Strings.Value (Result.name);
            Error_String : constant String := C.Strings.Value (Result.message);
         begin
            dbus_errors_h.dbus_error_free (error => Result);
            raise D_Bus_Error with Error_Name & " - " & Error_String;
         end;
      end if;
   end Check;

   -------------------------------------------------------------------------

   function Connect (Bus : Bus_Type := Bus_Session) return Connection_Type
   is
      D_Err : aliased dbus_errors_h.DBusError;
   begin
      return Result : constant Connection_Type
         := (Thin_Connection => dbus_bus_h.dbus_bus_get
               (c_type => Bus_Types (Bus),
                error  => D_Err'Access))
      do
         Check (Result => D_Err'Access);
      end return;
   end Connect;

   -------------------------------------------------------------------------

   function Connect (Address : String) return Connection_Type
   is
      C_Addr : C.Strings.chars_ptr := C.Strings.New_String (Str => Address);
      D_Err  : aliased dbus_errors_h.DBusError;
   begin
      return Result : constant Connection_Type
         := (Thin_Connection => dbus_connection_h.dbus_connection_open
               (address => C_Addr,
                error   => D_Err'Access))
      do
         C.Strings.Free (Item => C_Addr);

         Check (Result => D_Err'Access);

         dbus_connection_h.dbus_connection_set_exit_on_disconnect
           (Result.Thin_Connection, 0);
      end return;
   end Connect;

   -------------------------------------------------------------------------

   function Connect_Private
     (Bus : Bus_Type := Bus_Session) return Connection_Type
   is
      D_Err : aliased dbus_errors_h.DBusError;
   begin
      return Result : Connection_Type
      do
         Result.Thin_Connection := dbus_bus_h.dbus_bus_get_private
           (c_type => Bus_Types (Bus), error => D_Err'Access);

         Check (Result => D_Err'Access);

         dbus_connection_h.dbus_connection_set_exit_on_disconnect
           (Result.Thin_Connection, 0);
      end return;
   end Connect_Private;

   -------------------------------------------------------------------------

   function Connect_Private (Address : String) return Connection_Type
   is
      C_Addr : C.Strings.chars_ptr := C.Strings.New_String (Str => Address);
      D_Err  : aliased dbus_errors_h.DBusError;
   begin
      return Result : constant Connection_Type
         := (Thin_Connection => dbus_connection_h.dbus_connection_open_private
               (address => C_Addr,
                error   => D_Err'Access))
      do
         C.Strings.Free (Item => C_Addr);
         Check (Result => D_Err'Access);
      end return;
   end Connect_Private;

   -------------------------------------------------------------------------

   procedure Disconnect (Connection : in out Connection_Type)
   is
   begin
      dbus_connection_h.dbus_connection_close (Connection.Thin_Connection);
      Unref (Connection);
   end Disconnect;

   -------------------------------------------------------------------------

   procedure Flush (Connection : Connection_Type)
   is
   begin
      dbus_connection_h.dbus_connection_flush
        (connection => Connection.Thin_Connection);
   end Flush;

   -------------------------------------------------------------------------

   procedure Pop_Message
     (Connection :     Connection_Type;
      Message    : out Messages.Message_Type)
   is
      Thin_Msg : access dbus_message_h.DBusMessage;
   begin
      Thin_Msg := dbus_connection_h.dbus_connection_pop_message
        (connection => Connection.Thin_Connection);
      Message := Messages.To_Thick (Thin => Thin_Msg);
   end Pop_Message;

   -------------------------------------------------------------------------

   function Read_Write
     (Connection   : Connection_Type;
      Timeout_Msec : Timeout_Type)
      return Boolean
   is
      use type C.unsigned;
   begin
      return dbus_connection_h.dbus_connection_read_write
        (connection           => Connection.Thin_Connection,
         timeout_milliseconds => C.int (Timeout_Msec)) = 1;
   end Read_Write;

   -------------------------------------------------------------------------

   function Ref (Connection : Connection_Type) return Connection_Type
   is
   begin
      return
        (Thin_Connection =>
            dbus_connection_h.dbus_connection_ref
              (Connection.Thin_Connection));
   end Ref;

   -------------------------------------------------------------------------

   procedure Release_Name
     (Connection : Connection_Type;
      Name       : String)
   is
      use type C.int;

      C_Res : C.int;
      D_Err : aliased dbus_errors_h.DBusError;
      C_Name : C.Strings.chars_ptr := C.Strings.New_String (Name);
   begin
      C_Res := dbus_bus_h.dbus_bus_release_name
        (connection => Connection.Thin_Connection,
         name => C_Name,
         error => D_Err'Access);
      C.Strings.Free (C_Name);

      --  Check and free error if needed
      Check (D_Err'Access);

      if C_Res /= dbus_shared_h.DBUS_RELEASE_NAME_REPLY_RELEASED then
         raise D_Bus_Error
           with "Unable to release name " & Name & " (" & C_Res'Img & ")";
      end if;
   end Release_Name;

   -------------------------------------------------------------------------

   procedure Remove_Match
     (Connection : Connection_Type;
      Rule       : String)
   is
      D_Err : aliased dbus_errors_h.DBusError;
      C_Str : C.Strings.chars_ptr := C.Strings.New_String
        (Str => Rule);
   begin
      dbus_bus_h.dbus_bus_remove_match
        (connection => Connection.Thin_Connection,
         rule       => C_Str,
         error      => D_Err'Access);
      C.Strings.Free (Item => C_Str);
      Check (Result => D_Err'Access);
   end Remove_Match;

   -------------------------------------------------------------------------

   procedure Request_Name
     (Connection : Connection_Type;
      Name       : String)
   is
      use type C.int;

      C_Res  : C.int;
      C_Name : C.Strings.chars_ptr := C.Strings.New_String (Str => Name);
      D_Err  : aliased dbus_errors_h.DBusError;
   begin
      C_Res := dbus_bus_h.dbus_bus_request_name
        (connection => Connection.Thin_Connection,
         name       => C_Name,
         flags      => dbus_shared_h.DBUS_NAME_FLAG_REPLACE_EXISTING,
         error      => D_Err'Access);
      C.Strings.Free (Item => C_Name);

      Check (Result => D_Err'Access);

      if C_Res /= dbus_shared_h.DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER then
         raise D_Bus_Error with "Not primary owner for '"
           & Name & "' (" & C_Res'Img & ")";
      end if;
   end Request_Name;

   -------------------------------------------------------------------------

   procedure Send
     (Connection : Connection_Type;
      Message    : Messages.Message_Type)
   is
      use type dbus_types_h.dbus_bool_t;

      D_Serial : aliased dbus_arch_deps_h.dbus_uint32_t := 0;
   begin
      if dbus_connection_h.dbus_connection_send
        (connection    => Connection.Thin_Connection,
         message       => Messages.To_Thin (Msg => Message),
         client_serial => D_Serial'Access) = 0
      then
         raise D_Bus_Error with "Could not send message: out of memory";
      end if;
   end Send;

   -------------------------------------------------------------------------

   procedure Send_Signal
     (Connection  : Connection_Type;
      Object_Name : Types.Obj_Path;
      Iface       : String;
      Name        : String;
      Args        : Arguments.Argument_List_Type :=
        Arguments.Empty_Argument_List)
   is
      D_Msg : access dbus_message_h.DBusMessage := null;

      C_Object : C.Strings.chars_ptr;
      C_Iface  : C.Strings.chars_ptr;
      C_Name   : C.Strings.chars_ptr;

      ----------------------------------------------------------------------

      procedure Free_Strings;
      --  Free allocated memory.

      procedure Free_Strings
      is
      begin
         C.Strings.Free (Item => C_Object);
         C.Strings.Free (Item => C_Iface);
         C.Strings.Free (Item => C_Name);
      end Free_Strings;

   begin
      C_Object := C.Strings.New_String (Str => Types.To_String (Object_Name));
      C_Iface  := C.Strings.New_String (Str => Iface);
      C_Name   := C.Strings.New_String (Str => Name);

      D_Msg := dbus_message_h.dbus_message_new_signal
        (path  => C_Object,
         iface => C_Iface,
         name  => C_Name);

      if D_Msg = null then
         Free_Strings;
         raise D_Bus_Error with "Could not allocate message";
      end if;

      begin
         Add_Args (D_Message => D_Msg,
                   Args      => Args);

      exception
         when D_Bus_Error =>
            Free_Strings;
            raise;
      end;

      declare
         use type C.unsigned;

         D_Serial : aliased dbus_arch_deps_h.dbus_uint32_t := 0;
      begin
         if dbus_connection_h.dbus_connection_send
           (connection    => Connection.Thin_Connection,
            message       => D_Msg,
            client_serial => D_Serial'Access) = 0
         then
            Free_Strings;
            raise D_Bus_Error with "Could not send signal";
         end if;
      end;

      Free_Strings;
      dbus_message_h.dbus_message_unref (message => D_Msg);
   end Send_Signal;

   -------------------------------------------------------------------------

   procedure Unref (Connection : in out Connection_Type) is
   begin
      dbus_connection_h.dbus_connection_unref (Connection.Thin_Connection);
      Connection := Null_Connection;
   end Unref;
end D_Bus.Connection;

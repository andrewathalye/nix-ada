--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2011-2019  Reto Buerki <reet@codelabs.ch>
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

with System;

with Interfaces.C;

with dbus_shared_h;
with dbus_message_h;
with dbus_types_h;

procedure D_Bus.Connection.Dispatch
  (Connection : Connection_Type;
   Callback   : Callbacks.Message_Callback)
is
   use type Interfaces.C.int;
   use type dbus_types_h.dbus_bool_t;

   function Call_Back
     (D_Conn   : access dbus_connection_h.DBusConnection;
      Msg      : access dbus_message_h.DBusMessage;
      Usr_Data : System.Address)
         return dbus_shared_h.DBusHandlerResult;
   pragma Convention (C, Call_Back);
   --  Dispatch deserialized message to given callback procedure.

   function Call_Back
     (D_Conn   : access dbus_connection_h.DBusConnection;
      Msg      : access dbus_message_h.DBusMessage;
      Usr_Data : System.Address)
         return dbus_shared_h.DBusHandlerResult
   is
      pragma Unreferenced (D_Conn, Usr_Data);
   begin
      Callback (Msg => Messages.Create (D_Msg => Msg));

      return dbus_shared_h.DBUS_HANDLER_RESULT_HANDLED;
   end Call_Back;

   procedure Free_Usr_Data (arg1 : System.Address) is null;
   pragma Convention (C, Free_Usr_Data);

   D_Res : dbus_types_h.dbus_bool_t;
begin
   D_Res := dbus_connection_h.dbus_connection_add_filter
     (connection         => Connection.Thin_Connection,
      c_function         => Call_Back'Unrestricted_Access,
      user_data          => System.Null_Address,
      free_data_function => Free_Usr_Data'Unrestricted_Access);

   if D_Res = 0 then
      raise D_Bus_Error with "Could not add connection filter";
   end if;

   while dbus_connection_h.dbus_connection_read_write_dispatch
     (connection           => Connection.Thin_Connection,
      timeout_milliseconds => -1) = 1
   loop
      null;
   end loop;
end D_Bus.Connection.Dispatch;

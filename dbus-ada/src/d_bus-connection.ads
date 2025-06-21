--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2011-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2024 Andrew Athalye
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

with dbus_connection_h;
private with dbus_pending_call_h;

with D_Bus.Arguments;
with D_Bus.Messages;
with D_Bus.Types;

package D_Bus.Connection is
   type Connection_Type is private;
   --  D-Bus connection.
   --  Creating a connection of any type allocates data,
   --  which must be freed by calling `Free`.

   Null_Connection : constant Connection_Type;

   -------------
   -- CONNECT --
   -------------
   --  These subprograms additionally and implicitly `Ref` the
   --  Connection_Type which they return. The connection must be
   --  `Unref`’d to avoid a memory leak.

   function Connect (Bus : Bus_Type := Bus_Session) return Connection_Type;
   --  Connect to the given message bus type and return
   --  a shared connection. This is not thread-safe.

   function Connect (Address : String) return Connection_Type;
   --  Connect to the given remote address and return
   --  a shared connection. This is not thread-safe.

   function Connect_Private
     (Bus : Bus_Type := Bus_Session) return Connection_Type;
   --  Connect to the given message bus type and return
   --  a unique connection.

   function Connect_Private (Address : String) return Connection_Type;
   --  Connect to the given remote address and return
   --  a unique connection.

   procedure Disconnect (Connection : in out Connection_Type);
   --  Disconnect from a unique connection. The connection will
   --  be invalid after this and its data will be freed unless
   --  `Connection` was `Ref`’d by another consumer.

   ----------------
   -- REFERENCES --
   ----------------
   --  Reference management is thread-safe, but pay careful attention
   --  to the description of each and every subprogram.

   function Ref (Connection : Connection_Type) return Connection_Type;
   --  Add a reference to the count associated with `Connection` and
   --  return `Connection` for convenience.
   --
   --  This should be called if a consumer wishes to guarantee that
   --  they hold a valid `Connection_Type`.

   procedure Unref (Connection : in out Connection_Type);
   --  Remove a reference from the count associated with `Connection`
   --  `Connection` will then be made invalid so it cannot be used.
   --
   --  The underlying data will be freed automatically when there
   --  are no more references to `Connection`.
   --
   --  Never call `Unref` more times than you called `Ref` or `Connect*`

   ---------------------
   -- MESSAGE PASSING --
   ---------------------
   subtype Timeout_Type is Integer range -1 .. Integer'Last;

   Default_Timeout : constant Timeout_Type;

   function Call_Blocking
     (Connection   : Connection_Type;
      Destination  : String;
      Path         : Types.Obj_Path;
      Iface        : String;
      Method       : String;
      Timeout_Msec : Timeout_Type                 := Default_Timeout;
      Args         : Arguments.Argument_List_Type :=
        Arguments.Empty_Argument_List)
      return Arguments.Argument_List_Type;
   --  Synchronously call the given method.

   procedure Call_No_Reply
     (Connection   : Connection_Type;
      Destination  : String;
      Path         : Types.Obj_Path;
      Iface        : String;
      Method       : String;
      Args         : Arguments.Argument_List_Type :=
        Arguments.Empty_Argument_List);
   --  Call the given method but do not wait for or allow a reply.

   procedure Send_Signal
     (Connection  : Connection_Type;
      Object_Name : Types.Obj_Path;
      Iface       : String;
      Name        : String;
      Args        : Arguments.Argument_List_Type :=
        Arguments.Empty_Argument_List);
   --  Send a signal over the given connection.

   procedure Send
     (Connection : Connection_Type;
      Message    : Messages.Message_Type);
   --  Add given message to the outgoing message queue.

   procedure Request_Name
     (Connection : Connection_Type;
      Name       : String);
   --  Request name on the bus for given connection.

   procedure Release_Name
     (Connection : Connection_Type;
      Name       : String);
   --  Release name on the bus for given connection.

   procedure Add_Match
     (Connection : Connection_Type;
      Rule       : String);
   --  Add given match rule to match messages going through the message bus.

   procedure Remove_Match
     (Connection : Connection_Type;
      Rule       : String);
   --  Remove given match rule on messages going through the message bus.

   function Read_Write
     (Connection   : Connection_Type;
      Timeout_Msec : Timeout_Type)
      return Boolean;
   --  The return value indicates whether reading or writing is still possible
   --  for the specified connection, i.e. whether the connection is connected.

   procedure Pop_Message
     (Connection :     Connection_Type;
      Message    : out Messages.Message_Type);
   --  Returns the first-received message from the incoming message queue,
   --  removing it from the queue. The caller owns a reference to the returned
   --  message. If the queue is empty, Null_Message is returned.

   procedure Flush (Connection : Connection_Type);
   --  Send any pending messages.

private

   Default_Timeout : constant Timeout_Type
     := dbus_pending_call_h.DBUS_TIMEOUT_USE_DEFAULT;

   type Connection_Type is record
      Thin_Connection : access dbus_connection_h.DBusConnection := null;
   end record;

   Null_Connection : constant Connection_Type := (Thin_Connection => null);

end D_Bus.Connection;

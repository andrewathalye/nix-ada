--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2019  Tobias Brunner <tbrunner@hsr.ch>
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
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

private with Ada.Strings.Unbounded;

with D_Bus.Connection;
with D_Bus.Messages;

package D_Bus.Message_Dispatcher
is

   type Dispatcher_Type is abstract tagged limited private;

   --  Create new message dispatcher.
   --  The introspection data is handed out to clients if the 'Introspect'
   --  method is called on the 'org.freedesktop.DBus.Introspectable' interface.
   function Create (Introspect : String) return Dispatcher_Type is abstract;

   --  Handle method call or signal designated by In_Msg. Returns True if a
   --  handler is registered for the requested method call or signal, False if
   --  not.
   procedure Handle_Message
     (Dispatcher :     Dispatcher_Type;
      In_Msg     :     D_Bus.Messages.Message_Type;
      Signal     :     Boolean;
      Success    : out Boolean) is abstract;

   --  Peek for a new message to dispatch. Use a timeout of -1 to wait an
   --  infinite amount of time for a new message to appear.
   --  Success is False if the internal bus connection received the disconnect
   --  messsage. If no handler is registered for a given
   --  interface/method/signal or the Introspect method is called, an
   --  appropriate (error) message is enqued in the Messagebox package for
   --  further processing.
   procedure Peek
     (Dispatcher   :     Dispatcher_Type'Class;
      Conn         :     Connection.Connection_Type;
      Timeout_Msec :     Integer := 100;
      Success      : out Boolean);

private

   type Dispatcher_Type is abstract tagged limited record
      Introspect : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Base_Registration_Type is tagged record
      Iface  : Ada.Strings.Unbounded.Unbounded_String;
      Method : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Msg_Dst_Check_Func is  access function
     (Msg    : Messages.Message_Type;
      Iface  : String;
      Method : String) return Boolean;

end D_Bus.Message_Dispatcher;

--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2011  Reto Buerki <reet@codelabs.ch>
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

with dbus_message_h;
with D_Bus.Arguments;

package D_Bus.Messages is

   type Message_Variant is
     (Invalid,
      Method_Call,
      Method_Return,
      Error,
      Signal);
   --  D-Bus message types.

   type Message_Type is private;
   --  A D-Bus message, see the section 'Message Format' in the D-Bus
   --  specification.

   Null_Message : constant Message_Type;

   function New_Method_Return (Method_Call : Message_Type) return Message_Type;
   --  Constructs a message that is a reply to a method call.

   function New_Error
     (Reply_To      : Message_Type;
      Error_Name    : String;
      Error_Message : String)
      return Message_Type;
   --  Creates a new message that is an error reply to another message.

   function Create
     (D_Msg : access dbus_message_h.DBusMessage)
      return Message_Type;
   --  Create message object from low-level D-Bus message.

   function Get_Type (Msg : Message_Type) return Message_Variant;
   --  Return the message's type.

   function Get_Serial (Msg : Message_Type) return Positive;
   --  Return the message's serial number.

   function Get_Sender (Msg : Message_Type) return String;
   --  Return the message's sender.

   function Get_Destination (Msg : Message_Type) return String;
   --  Return the message's destination.

   function Get_Path (Msg : Message_Type) return String;
   --  Return the path.

   function Get_Interface (Msg : Message_Type) return String;
   --  Return the called interface.

   function Get_Member (Msg : Message_Type) return String;
   --  Return the member; either the method name or signal name.

   function Get_Arguments
     (Msg : Message_Type)
      return Arguments.Argument_List_Type;
   --  Return the message's arguments (if any).

   procedure Add_Arguments
     (Msg  : in out Message_Type;
      Args :        Arguments.Argument_List_Type);
   --  Add given arguments to message.

   function Is_Method_Call
     (Msg    : Message_Type;
      Iface  : String;
      Method : String)
      return Boolean;
   --  Checks whether the given message is a method call with specified
   --  interface and member fields.

   function Is_Signal
     (Msg         : Message_Type;
      Iface       : String;
      Signal_Name : String)
      return Boolean;
   --  Checks whether the given message is a signal with specified interface
   --  and name.

   function Is_No_Reply_Expected (Msg : Message_Type) return Boolean;
   --  Check whether the message doesn't expect method return or error
   --  replies. If set, replies should be omitted.

   function Is_Null (Msg : Message_Type) return Boolean;
   --  Returns True if given message is nil.

   function Ref (Msg : Message_Type) return Message_Type;
   --  Increment the reference count of the given message and return copy.

   procedure Unref (Msg : in out Message_Type);
   --  Unreference given message. The message is freed if the reference count
   --  reaches zero.

   function To_Thin
     (Msg : Message_Type)
      return access dbus_message_h.DBusMessage;
   --  Helper function to convert the Ada D-Bus message to a thin binding
   --  memory address.

   function To_Thick
     (Thin : access dbus_message_h.DBusMessage)
      return Message_Type;
   --  Helper function to convert thin binding message to Ada D-Bus message.

private

   type Message_Type is record
      Thin_Msg : access dbus_message_h.DBusMessage := null;
   end record;

   Null_Message : constant Message_Type := (Thin_Msg => null);

   function To_Thick
     (Thin : access dbus_message_h.DBusMessage)
      return Message_Type
   is (Thin_Msg => Thin);

   function Is_Null (Msg : Message_Type) return Boolean
   is (Msg = Null_Message);

end D_Bus.Messages;

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

with Interfaces.C.Strings;

with dbus_types_h;

package body D_Bus.Messages is

   use dbus_message_h;
   use type Interfaces.C.Strings.chars_ptr;

   package C renames Interfaces.C;

   function Value_Or_Empty (Ptr : C.Strings.chars_ptr) return String;
   --  Returns empty string if the given pointer is a null pointer, if not the
   --  function returns the corresponding string value.

   -------------------------------------------------------------------------

   procedure Add_Arguments
     (Msg  : in out Message_Type;
      Args :        Arguments.Argument_List_Type)
   is
      D_Args : aliased DBusMessageIter;
   begin
      dbus_message_iter_init_append
        (message => Msg.Thin_Msg,
         iter    => D_Args'Access);
      Arguments.Serialize (Args   => Args,
                           D_Args => D_Args'Access);
   end Add_Arguments;

   -------------------------------------------------------------------------

   function Create
     (D_Msg : access dbus_message_h.DBusMessage)
      return Message_Type
   is
   begin
      return M : Message_Type do
         M.Thin_Msg := D_Msg;
      end return;
   end Create;

   -------------------------------------------------------------------------

   function Get_Arguments
     (Msg : Message_Type)
      return Arguments.Argument_List_Type
   is
      use type dbus_types_h.dbus_bool_t;

      D_Args : aliased dbus_message_h.DBusMessageIter;
      Args   : Arguments.Argument_List_Type;
   begin
      if dbus_message_h.dbus_message_iter_init
        (message => Msg.Thin_Msg,
         iter    => D_Args'Access) = 1
      then
         Args := Arguments.Deserialize (D_Args'Access);
      end if;

      return Args;
   end Get_Arguments;

   -------------------------------------------------------------------------

   function Get_Destination (Msg : Message_Type) return String
   is
      C_Dest : constant C.Strings.chars_ptr := dbus_message_get_destination
        (message => Msg.Thin_Msg);
   begin
      return Value_Or_Empty (Ptr => C_Dest);
   end Get_Destination;

   -------------------------------------------------------------------------

   function Get_Interface (Msg : Message_Type) return String
   is
      C_Iface : constant C.Strings.chars_ptr := dbus_message_get_interface
        (message => Msg.Thin_Msg);
   begin
      return Value_Or_Empty (Ptr => C_Iface);
   end Get_Interface;

   -------------------------------------------------------------------------

   function Get_Member (Msg : Message_Type) return String
   is
      C_Member : constant C.Strings.chars_ptr := dbus_message_get_member
        (message => Msg.Thin_Msg);
   begin
      return Value_Or_Empty (Ptr => C_Member);
   end Get_Member;

   -------------------------------------------------------------------------

   function Get_Path (Msg : Message_Type) return String
   is
      C_Path : constant C.Strings.chars_ptr := dbus_message_get_path
        (message => Msg.Thin_Msg);
   begin
      return Value_Or_Empty (Ptr => C_Path);
   end Get_Path;

   -------------------------------------------------------------------------

   function Get_Sender (Msg : Message_Type) return String
   is
      C_Sender : constant C.Strings.chars_ptr := dbus_message_get_sender
        (message => Msg.Thin_Msg);
   begin
      return Value_Or_Empty (Ptr => C_Sender);
   end Get_Sender;

   ------------------------------------------------------------------------

   function Get_Serial (Msg : Message_Type) return Positive
   is
   begin
      return Positive (dbus_message_get_serial (message => Msg.Thin_Msg));

   exception
      when Constraint_Error =>
         raise D_Bus_Error with "Message has an invalid serial number";
   end Get_Serial;

   -------------------------------------------------------------------------

   function Get_Type (Msg : Message_Type) return Message_Variant
   is
   begin
      return Message_Variant'Val
        (dbus_message_get_type (message => Msg.Thin_Msg));

   exception
      when Constraint_Error =>
         raise D_Bus_Error with "Message has an invalid type";
   end Get_Type;

   -------------------------------------------------------------------------

   function Is_Method_Call
     (Msg    : Message_Type;
      Iface  : String;
      Method : String)
      return Boolean
   is
      use type C.unsigned;

      C_Iface  : C.Strings.chars_ptr := C.Strings.New_String
        (Str => Iface);
      C_Member : C.Strings.chars_ptr := C.Strings.New_String
        (Str => Method);
      Res      : C.unsigned;
   begin
      Res := dbus_message_is_method_call
        (message => Msg.Thin_Msg,
         iface   => C_Iface,
         method  => C_Member);
      C.Strings.Free (Item => C_Iface);
      C.Strings.Free (Item => C_Member);
      return Res = 1;
   end Is_Method_Call;

   -------------------------------------------------------------------------

   function Is_No_Reply_Expected (Msg : Message_Type) return Boolean
   is
      use type C.unsigned;
   begin
      return dbus_message_get_no_reply (message => Msg.Thin_Msg) = 1;

   exception
      when Constraint_Error =>
         raise D_Bus_Error with "Message has an invalid type";
   end Is_No_Reply_Expected;

   -------------------------------------------------------------------------

   function Is_Signal
     (Msg         : Message_Type;
      Iface       : String;
      Signal_Name : String)
      return Boolean
   is
      use type C.unsigned;

      C_Iface  : C.Strings.chars_ptr := C.Strings.New_String
        (Str => Iface);
      C_Signal : C.Strings.chars_ptr := C.Strings.New_String
        (Str => Signal_Name);
      Res      : C.unsigned;
   begin
      Res := dbus_message_is_signal
        (message     => Msg.Thin_Msg,
         iface       => C_Iface,
         signal_name => C_Signal);
      C.Strings.Free (Item => C_Iface);
      C.Strings.Free (Item => C_Signal);
      return Res = 1;
   end Is_Signal;

   -------------------------------------------------------------------------

   function New_Error
     (Reply_To      : Message_Type;
      Error_Name    : String;
      Error_Message : String)
      return Message_Type
   is
      Error  : Message_Type;
      C_Name : C.Strings.chars_ptr
        := C.Strings.New_String (Str => Error_Name);
      C_Msg  : C.Strings.chars_ptr
        := C.Strings.New_String (Str => Error_Message);
   begin
      Error.Thin_Msg := dbus_message_new_error
        (reply_to      => Reply_To.Thin_Msg,
         error_name    => C_Name,
         error_message => C_Msg);
      C.Strings.Free (Item => C_Name);
      C.Strings.Free (Item => C_Msg);

      if Error.Thin_Msg = null then
         raise D_Bus_Error with "Could not create error reply";
      end if;

      return Error;
   end New_Error;

   -------------------------------------------------------------------------

   function New_Method_Return (Method_Call : Message_Type) return Message_Type
   is
      Reply : Message_Type;
   begin
      Reply.Thin_Msg := dbus_message_new_method_return
        (method_call => Method_Call.Thin_Msg);

      if Reply.Thin_Msg = null then
         raise D_Bus_Error with "Could not create method reply message";
      end if;

      return Reply;
   end New_Method_Return;

   -------------------------------------------------------------------------

   function Ref (Msg : Message_Type) return Message_Type
   is
   begin
      return Message_Type'
        (Thin_Msg => dbus_message_h.dbus_message_ref
           (message => Msg.Thin_Msg));
   end Ref;

   -------------------------------------------------------------------------

   function To_Thin
     (Msg : Message_Type)
      return access dbus_message_h.DBusMessage
   is
   begin
      return Msg.Thin_Msg;
   end To_Thin;

   -------------------------------------------------------------------------

   procedure Unref (Msg : in out Message_Type)
   is
   begin
      dbus_message_unref (message => Msg.Thin_Msg);
   end Unref;

   -------------------------------------------------------------------------

   function Value_Or_Empty (Ptr : C.Strings.chars_ptr) return String
   is
   begin
      if Ptr = C.Strings.Null_Ptr then
         return "";
      end if;

      return C.Strings.Value (Item => Ptr);
   end Value_Or_Empty;

end D_Bus.Messages;

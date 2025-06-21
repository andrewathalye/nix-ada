--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2011, 2012  Reto Buerki <reet@codelabs.ch>
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
with System;

with dbus_types_h;
with dbus_connection_h;
with dbus_message_h;
with dbus_shared_h;

with D_Bus.Messages;

package body D_Bus.Connection.G_Main is

   package C renames Interfaces.C;

   procedure dbus_connection_setup_with_g_main
     (connection : access dbus_connection_h.DBusConnection;
      context    : D_Bus.G_Main.Main_Context);
   pragma Import (C, dbus_connection_setup_with_g_main,
                  "dbus_connection_setup_with_g_main");

   function Object_Path_Message
     (conn : access dbus_connection_h.DBusConnection;
      msg  : access dbus_message_h.DBusMessage;
      data : System.Address)
      return dbus_shared_h.DBusHandlerResult;
   pragma Convention (C, Object_Path_Message);
   --  Handler for path messages.

   procedure Object_Path_Unregister
     (conn : access dbus_connection_h.DBusConnection;
      data : System.Address) is null;
   pragma Convention (C, Object_Path_Unregister);

   procedure Nul_Pad (arg1 : System.Address) is null;
   pragma Convention (C, Nul_Pad);

   Obj_Path_Table : aliased constant dbus_connection_h.DBusObjectPathVTable
     := (unregister_function => Object_Path_Unregister'Access,
         message_function    => Object_Path_Message'Access,
         dbus_internal_pad1  => Nul_Pad'Access,
         dbus_internal_pad2  => Nul_Pad'Access,
         dbus_internal_pad3  => Nul_Pad'Access,
         dbus_internal_pad4  => Nul_Pad'Access);
   --  Object V path table.

   -------------------------------------------------------------------------

   function Object_Path_Message
     (conn : access dbus_connection_h.DBusConnection;
      msg  : access dbus_message_h.DBusMessage;
      data : System.Address)
      return dbus_shared_h.DBusHandlerResult
   is
      pragma Unreferenced (data);

      use D_Bus.Messages;

      Reply   : Message_Type;
      Message : constant Message_Type := Create (D_Msg => msg);
      Path    : constant String       := Get_Path (Msg => Message);
      Method  : constant String       := Get_Member (Msg => Message);
      Obj     : constant Object'Class := Services.Element
        (Key => To_Unbounded_String (Path));
   begin
      Obj.Call (Name    => Method,
                Request => Message,
                Reply   => Reply);
      Send (Connection => (Thin_Connection => conn),
            Message    => Reply);

      return dbus_shared_h.DBUS_HANDLER_RESULT_HANDLED;

   exception
      when Unknown_Method =>
         declare
            Error  : constant Message_Type := New_Error
              (Reply_To      => Message,
               Error_Name    => "org.freedesktop.DBus.Error.UnknownMethod",
               Error_Message => "Received unknown method call: " & Method);
         begin
            Send (Connection => (Thin_Connection => conn),
                  Message    => Error);
            return dbus_shared_h.DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
         end;
   end Object_Path_Message;

   -------------------------------------------------------------------------

   procedure Register_Object
     (Connection :        Connection_Type;
      Path       :        Types.Obj_Path := +"/";
      Object     : in out Service.Object'Class)
   is
      use type dbus_types_h.dbus_bool_t;

      Str_Path : constant String     := Types.To_String (Path => Path);
      C_Path   : C.Strings.chars_ptr := C.Strings.New_String (Str => Str_Path);
      D_Res    : dbus_types_h.dbus_bool_t;
   begin
      D_Res := dbus_connection_h.dbus_connection_register_object_path
        (connection => Connection.Thin_Connection,
         path       => C_Path,
         vtable     => Obj_Path_Table'Access,
         user_data  => System.Null_Address);
      C.Strings.Free (Item => C_Path);

      if D_Res /= 1 then
         raise D_Bus_Error with "Could not register object on path '"
           & Str_Path & "'";
      end if;

      Object.Initialize;
      Services.Insert (Key      => To_Unbounded_String (Source => Str_Path),
                       New_Item => Object);
   end Register_Object;

   -------------------------------------------------------------------------

   procedure Setup_With_G_Main
     (Connection : Connection_Type;
      Context : D_Bus.G_Main.Main_Context := D_Bus.G_Main.Default_Context)
   is
   begin
      dbus_connection_setup_with_g_main
        (connection => Connection.Thin_Connection,
         context    => Context);
   end Setup_With_G_Main;

end D_Bus.Connection.G_Main;

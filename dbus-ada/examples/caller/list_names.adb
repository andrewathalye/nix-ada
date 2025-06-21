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

with Ada.Text_IO;

with D_Bus.Types;
with D_Bus.Arguments.Basic;
with D_Bus.Arguments.Containers;
with D_Bus.Connection;

pragma Unreferenced (D_Bus.Arguments.Basic);
pragma Unreferenced (D_Bus.Arguments.Containers);

procedure List_Names
is
   use D_Bus;
   use type D_Bus.Types.Obj_Path;

   --  Connect to the D-Bus session bus

   Conn   : constant Connection.Connection_Type := Connection.Connect;
   Result : Arguments.Argument_List_Type;

   procedure Print (Argument : Arguments.Argument_Type'Class);
   --  Print out an argument.

   procedure Print (Argument : Arguments.Argument_Type'Class)
   is
   begin
      Ada.Text_IO.Set_Col (To => 1);
      Ada.Text_IO.Put ("(" & Argument.Get_Code'Img & " )");
      Ada.Text_IO.Set_Col (To => 10);
      Ada.Text_IO.Put_Line (Argument.To_String);
   end Print;

begin

   --  Request a name on the bus

   Connection.Request_Name
     (Connection => Conn,
      Name       => "dbus.ada.caller");

   --  Call a method on a remote object

   Result := Connection.Call_Blocking
     (Connection  => Conn,
      Destination => "org.freedesktop.DBus",
      Path        => +"/",
      Iface       => "org.freedesktop.DBus",
      Method      => "ListNames");
   Ada.Text_IO.Put_Line ("Method called successfully:");

   Arguments.Iterate (List    => Result,
                      Process => Print'Access);
end List_Names;

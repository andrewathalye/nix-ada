--
--  D_Bus/Ada - Example program which does desktop notification on Linux
--
--  Copyright (C) 2011, 2012  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2011        Tero Koskinen <tero.koskinen@iki.fi>
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

procedure Notify
is
   use D_Bus;
   use D_Bus.Arguments.Basic;
   use D_Bus.Arguments.Containers;
   use type D_Bus.Types.Obj_Path;

   --  Connect to the D-Bus session bus

   Conn      : constant Connection.Connection_Type := Connection.Connect;
   Result    : Arguments.Argument_List_Type;
   Nil_Array : Arguments.Containers.Array_Type;
begin

   --  Workaround for 'cannot serialize empty array' problem

   Nil_Array.Append (+"");

   --  Request a name on the bus

   Connection.Request_Name
     (Connection => Conn,
      Name       => "notify.example");

   --  Call a method on a remote object

   Result := Connection.Call_Blocking
     (Connection  => Conn,
      Destination => "org.freedesktop.Notifications",
      Path        => +"/org/freedesktop/Notifications",
      Iface       => "org.freedesktop.Notifications",
      Method      => "Notify",
      Args        => +"ada.notify" & Unsigned_32'(0) & "" & "Hello"
      & "Hello, World from Ada!" & Nil_Array & Nil_Array & Signed_32'(-1));

   Ada.Text_IO.Put ("Notify successful with ID ");
   Ada.Text_IO.Put_Line (U_Int32_Type (Result.First_Element).To_String);
end Notify;

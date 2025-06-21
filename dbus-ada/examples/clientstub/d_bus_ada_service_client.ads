--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2011-2014  Reto Buerki <reet@codelabs.ch>
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

with D_Bus.Types;

package D_Bus_Ada_Service_Client is

   function Get_Name return String;

private

   use type D_Bus.Types.Obj_Path;

   Service : constant String               := "dbus.ada.service";
   Iface   : constant String               := "dbus.ada.service";
   Path    : constant D_Bus.Types.Obj_Path := +"/org/test/object";

end D_Bus_Ada_Service_Client;

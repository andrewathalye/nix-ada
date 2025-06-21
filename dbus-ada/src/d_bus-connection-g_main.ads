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

with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Maps;

with D_Bus.Service;
with D_Bus.G_Main;

package D_Bus.Connection.G_Main is

   use type D_Bus.Types.Obj_Path;

   procedure Setup_With_G_Main
     (Connection : Connection_Type;
      Context : D_Bus.G_Main.Main_Context := D_Bus.G_Main.Default_Context);
   --  Integrate the given connection with `Context`.
   --  Only one context may be associated with a given connection.

   procedure Register_Object
     (Connection :        Connection_Type;
      Path       :        Types.Obj_Path := +"/";
      Object     : in out Service.Object'Class);
   --  Register given service object on specified path. This procedure also
   --  takes care about object initialization.

private

   use Ada.Strings.Unbounded;
   use D_Bus.Service;

   package Service_Obj_Map_Package is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Unbounded_String,
        Element_Type => Object'Class);

   package SOMP renames Service_Obj_Map_Package;

   Services : SOMP.Map;
   --  All registered service objects.

end D_Bus.Connection.G_Main;

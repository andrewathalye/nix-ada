--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2012  Reto Buerki <reet@codelabs.ch>
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

with Ahven;

with D_Bus.Types;

package body Types_Tests is

   use Ahven;
   use D_Bus;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Types");
      T.Add_Test_Routine
        (Routine => Validate_Paths'Access,
         Name    => "Validate object paths");
      T.Add_Test_Routine
        (Routine => Path_To_String'Access,
         Name    => "Convert path to string");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Path_To_String
   is
      use type Types.Obj_Path;

      Str  : constant String         := "/com/example/MusicPlayer1";
      Path : constant Types.Obj_Path := +Str;
   begin
      Assert (Condition => Types.To_String (Path => Path) = Str,
              Message   => "String mismatch");
   end Path_To_String;

   -------------------------------------------------------------------------

   procedure Validate_Paths
   is
   begin
      Assert (Condition => Types.Is_Valid (Path => "/"),
              Message   => "Root not valid");
      Assert (Condition => not Types.Is_Valid (Path => "/obj/"),
              Message   => "Trailing slash valid");
      Assert (Condition => not Types.Is_Valid (Path => "/obj//obj2"),
              Message   => "Multiple slashes valid");
      Assert (Condition => not Types.Is_Valid (Path => "/obj/o*"),
              Message   => "Invalid char not detected");

      declare
         use type D_Bus.Types.Obj_Path;

         Path : Types.Obj_Path;
         pragma Unreferenced (Path);
      begin
         Path := +"not a path";
         Fail (Message => "Exception expected");

      exception
         when D_Bus_Error => null;
      end;
   end Validate_Paths;

end Types_Tests;

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

with Ada.Exceptions;

with Ahven;

with D_Bus.Arguments.Basic;

package body Arguments_Tests is

   use Ahven;
   use D_Bus;
   use D_Bus.Arguments;

   -------------------------------------------------------------------------

   procedure Get_Elements
   is
      use D_Bus.Arguments.Basic;

      L : Argument_List_Type;
   begin
      L.Append (New_Item => +"first");
      L.Append (New_Item => +"second");
      L.Append (New_Item => +"third");

      Assert (Condition => String_Type (L.Get_Element (Pos => 1)) = +"first",
              Message   => "First mismatch");
      Assert (Condition => String_Type (L.Get_Element (Pos => 3)) = +"third",
              Message   => "Third mismatch");
      Assert (Condition => String_Type (L.Get_Element (Pos => 2)) = +"second",
              Message   => "Second mismatch");

      begin
         declare
            Dummy : constant Argument_Type'Class
              := String_Type (L.Get_Element (Pos => 4));
         begin
            Fail (Message => "Exception expected");
         end;

      exception
         when E : Arguments_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "No element at position 4",
                    Message   => "Exception mismatch");
      end;
   end Get_Elements;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Arguments handling");
      T.Add_Test_Routine
        (Routine => List_Append'Access,
         Name    => "Append arguments to list");
      T.Add_Test_Routine
        (Routine => Get_Elements'Access,
         Name    => "Get elements from list");
   end Initialize;

   -------------------------------------------------------------------------

   procedure List_Append
   is
      L : Argument_List_Type;
      B : Basic.String_Type;
   begin
      Assert (Condition => L.Is_Empty,
              Message   => "New list not empty1");
      Assert (Condition => L.Get_Count = 0,
              Message   => "New list not empty2");

      L.Append (New_Item => B);
      L.Append (New_Item => B);
      Assert (Condition => L.Get_Count = 2,
              Message   => "Count not 2");
      Assert (Condition => not L.Is_Empty,
              Message   => "List is empty");
   end List_Append;

end Arguments_Tests;

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

with Ahven.Framework;

package Arguments_Container_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Testcase);
   --  Initialize testcase.

   procedure Append_To_Array;
   --  Verify array append operation.

   procedure Marshal_Array_Of_Strings;
   --  Verify serialization/deserialization of array of strings.

   procedure Marshal_Array_Of_Arrays_Of_Strings;
   --  Verify serialization/deserialization of an array of arrays of strings.

   procedure Marshal_Struct;
   --  Verify serialization/deserialization of struct.

   procedure Marshal_Dict_Entries;
   --  Verify serialization/deserialization of dict entries.

   procedure Marshal_Dict_Entries_With_Variant_Value;
   --  Verify serialization/deserialization of dict entries with variant
   --  values.

   procedure Marshal_Variant;
   --  Verify serialization/deserialization of variant.

end Arguments_Container_Tests;

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

with Ahven.Framework;

package Arguments_Basic_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Testcase);
   --  Initialize testcase.

   procedure Marshal_String_Type;
   --  Verify serialization/deserialization of string type.

   procedure Marshal_Object_Path_Type;
   --  Verify serialization/deserialization of object path type.

   procedure Marshal_Boolean_Type;
   --  Verify serialization/deserialization of boolean type.

   procedure Marshal_U_Int64_Type;
   --  Verify serialization/deserialization of U_Int64 type.

   procedure Marshal_Int64_Type;
   --  Verify serialization/deserialization of Int64 type.

   procedure Marshal_U_Int32_Type;
   --  Verify serialization/deserialization of U_Int32 type.

   procedure Marshal_Int32_Type;
   --  Verify serialization/deserialization of Int32 type.

   procedure Marshal_U_Int16_Type;
   --  Verify serialization/deserialization of U_Int16 type.

   procedure Marshal_Int16_Type;
   --  Verify serialization/deserialization of Int16 type.

   procedure Marshal_Byte_Type;
   --  Verify serialization/deserialization of byte type.

end Arguments_Basic_Tests;

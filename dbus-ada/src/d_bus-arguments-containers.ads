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

with Ada.Strings.Unbounded;

package D_Bus.Arguments.Containers is

   -----------
   -- ARRAY --
   -----------

   type Array_Type is new Argument_List_Type and Argument_Type with private;
   --  D-Bus array container.

   for Array_Type'External_Tag use "a";

   overriding
   procedure Serialize
     (Args   : Array_Type;
      D_Args : not null access dbus_message_h.DBusMessageIter);
   --  Serialize array to D-Bus arguments.

   overriding
   function Deserialize
     (D_Args : not null access dbus_message_h.DBusMessageIter)
      return Array_Type;
   --  Create new array from low-level D-Bus message argument.

   overriding
   function Get_Signature (Arg : Array_Type) return String;
   --  Return the array argument's signature.

   overriding
   function To_String (Arg : Array_Type) return String;
   --  Return string representation of array elements.

   overriding
   procedure Append
     (List     : in out Array_Type;
      New_Item :        Argument_Type'Class);
   --  Append argument to array.

   function "+" (Left : Array_Type) return Argument_List_Type;
   --  Create new argument list and add given array to it.

   function "&"
     (Left  : Argument_List_Type;
      Right : Array_Type)
      return Argument_List_Type;
   --  Add array to argument list.

   procedure Set_Signature
     (List : in out Array_Type;
      Signature : String);
   --  Sets an empty array's signature. It is an error to call this
   --  on a list with elements.

   ------------
   -- STRUCT --
   ------------

   type Struct_Type is new Argument_List_Type and Argument_Type with private;
   --  D-Bus struct.

   for Struct_Type'External_Tag use "r";

   overriding
   procedure Serialize
     (Args   : Struct_Type;
      D_Args : not null access dbus_message_h.DBusMessageIter);
   --  Serialize struct to D-Bus arguments.

   overriding
   function Deserialize
     (D_Args : not null access dbus_message_h.DBusMessageIter)
      return Struct_Type;
   --  Create new struct from low-level D-Bus message argument.

   overriding
   function Get_Signature (Arg : Struct_Type) return String;
   --  Return the struct argument's signature.

   overriding
   function To_String (Arg : Struct_Type) return String;
   --  Return string representation of struct.

   function "+" (Left : Struct_Type) return Argument_List_Type;
   --  Create new argument list and add given struct to it.

   function "&"
     (Left  : Argument_List_Type;
      Right : Struct_Type)
      return Argument_List_Type;
   --  Add struct to argument list.

   ----------------
   -- DICT_ENTRY --
   ----------------

   type Dict_Entry_Type is new Argument_Type with private;
   --  Dict entry.

   for Dict_Entry_Type'External_Tag use "e";

   overriding
   procedure Serialize
     (Args   : Dict_Entry_Type;
      D_Args : not null access dbus_message_h.DBusMessageIter);
   --  Serialize dict entry to D-Bus arguments.

   overriding
   function Deserialize
     (D_Args : not null access dbus_message_h.DBusMessageIter)
      return Dict_Entry_Type;
   --  Create new dict entry from low-level D-Bus message argument.

   overriding
   function Get_Signature (Arg : Dict_Entry_Type) return String;
   --  Return the dict entry argument's signature.

   overriding
   function To_String (Arg : Dict_Entry_Type) return String;
   --  Return string representation of dict entry.

   function Create
     (Key   : Basic_Type'Class;
      Value : Argument_Type'Class)
      return Dict_Entry_Type;
   --  Create a new dict entry with given key/value.

   function Get_Key (Item : Dict_Entry_Type) return Basic_Type'Class;
   --  Return key element of the dict entry.

   function Get_Value (Item : Dict_Entry_Type) return Argument_Type'Class;
   --  Return value argument of the dict entry.

   -------------
   -- VARIANT --
   -------------

   type Variant_Type is new Argument_Type with private;
   --  D-Bus variant.

   for Variant_Type'External_Tag use "v";

   overriding
   procedure Serialize
     (Args   : Variant_Type;
      D_Args : not null access dbus_message_h.DBusMessageIter);
   --  Serialize variant to D-Bus arguments.

   overriding
   function Deserialize
     (D_Args : not null access dbus_message_h.DBusMessageIter)
      return Variant_Type;
   --  Create new variant from low-level D-Bus message argument.

   overriding
   function Get_Signature (Arg : Variant_Type) return String;
   --  Return the variant's signature.

   overriding
   function To_String (Arg : Variant_Type) return String;
   --  Return the string representation of variant.

   function "+" (Left : Variant_Type) return Argument_List_Type;
   --  Create a new argument list and add given variant to it.

   function "&"
     (Left  : Argument_List_Type;
      Right : Variant_Type)
      return Argument_List_Type;
   --  Add variant to argument list.

   function Create (Source : Argument_Type'Class) return Variant_Type;
   --  Create a new variant using the given argument as source.

   function Get_Argument (Item : Variant_Type) return Argument_Type'Class;
   --  Return the argument inside the variant.

private

   type Array_Type is new Argument_List_Type
     and Argument_Type with record
      Signature : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Struct_Type is new Argument_List_Type
     and Argument_Type with null record;

   type Dict_Entry_Type is new Argument_List_Type
     and Argument_Type with null record;

   type Variant_Type is new Argument_List_Type
     and Argument_Type with null record;

end D_Bus.Arguments.Containers;

--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2011, 2012  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2024  Andrew Athalye
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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with dbus_message_h;

with D_Bus.Marshaling;

package D_Bus.Arguments is

   type Argument_Type is interface and Marshaling.Object;
   --  D-Bus argument type.

   function To_String (Arg : Argument_Type) return String is abstract;
   --  Return string representation of an argument.

   function Get_Signature (Arg : Argument_Type) return String is abstract;
   --  Return the argument's signature.

   function Get_Code (Arg : Argument_Type'Class) return Integer;
   --  Return D-Bus code of argument type implementation. See the chapter
   --  'Type Signatures' in the D-Bus specification for more information.

   type Argument_List_Type is new Marshaling.Object with private;
   --  List of D-Bus arguments.

   Empty_Argument_List : constant Argument_List_Type;

   overriding
   procedure Serialize
     (Args   : Argument_List_Type;
      D_Args : not null access dbus_message_h.DBusMessageIter);
   --  Serialize list of argument types to D-Bus arguments.

   overriding
   function Deserialize
     (D_Args : not null access dbus_message_h.DBusMessageIter)
      return Argument_List_Type;
   --  Deserialize argument types from low-level D-Bus message starting at
   --  given message iterator position.

   procedure Append
     (List     : in out Argument_List_Type;
      New_Item :        Argument_Type'Class);
   --  Append argument to list.

   procedure Iterate
     (List    : Argument_List_Type;
      Process : not null access procedure (Arg : Argument_Type'Class));
   --  Iterate over arguments in the argument list.

   function First_Element
     (List : Argument_List_Type)
      return Argument_Type'Class;
   --  Return the first element in the argument list.

   function Last_Element
     (List : Argument_List_Type)
      return Argument_Type'Class;
   --  Return the last element in the argument list.

   function Get_Count (List : Argument_List_Type) return Natural;
   --  Return argument count.

   function Get_Element
     (List : Argument_List_Type;
      Pos  : Positive)
      return Argument_Type'Class;
   --  Return element at given position. Raises an exception if no argument
   --  exists at the specified position. List index range is 1 .. Get_Count().

   function Is_Empty (List : Argument_List_Type) return Boolean;
   --  Return True if argument list is empty.

   type Basic_Type is abstract new Argument_Type with private;
   --  Parent of all basic types.

   overriding
   function Get_Signature (Arg : Basic_Type) return String;
   --  Return the argument's signature.

   Arguments_Error : exception;

private

   type Basic_Type is abstract new Argument_Type with null record;

   package Argument_List_Package is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Element_Type => Argument_Type'Class);

   package ALP renames Argument_List_Package;

   type Argument_List_Type is new Marshaling.Object with record
      Data : ALP.List;
   end record;

   Empty_Argument_List : constant Argument_List_Type
     := Argument_List_Type'(others => <>);

   type ASCII_Code is (a, b, d, e, g, h, i, n, o, q, r, s, t, u, v, x, y);
   --  ASCII type codes of D-Bus types (see 'Type Signatures' in the D-Bus
   --  specification).

   Code_Table : constant array (ASCII_Code) of Integer
     := (a => Character'Pos ('a'),
         b => Character'Pos ('b'),
         d => Character'Pos ('d'),
         e => Character'Pos ('e'),
         i => Character'Pos ('i'),
         g => Character'Pos ('g'),
         h => Character'Pos ('h'),
         n => Character'Pos ('n'),
         o => Character'Pos ('o'),
         q => Character'Pos ('q'),
         r => Character'Pos ('r'),
         s => Character'Pos ('s'),
         t => Character'Pos ('t'),
         u => Character'Pos ('u'),
         v => Character'Pos ('v'),
         y => Character'Pos ('y'),
         x => Character'Pos ('x'));
   --  Mapping of ASCII codes to their integer representation.

   function Get_Tag (Arg : Argument_Type'Class) return String;
   --  Return the external tag of the given argument.

end D_Bus.Arguments;

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

with Ada.Strings.Unbounded;

with D_Bus.Types;

package D_Bus.Arguments.Basic is

   ------------
   -- STRING --
   ------------

   type String_Type is new Basic_Type with private;
   --  D-Bus basic string argument.

   for String_Type'External_Tag use "s";

   overriding
   procedure Serialize
     (Arg   : String_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);
   --  Serialize given string argument to D-Bus argument.

   overriding
   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return String_Type;
   --  Create new string argument from low-level D-Bus message argument.

   overriding
   function To_String (Arg : String_Type) return String;
   --  Return string representation of argument.

   function "+" (Left : String) return String_Type;
   --  Create new string argument.

   function "+" (Left : String) return Argument_List_Type;
   --  Create new argument list and add given string to it.

   function "&"
     (Left  : Argument_List_Type;
      Right : String)
      return Argument_List_Type;
   --  Add string to argument list.

   -----------------
   -- OBJECT_PATH --
   -----------------

   type Object_Path_Type is new Basic_Type with private;
   --  D-Bus object path argument.

   for Object_Path_Type'External_Tag use "o";

   overriding
   procedure Serialize
     (Arg   : Object_Path_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);
   --  Serialize given object path argument to D-Bus argument.

   overriding
   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Object_Path_Type;
   --  Create new obect path argument from low-level D-Bus message argument.

   overriding
   function To_String (Arg : Object_Path_Type) return String;
   --  Return string representation of argument.

   function "+" (Left : Types.Obj_Path) return Object_Path_Type;
   --  Create new object path argument.

   function "+" (Left : Types.Obj_Path) return Argument_List_Type;
   --  Create new argument list and add given object path to it.

   function "&"
     (Left  : Argument_List_Type;
      Right : Types.Obj_Path)
      return Argument_List_Type;
   --  Add object path to argument list.

   function To_Ada (Arg : Object_Path_Type) return Types.Obj_Path;
   --  Convert D-Bus object path argument to Ada object path.

   ---------------
   -- SIGNATURE --
   ---------------

   type Signature_Type is new D_Bus.Arguments.Basic_Type with private;
   for Signature_Type'External_Tag use "g";

   function To_String (Arg : Signature_Type) return String;
   function To_Ada (Arg : Signature_Type) return Types.Signature;

   function "+" (Left : Types.Signature) return Signature_Type;

   overriding procedure Serialize
     (Arg   : Signature_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);

   overriding function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Signature_Type;

   -------------
   -- BOOLEAN --
   -------------

   type Boolean_Type is new Basic_Type with private;
   --  D-Bus basic boolean argument.

   for Boolean_Type'External_Tag use "b";

   overriding
   procedure Serialize
     (Arg   : Boolean_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);
   --  Serialize given boolean argument to D-Bus argument.

   overriding
   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Boolean_Type;
   --  Create new boolean argument from low-level D-Bus message argument.

   overriding
   function To_String (Arg : Boolean_Type) return String;
   --  Return string representation of boolean argument.

   function "+" (Left : Boolean) return Boolean_Type;
   --  Create new boolean argument.

   function "+" (Left : Boolean) return Argument_List_Type;
   --  Create new argument list and add given boolean argument to it.

   function "&"
     (Left  : Argument_List_Type;
      Right : Boolean)
      return Argument_List_Type;
   --  Add boolean argument to argument list.

   function To_Ada (Arg : Boolean_Type) return Boolean;
   --  Convert D-Bus boolean argument to Ada type.

   ------------
   -- UINT64 --
   ------------

   type U_Int64_Type is new Basic_Type with private;
   --  D-Bus 64-bit unsigned integer type.

   for U_Int64_Type'External_Tag use "t";

   overriding
   procedure Serialize
     (Arg   : U_Int64_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);
   --  Serialize given uint64 argument to D-Bus argument.

   overriding
   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return U_Int64_Type;
   --  Create new uint64 argument from low-level D-Bus message argument.

   overriding
   function To_String (Arg : U_Int64_Type) return String;
   --  Return string representation of uint64 argument.

   function "+" (Left : Unsigned_64) return U_Int64_Type;
   --  Create new uint64 argument.

   function "+" (Left : Unsigned_64) return Argument_List_Type;
   --  Create new argument list and add given 64 bit unsigned number to it.

   function "&"
     (Left  : Argument_List_Type;
      Right : Unsigned_64)
      return Argument_List_Type;
   --  Add 64 bit unsigned number argument to argument list.

   function To_Ada (Arg : U_Int64_Type) return Unsigned_64;
   --  Convert D-Bus UINT64 argument to Ada type.

   -----------
   -- INT64 --
   -----------

   type Int64_Type is new Basic_Type with private;
   --  D-Bus 64-bit signed integer type.

   for Int64_Type'External_Tag use "x";

   overriding
   procedure Serialize
     (Arg   : Int64_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);
   --  Serialize given int64 argument to D-Bus argument.

   overriding
   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Int64_Type;
   --  Create new int64 argument from low-level D-Bus message argument.

   overriding
   function To_String (Arg : Int64_Type) return String;
   --  Return string representation of int64 argument.

   function "+" (Left : Signed_64) return Int64_Type;
   --  Create new int64 argument.

   function "+" (Left : Signed_64) return Argument_List_Type;
   --  Create new argument list and add given 64 bit signed number to it.

   function "&"
     (Left  : Argument_List_Type;
      Right : Signed_64)
      return Argument_List_Type;
   --  Add 64 bit signed number argument to argument list.

   function To_Ada (Arg : Int64_Type) return Signed_64;
   --  Convert D-Bus INT64 argument to Ada type.

   ------------
   -- UINT32 --
   ------------

   type U_Int32_Type is new Basic_Type with private;
   --  D-Bus 32-bit unsigned integer type.

   for U_Int32_Type'External_Tag use "u";

   overriding
   procedure Serialize
     (Arg   : U_Int32_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);
   --  Serialize given uint32 argument to D-Bus argument.

   overriding
   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return U_Int32_Type;
   --  Create new uint32 argument from low-level D-Bus message argument.

   overriding
   function To_String (Arg : U_Int32_Type) return String;
   --  Return string representation of uint32 argument.

   function "+" (Left : Unsigned_32) return U_Int32_Type;
   --  Create new uint32 argument.

   function "+" (Left : Unsigned_32) return Argument_List_Type;
   --  Create new argument list and add given 32 bit unsigned number to it.

   function "&"
     (Left  : Argument_List_Type;
      Right : Unsigned_32)
      return Argument_List_Type;
   --  Add 32 bit unsigned number argument to argument list.

   function To_Ada (Arg : U_Int32_Type) return Unsigned_32;
   --  Convert D-Bus UINT32 argument to Ada type.

   -----------
   -- INT32 --
   -----------

   type Int32_Type is new Basic_Type with private;
   --  D-Bus 32-bit signed integer type.

   for Int32_Type'External_Tag use "i";

   overriding
   procedure Serialize
     (Arg   : Int32_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);
   --  Serialize given int32 argument to D-Bus argument.

   overriding
   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Int32_Type;
   --  Create new int32 argument from low-level D-Bus message argument.

   overriding
   function To_String (Arg : Int32_Type) return String;
   --  Return string representation of int32 argument.

   function "+" (Left : Signed_32) return Int32_Type;
   --  Create new int32 argument.

   function "+" (Left : Signed_32) return Argument_List_Type;
   --  Create new argument list and add given 32 bit signed number to it.

   function "&"
     (Left  : Argument_List_Type;
      Right : Signed_32)
      return Argument_List_Type;
   --  Add 32 bit signed number argument to argument list.

   function To_Ada (Arg : Int32_Type) return Signed_32;
   --  Convert D-Bus INT32 argument to Ada type.

   ------------
   -- UINT16 --
   ------------

   type U_Int16_Type is new Basic_Type with private;
   --  D-Bus 16-bit unsigned integer type.

   for U_Int16_Type'External_Tag use "q";

   overriding
   procedure Serialize
     (Arg   : U_Int16_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);
   --  Serialize given uint16 argument to D-Bus argument.

   overriding
   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return U_Int16_Type;
   --  Create new uint16 argument from low-level D-Bus message argument.

   overriding
   function To_String (Arg : U_Int16_Type) return String;
   --  Return string representation of uint16 argument.

   function "+" (Left : Unsigned_16) return U_Int16_Type;
   --  Create new uint16 argument.

   function "+" (Left : Unsigned_16) return Argument_List_Type;
   --  Create new argument list and add given 16 bit unsigned number to it.

   function "&"
     (Left  : Argument_List_Type;
      Right : Unsigned_16)
      return Argument_List_Type;
   --  Add 16 bit unsigned number argument to argument list.

   function To_Ada (Arg : U_Int16_Type) return Unsigned_16;
   --  Convert D-Bus UINT16 argument to Ada type.

   -----------
   -- INT16 --
   -----------

   type Int16_Type is new Basic_Type with private;
   --  D-Bus 16-bit signed integer type.

   for Int16_Type'External_Tag use "n";

   overriding
   procedure Serialize
     (Arg   : Int16_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);
   --  Serialize given int16 argument to D-Bus argument.

   overriding
   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Int16_Type;
   --  Create new int16 argument from low-level D-Bus message argument.

   overriding
   function To_String (Arg : Int16_Type) return String;
   --  Return string representation of int16 argument.

   function "+" (Left : Signed_16) return Int16_Type;
   --  Create new int16 argument.

   function "+" (Left : Signed_16) return Argument_List_Type;
   --  Create new argument list and add given 16 bit signed number to it.

   function "&"
     (Left  : Argument_List_Type;
      Right : Signed_16)
      return Argument_List_Type;
   --  Add 16 bit signed number argument to argument list.

   function To_Ada (Arg : Int16_Type) return Signed_16;
   --  Convert D-Bus INT16 argument to Ada type.

   ----------
   -- BYTE --
   ----------

   type Byte_Type is new Basic_Type with private;
   --  D-Bus 8-bit unsigned integer type.

   for Byte_Type'External_Tag use "y";

   overriding
   procedure Serialize
     (Arg   : Byte_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);
   --  Serialize given byte argument to D-Bus argument.

   overriding
   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Byte_Type;
   --  Create new byte argument from low-level D-Bus message argument.

   overriding
   function To_String (Arg : Byte_Type) return String;
   --  Return string representation of byte argument.

   function "+" (Left : Byte) return Byte_Type;
   --  Create new byte argument.

   function "+" (Left : Byte) return Argument_List_Type;
   --  Create new argument list and add given byte to it.

   function "&"
     (Left  : Argument_List_Type;
      Right : Byte)
      return Argument_List_Type;
   --  Add byte to argument list.

   function To_Ada (Arg : Byte_Type) return Byte;
   --  Convert D-Bus byte argument to Ada type.

   ------------
   -- DOUBLE --
   ------------

   type Double_Type is new D_Bus.Arguments.Basic_Type with private;
   for Double_Type'External_Tag use "d";

   function To_String (Arg : Double_Type) return String;
   function To_Ada (Arg : Double_Type) return Double;

   function "+" (Left : Double) return Double_Type;

   overriding
   procedure Serialize
     (Arg   : Double_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);

   overriding
   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Double_Type;

   ---------------------
   -- FILE_DESCRIPTOR --
   ---------------------

   type File_Descriptor_Type is new D_Bus.Arguments.Basic_Type with private;
   for File_Descriptor_Type'External_Tag use "h";

   function To_String (Arg : File_Descriptor_Type) return String;
   function To_Ada (Arg : File_Descriptor_Type) return File_Descriptor;

   function "+" (Left : File_Descriptor) return File_Descriptor_Type;

   overriding
   procedure Serialize
     (Arg   : File_Descriptor_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter);

   overriding
   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return File_Descriptor_Type;
private

   type String_Type is new Basic_Type with record
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Object_Path_Type is new Basic_Type with record
      Value : Types.Obj_Path;
   end record;

   type Signature_Type is new Basic_Type with record
      Value : Types.Signature;
   end record;

   type Boolean_Type is new Basic_Type with record
      Value : Boolean := False;
   end record;

   type U_Int64_Type is new Basic_Type with record
      Value : Unsigned_64 := 0;
   end record;

   type Int64_Type is new Basic_Type with record
      Value : Signed_64 := 0;
   end record;

   type U_Int32_Type is new Basic_Type with record
      Value : Unsigned_32 := 0;
   end record;

   type Int32_Type is new Basic_Type with record
      Value : Signed_32 := 0;
   end record;

   type U_Int16_Type is new Basic_Type with record
      Value : Unsigned_16 := 0;
   end record;

   type Int16_Type is new Basic_Type with record
      Value : Signed_16 := 0;
   end record;

   type Byte_Type is new Basic_Type with record
      Value : Byte := 0;
   end record;

   type Double_Type is new Basic_Type with record
      Value : Double := 0.0;
   end record;

   type File_Descriptor_Type is new Basic_Type with record
      Value : File_Descriptor := 0;
   end record;

end D_Bus.Arguments.Basic;

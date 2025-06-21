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

with Ada.Strings.Fixed;

with System;

with Interfaces.C.Strings;

with dbus_types_h;
with dbus_arch_deps_h;

package body D_Bus.Arguments.Basic is

   package C renames Interfaces.C;

   procedure Serialize
     (Code     : Integer;
      Arg_Name : String;
      Address  : System.Address;
      D_Arg    : not null access dbus_message_h.DBusMessageIter);
   --  Serialize argument at address with given code to low-level D-Bus message
   --  position.

   procedure Deserialize
     (D_Arg   : not null access dbus_message_h.DBusMessageIter;
      Address : System.Address);
   --  Deserialize argument from low-level D-Bus message to given address.

   function Trim
     (Source : String;
      Side   : Ada.Strings.Trim_End := Ada.Strings.Left)
      return String
      renames Ada.Strings.Fixed.Trim;
   --  Convenience function to trim a basic argument string representation.

   -------------------------------------------------------------------------

   function "&"
     (Left  : Argument_List_Type;
      Right : String)
      return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      Arg_List := Left;
      Arg_List.Data.Append (New_Item => +Right);
      return Arg_List;
   end "&";

   -------------------------------------------------------------------------

   function "&"
     (Left  : Argument_List_Type;
      Right : Types.Obj_Path)
      return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      Arg_List := Left;
      Arg_List.Data.Append (New_Item => +Right);
      return Arg_List;
   end "&";

   -------------------------------------------------------------------------

   function "&"
     (Left  : Argument_List_Type;
      Right : Boolean)
      return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      Arg_List := Left;
      Arg_List.Data.Append (New_Item => +Right);
      return Arg_List;
   end "&";

   -------------------------------------------------------------------------

   function "&"
     (Left  : Argument_List_Type;
      Right : Unsigned_64)
      return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      Arg_List := Left;
      Arg_List.Data.Append (New_Item => +Right);
      return Arg_List;
   end "&";

   -------------------------------------------------------------------------

   function "&"
     (Left  : Argument_List_Type;
      Right : Signed_64)
      return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      Arg_List := Left;
      Arg_List.Data.Append (New_Item => +Right);
      return Arg_List;
   end "&";

   -------------------------------------------------------------------------

   function "&"
     (Left  : Argument_List_Type;
      Right : Unsigned_32)
      return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      Arg_List := Left;
      Arg_List.Data.Append (New_Item => +Right);
      return Arg_List;
   end "&";

   -------------------------------------------------------------------------

   function "&"
     (Left  : Argument_List_Type;
      Right : Signed_32)
      return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      Arg_List := Left;
      Arg_List.Data.Append (New_Item => +Right);
      return Arg_List;
   end "&";

   -------------------------------------------------------------------------

   function "&"
     (Left  : Argument_List_Type;
      Right : Unsigned_16)
      return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      Arg_List := Left;
      Arg_List.Data.Append (New_Item => +Right);
      return Arg_List;
   end "&";

   -------------------------------------------------------------------------

   function "&"
     (Left  : Argument_List_Type;
      Right : Signed_16)
      return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      Arg_List := Left;
      Arg_List.Data.Append (New_Item => +Right);
      return Arg_List;
   end "&";

   -------------------------------------------------------------------------

   function "&"
     (Left  : Argument_List_Type;
      Right : Byte)
      return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      Arg_List := Left;
      Arg_List.Data.Append (New_Item => +Right);
      return Arg_List;
   end "&";

   -------------------------------------------------------------------------

   function "+" (Left : String) return String_Type
   is
      use Ada.Strings.Unbounded;
   begin
      return String_Type'
        (Value => To_Unbounded_String (Source => Left));
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Types.Obj_Path) return Object_Path_Type
   is
   begin
      return Object_Path_Type'(Value => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Types.Signature) return Signature_Type is
   begin
      return Signature_Type'(Value => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Boolean) return Boolean_Type
   is
   begin
      return Boolean_Type'(Value => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Unsigned_64) return U_Int64_Type
   is
   begin
      return U_Int64_Type'(Value => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Signed_64) return Int64_Type
   is
   begin
      return Int64_Type'(Value => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Unsigned_32) return U_Int32_Type
   is
   begin
      return U_Int32_Type'(Value => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Signed_32) return Int32_Type
   is
   begin
      return Int32_Type'(Value => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Unsigned_16) return U_Int16_Type
   is
   begin
      return U_Int16_Type'(Value => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Signed_16) return Int16_Type
   is
   begin
      return Int16_Type'(Value => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Byte) return Byte_Type
   is
   begin
      return Byte_Type'(Value => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Double) return Double_Type
   is
   begin
      return Double_Type'(Value => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : File_Descriptor) return File_Descriptor_Type
   is
   begin
      return File_Descriptor_Type'(Value => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : String) return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      return "&"
        (Left  => Arg_List,
         Right => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Types.Obj_Path) return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      return "&"
        (Left  => Arg_List,
         Right => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Boolean) return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      return "&"
        (Left  => Arg_List,
         Right => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Unsigned_64) return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      return "&"
        (Left  => Arg_List,
         Right => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Signed_64) return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      return "&"
        (Left  => Arg_List,
         Right => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Unsigned_32) return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      return "&"
        (Left  => Arg_List,
         Right => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Signed_32) return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      return "&"
        (Left  => Arg_List,
         Right => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Unsigned_16) return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      return "&"
        (Left  => Arg_List,
         Right => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Signed_16) return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      return "&"
        (Left  => Arg_List,
         Right => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Byte) return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      return "&"
        (Left  => Arg_List,
         Right => Left);
   end "+";

   -------------------------------------------------------------------------

   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return String_Type
   is
      use Ada.Strings.Unbounded;

      New_String : String_Type;
      D_String   : C.Strings.chars_ptr;
   begin
      Deserialize (D_Arg   => D_Arg,
                   Address => D_String'Address);

      New_String.Value := To_Unbounded_String
        (Source => C.Strings.Value (Item => D_String));
      return New_String;
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Object_Path_Type
   is
      use type D_Bus.Types.Obj_Path;

      New_Object_Path : Object_Path_Type;
      D_String        : C.Strings.chars_ptr;
   begin
      Deserialize (D_Arg   => D_Arg,
                   Address => D_String'Address);

      New_Object_Path.Value := +C.Strings.Value (Item => D_String);
      return New_Object_Path;
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Signature_Type
   is
      use Types;

      D_String : Interfaces.C.Strings.chars_ptr;
   begin
      dbus_message_h.dbus_message_iter_get_basic
        (iter => D_Arg, value => D_String'Address);

      return Signature : Signature_Type do
         Signature.Value := +Interfaces.C.Strings.Value (D_String);
      end return;
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Boolean_Type
   is
      use type dbus_types_h.dbus_bool_t;

      New_Bool : Boolean_Type;
      D_Bool   : dbus_types_h.dbus_bool_t;
   begin
      Deserialize (D_Arg   => D_Arg,
                   Address => D_Bool'Address);

      New_Bool.Value := not (D_Bool = 0);
      return New_Bool;
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return U_Int64_Type
   is
      New_U_Int64 : U_Int64_Type;
      D_U_Int64   : dbus_arch_deps_h.dbus_uint64_t;
   begin
      Deserialize (D_Arg   => D_Arg,
                   Address => D_U_Int64'Address);

      New_U_Int64.Value := Unsigned_64 (D_U_Int64);
      return New_U_Int64;
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Int64_Type
   is
      New_Int64 : Int64_Type;
      D_Int64   : dbus_arch_deps_h.dbus_int64_t;
   begin
      Deserialize (D_Arg   => D_Arg,
                   Address => D_Int64'Address);

      New_Int64.Value := Signed_64 (D_Int64);
      return New_Int64;
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return U_Int32_Type
   is
      New_U_Int32 : U_Int32_Type;
      D_U_Int32   : dbus_arch_deps_h.dbus_uint32_t;
   begin
      Deserialize (D_Arg   => D_Arg,
                   Address => D_U_Int32'Address);

      New_U_Int32.Value := Unsigned_32 (D_U_Int32);
      return New_U_Int32;
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Int32_Type
   is
      New_Int32 : Int32_Type;
      D_Int32   : dbus_arch_deps_h.dbus_int32_t;
   begin
      Deserialize (D_Arg   => D_Arg,
                   Address => D_Int32'Address);

      New_Int32.Value := Signed_32 (D_Int32);
      return New_Int32;
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return U_Int16_Type
   is
      New_U_Int16 : U_Int16_Type;
      D_U_Int16   : dbus_arch_deps_h.dbus_uint16_t;
   begin
      Deserialize (D_Arg   => D_Arg,
                   Address => D_U_Int16'Address);

      New_U_Int16.Value := Unsigned_16 (D_U_Int16);
      return New_U_Int16;
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Int16_Type
   is
      New_Int16 : Int16_Type;
      D_Int16   : dbus_arch_deps_h.dbus_int16_t;
   begin
      Deserialize (D_Arg   => D_Arg,
                   Address => D_Int16'Address);

      New_Int16.Value := Signed_16 (D_Int16);
      return New_Int16;
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Byte_Type
   is
      New_Byte : Byte_Type;
   begin
      Deserialize (D_Arg   => D_Arg,
                   Address => New_Byte.Value'Address);

      return New_Byte;
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return Double_Type
   is
      Val : aliased Double;
   begin
      dbus_message_h.dbus_message_iter_get_basic
        (iter => D_Arg, value => Val'Address);

      return Double_Type'(Value => Val);
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Arg : not null access dbus_message_h.DBusMessageIter)
      return File_Descriptor_Type
   is
      Val : aliased File_Descriptor;
   begin
      dbus_message_h.dbus_message_iter_get_basic
        (iter => D_Arg, value => Val'Address);

      return File_Descriptor_Type'(Value => Val);
   end Deserialize;

   -------------------------------------------------------------------------

   procedure Deserialize
     (D_Arg   : not null access dbus_message_h.DBusMessageIter;
      Address : System.Address)
   is
   begin
      dbus_message_h.dbus_message_iter_get_basic
        (iter  => D_Arg,
         value => Address);
   end Deserialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Code     : Integer;
      Arg_Name : String;
      Address  : System.Address;
      D_Arg    : not null access dbus_message_h.DBusMessageIter)
   is
      use type dbus_types_h.dbus_bool_t;

      D_Res : dbus_types_h.dbus_bool_t;
   begin
      D_Res := dbus_message_h.dbus_message_iter_append_basic
        (iter   => D_Arg,
         c_type => C.int (Code),
         value  => Address);

      if D_Res = 0 then
         raise D_Bus_Error with "Unable to append basic "
           & Arg_Name & " argument";
      end if;
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Arg   : String_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
      D_Value : C.Strings.chars_ptr
        := C.Strings.New_String (Str => Arg.To_String);
   begin
      Serialize (Code     => Arg.Get_Code,
                 Arg_Name => "string",
                 Address  => D_Value'Address,
                 D_Arg    => D_Arg);

      C.Strings.Free (Item => D_Value);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Arg   : Object_Path_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
      D_Value : C.Strings.chars_ptr
        := C.Strings.New_String (Str => Arg.To_String);
   begin
      Serialize (Code     => Arg.Get_Code,
                 Arg_Name => "object path",
                 Address  => D_Value'Address,
                 D_Arg    => D_Arg);

      C.Strings.Free (Item => D_Value);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Arg   : Signature_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
      D_String : C.Strings.chars_ptr := C.Strings.New_String
        (Str => Arg.To_String);
   begin
      Serialize (Code     => Arg.Get_Code,
                 Arg_Name => "signature",
                 Address  => D_String'Address,
                 D_Arg    => D_Arg);

      C.Strings.Free (Item => D_String);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Arg   : Boolean_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
      D_Value : dbus_types_h.dbus_bool_t := Boolean'Pos (Arg.Value);
   begin
      Serialize (Code     => Arg.Get_Code,
                 Arg_Name => "boolean",
                 Address  => D_Value'Address,
                 D_Arg    => D_Arg);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Arg   : U_Int64_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
   begin
      Serialize (Code     => Arg.Get_Code,
                 Arg_Name => "uint64",
                 Address  => Arg.Value'Address,
                 D_Arg    => D_Arg);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Arg   : Int64_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
   begin
      Serialize (Code     => Arg.Get_Code,
                 Arg_Name => "int64",
                 Address  => Arg.Value'Address,
                 D_Arg    => D_Arg);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Arg   : U_Int32_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
   begin
      Serialize (Code     => Arg.Get_Code,
                 Arg_Name => "uint32",
                 Address  => Arg.Value'Address,
                 D_Arg    => D_Arg);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Arg   : Int32_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
   begin
      Serialize (Code     => Arg.Get_Code,
                 Arg_Name => "int32",
                 Address  => Arg.Value'Address,
                 D_Arg    => D_Arg);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Arg   : U_Int16_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
   begin
      Serialize (Code     => Arg.Get_Code,
                 Arg_Name => "uint16",
                 Address  => Arg.Value'Address,
                 D_Arg    => D_Arg);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Arg   : Int16_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
   begin
      Serialize (Code     => Arg.Get_Code,
                 Arg_Name => "int16",
                 Address  => Arg.Value'Address,
                 D_Arg    => D_Arg);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Arg   : Byte_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
   begin
      Serialize (Code     => Arg.Get_Code,
                 Arg_Name => "byte",
                 Address  => Arg.Value'Address,
                 D_Arg    => D_Arg);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Arg   : Double_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
   begin
      Serialize (Code     => Arg.Get_Code,
                 Arg_Name => "double",
                 Address  => Arg.Value'Address,
                 D_Arg    => D_Arg);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Arg   : File_Descriptor_Type;
      D_Arg : not null access dbus_message_h.DBusMessageIter)
   is
   begin
      Serialize (Code     => Arg.Get_Code,
                 Arg_Name => "file descriptor",
                 Address  => Arg.Value'Address,
                 D_Arg    => D_Arg);
   end Serialize;

   -------------------------------------------------------------------------

   function To_Ada (Arg : Object_Path_Type) return Types.Obj_Path
   is
   begin
      return Arg.Value;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_Ada (Arg : Signature_Type) return Types.Signature
   is
   begin
      return Arg.Value;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_Ada (Arg : Boolean_Type) return Boolean
   is
   begin
      return Arg.Value;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_Ada (Arg : U_Int64_Type) return Unsigned_64
   is
   begin
      return Arg.Value;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_Ada (Arg : U_Int32_Type) return Unsigned_32
   is
   begin
      return Arg.Value;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_Ada (Arg : Int64_Type) return Signed_64
   is
   begin
      return Arg.Value;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_Ada (Arg : Int32_Type) return Signed_32
   is
   begin
      return Arg.Value;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_Ada (Arg : U_Int16_Type) return Unsigned_16
   is
   begin
      return Arg.Value;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_Ada (Arg : Int16_Type) return Signed_16
   is
   begin
      return Arg.Value;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_Ada (Arg : Byte_Type) return Byte
   is
   begin
      return Arg.Value;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_Ada (Arg : Double_Type) return Double
   is
   begin
      return Arg.Value;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_Ada (Arg : File_Descriptor_Type) return File_Descriptor
   is
   begin
      return Arg.Value;
   end To_Ada;

   -------------------------------------------------------------------------

   function To_String (Arg : String_Type) return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Source => Arg.Value);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : Object_Path_Type) return String
   is
   begin
      return Types.To_String (Path => Arg.Value);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : Signature_Type) return String
   is
   begin
      return Types.To_String (Sig => Arg.Value);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : Boolean_Type) return String
   is
   begin
      return Arg.Value'Img;
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : U_Int64_Type) return String
   is
   begin
      return Trim (Source => Arg.Value'Img);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : Int64_Type) return String
   is
   begin
      return Trim (Source => Arg.Value'Img);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : U_Int32_Type) return String
   is
   begin
      return Trim (Source => Arg.Value'Img);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : Int32_Type) return String
   is
   begin
      return Trim (Source => Arg.Value'Img);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : U_Int16_Type) return String
   is
   begin
      return Trim (Source => Arg.Value'Img);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : Int16_Type) return String
   is
   begin
      return Trim (Source => Arg.Value'Img);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : Byte_Type) return String
   is
   begin
      return Trim (Source => Arg.Value'Img);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : Double_Type) return String
   is
   begin
      return Trim (Source => Arg.Value'Img);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : File_Descriptor_Type) return String
   is
   begin
      return Trim (Source => Arg.Value'Img);
   end To_String;

end D_Bus.Arguments.Basic;

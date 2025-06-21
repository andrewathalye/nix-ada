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

with Interfaces.C.Strings;

with dbus_types_h;

package body D_Bus.Arguments.Containers is

   package C renames Interfaces.C;

   procedure Serialize_Container
     (Args      : Argument_List_Type;
      Code      : ASCII_Code;
      Signature : String := "";
      D_Args    : not null access dbus_message_h.DBusMessageIter);
   --  Serialization helper for container types. Code is the container code,
   --  Signature is the required signature as described in the D-Bus low-level
   --  public API, function dbus_message_iter_open_container.

   -------------------------------------------------------------------------

   function "&"
     (Left  : Argument_List_Type;
      Right : Array_Type)
      return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      Arg_List := Left;
      Arg_List.Data.Append (New_Item => Right);
      return Arg_List;
   end "&";

   -------------------------------------------------------------------------

   function "&"
     (Left  : Argument_List_Type;
      Right : Struct_Type)
      return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      Arg_List := Left;
      Arg_List.Data.Append (New_Item => Right);
      return Arg_List;
   end "&";

   -------------------------------------------------------------------------

   function "&"
     (Left  : Argument_List_Type;
      Right : Variant_Type)
      return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      Arg_List := Left;
      Arg_List.Data.Append (New_Item => Right);
      return Arg_List;
   end "&";

   -------------------------------------------------------------------------

   function "+" (Left : Array_Type) return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      return "&"
        (Left  => Arg_List,
         Right => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Struct_Type) return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      return "&"
        (Left  => Arg_List,
         Right => Left);
   end "+";

   -------------------------------------------------------------------------

   function "+" (Left : Variant_Type) return Argument_List_Type
   is
      Arg_List : Argument_List_Type;
   begin
      return "&"
        (Left  => Arg_List,
         Right => Left);
   end "+";

   -------------------------------------------------------------------------

   procedure Append
     (List     : in out Array_Type;
      New_Item :        Argument_Type'Class)
   is
      use Ada.Strings.Unbounded;

      Arg_Signature : constant Unbounded_String := To_Unbounded_String
        (New_Item.Get_Signature);
   begin
      if List.Signature /= Null_Unbounded_String
         and then List.Signature /= Arg_Signature
      then
         raise D_Bus_Error with "Array can only hold one single complete type";
      end if;

      List.Signature := Arg_Signature;
      Argument_List_Type (List).Append (New_Item => New_Item);
   end Append;

   -------------------------------------------------------------------------

   function Create
     (Key   : Basic_Type'Class;
      Value : Argument_Type'Class)
      return Dict_Entry_Type
   is
      D : Dict_Entry_Type;
   begin
      D.Append (New_Item => Key);
      D.Append (New_Item => Value);

      return D;
   end Create;

   -------------------------------------------------------------------------

   function Create (Source : Argument_Type'Class) return Variant_Type
   is
      V : Variant_Type;
   begin
      V.Append (New_Item => Source);

      return V;
   end Create;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Args : not null access dbus_message_h.DBusMessageIter)
      return Array_Type
   is
      use Ada.Strings.Unbounded;
      use Interfaces.C.Strings;

      D_Sub_Args : aliased dbus_message_h.DBusMessageIter;

      Result : Array_Type;
   begin
      dbus_message_h.dbus_message_iter_recurse
        (iter => D_Args,
         sub  => D_Sub_Args'Access);

      Argument_List_Type (Result) := Deserialize (D_Sub_Args'Access);

      --  Set signature and skip leading 'a'
      declare
         C_Signature : C.Strings.chars_ptr :=
           dbus_message_h.dbus_message_iter_get_signature (D_Args);
         Signature : constant String := Value (C_Signature);
      begin
         C.Strings.Free (C_Signature);

         Result.Signature := To_Unbounded_String
           (Signature (Signature'First + 1 .. Signature'Last));
      end;

      return Result;
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Args : not null access dbus_message_h.DBusMessageIter)
      return Struct_Type
   is
      D_Sub_Args : aliased dbus_message_h.DBusMessageIter;
   begin
      dbus_message_h.dbus_message_iter_recurse
        (iter => D_Args,
         sub  => D_Sub_Args'Access);

      return Result : Struct_Type do
         Argument_List_Type (Result)
           := Deserialize (D_Args => D_Sub_Args'Access);
      end return;
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Args : not null access dbus_message_h.DBusMessageIter)
      return Dict_Entry_Type
   is
      D_Sub_Args : aliased dbus_message_h.DBusMessageIter;
   begin
      dbus_message_h.dbus_message_iter_recurse
        (iter => D_Args,
         sub  => D_Sub_Args'Access);

      return Result : Dict_Entry_Type do
         Argument_List_Type (Result)
           := Deserialize (D_Args => D_Sub_Args'Access);
      end return;
   end Deserialize;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Args : not null access dbus_message_h.DBusMessageIter)
      return Variant_Type
   is
      D_Sub_Args : aliased dbus_message_h.DBusMessageIter;
   begin
      dbus_message_h.dbus_message_iter_recurse
        (iter => D_Args,
         sub  => D_Sub_Args'Access);

      return Result : Variant_Type do
         Argument_List_Type (Result)
           := Deserialize (D_Args => D_Sub_Args'Access);
      end return;
   end Deserialize;

   -------------------------------------------------------------------------

   function Get_Argument (Item : Variant_Type) return Argument_Type'Class
   is
   begin
      return Item.First_Element;
   end Get_Argument;

   -------------------------------------------------------------------------

   function Get_Key (Item : Dict_Entry_Type) return Basic_Type'Class
   is
   begin
      return Basic_Type'Class (Item.First_Element);
   end Get_Key;

   -------------------------------------------------------------------------

   function Get_Signature (Arg : Array_Type) return String
   is
      use Ada.Strings.Unbounded;
   begin
      return "a" & To_String (Arg.Signature);
   end Get_Signature;

   -------------------------------------------------------------------------

   function Get_Signature (Arg : Struct_Type) return String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;

      procedure Append_Sig (Arg : Argument_Type'Class);
      --  Append argument's signature to struct signature.

      procedure Append_Sig (Arg : Argument_Type'Class)
      is
      begin
         Result := Result & Arg.Get_Signature;
      end Append_Sig;
   begin
      Result := Result & "(";
      Arg.Iterate (Process => Append_Sig'Access);
      Result := Result & ")";

      return To_String (Result);
   end Get_Signature;

   -------------------------------------------------------------------------

   function Get_Signature (Arg : Dict_Entry_Type) return String
   is
   begin
      return "{" & Arg.First_Element.Get_Signature
        & Arg.Last_Element.Get_Signature & "}";
   end Get_Signature;

   -------------------------------------------------------------------------

   function Get_Signature (Arg : Variant_Type) return String
   is
      pragma Unreferenced (Arg);
   begin
      return "v";
   end Get_Signature;

   -------------------------------------------------------------------------

   function Get_Value (Item : Dict_Entry_Type) return Argument_Type'Class
   is
   begin
      return Item.Last_Element;
   end Get_Value;

   -------------------------------------------------------------------------

   procedure Serialize
     (Args   : Array_Type;
      D_Args : not null access dbus_message_h.DBusMessageIter)
   is
      use Ada.Strings.Unbounded;
   begin
      if Args.Signature = Null_Unbounded_String then
         raise D_Bus_Error with
            "Serialization error: Empty arrays must have their signatures" &
            " set manually (Set_Signature).";
      end if;

      Serialize_Container (Args      => Argument_List_Type (Args),
                           Code      => a,
                           Signature => To_String (Args.Signature),
                           D_Args    => D_Args);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Args   : Struct_Type;
      D_Args : not null access dbus_message_h.DBusMessageIter)
   is
   begin
      if Args.Is_Empty then
         raise D_Bus_Error with "Serialization error: Struct is empty";
      end if;

      Serialize_Container (Args      => Argument_List_Type (Args),
                           Code      => r,
                           Signature => "",
                           D_Args    => D_Args);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Args   : Dict_Entry_Type;
      D_Args : not null access dbus_message_h.DBusMessageIter)
   is
   begin
      if Args.Get_Count /= 2 then
         raise D_Bus_Error with
           "Serialization error: dict entry needs exactly two elements";
      end if;

      Serialize_Container (Args      => Argument_List_Type (Args),
                           Code      => e,
                           Signature => "",
                           D_Args    => D_Args);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (Args   : Variant_Type;
      D_Args : not null access dbus_message_h.DBusMessageIter)
   is
   begin
      if Args.Get_Count /= 1 then
         raise D_Bus_Error with
           "Serialization error: variant contains no argument";
      end if;

      Serialize_Container (Args      => Argument_List_Type (Args),
                           Code      => v,
                           Signature => Args.First_Element.Get_Signature,
                           D_Args    => D_Args);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize_Container
     (Args      : Argument_List_Type;
      Code      : ASCII_Code;
      Signature : String := "";
      D_Args    : not null access dbus_message_h.DBusMessageIter)
   is
      use type dbus_types_h.dbus_bool_t;
      use type C.Strings.chars_ptr;

      C_Sig      : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      D_Sub_Args : aliased dbus_message_h.DBusMessageIter;
      D_Res      : dbus_types_h.dbus_bool_t;
   begin
      if Signature'Length /= 0 then
         C_Sig := C.Strings.New_String (Str => Signature);
      end if;

      D_Res := dbus_message_h.dbus_message_iter_open_container
        (iter                => D_Args,
         c_type              => C.int (Code_Table (Code)),
         contained_signature => C_Sig,
         sub                 => D_Sub_Args'Access);

      if C_Sig /= C.Strings.Null_Ptr then
         C.Strings.Free (Item => C_Sig);
      end if;

      if D_Res = 0 then
         raise D_Bus_Error with "Unable to create D-Bus container";
      end if;

      Args.Serialize (D_Args => D_Sub_Args'Access);

      D_Res := dbus_message_h.dbus_message_iter_close_container
        (iter => D_Args,
         sub  => D_Sub_Args'Access);

      if D_Res = 0 then
         raise D_Bus_Error with "Unable to close D-Bus container";
      end if;
   end Serialize_Container;

   -------------------------------------------------------------------------

   procedure Set_Signature
     (List : in out Array_Type;
      Signature : String)
   is
      use Ada.Strings.Unbounded;
   begin
      if List.Signature /= Null_Unbounded_String then
         raise D_Bus_Error with "Tried to Set_Signature on a non-empty List";
      end if;

      List.Signature := To_Unbounded_String (Signature);
   end Set_Signature;

   -------------------------------------------------------------------------

   function To_String (Arg : Array_Type) return String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;

      procedure Append_String (Arg : Argument_Type'Class);
      --  Append argument's string representation to result string

      procedure Append_String (Arg : Argument_Type'Class)
      is
      begin
         Result := Result & " " & Arg.To_String;
      end Append_String;
   begin
      Result := Result & "[";
      Arg.Iterate (Process => Append_String'Access);
      Result := Result & " ]";

      return To_String (Result);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : Struct_Type) return String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;

      procedure Append_String (Arg : Argument_Type'Class);
      --  Append argument's string representation to result string

      procedure Append_String (Arg : Argument_Type'Class)
      is
      begin
         Result := Result & " " & Arg.To_String;
      end Append_String;
   begin
      Result := Result & "(";
      Arg.Iterate (Process => Append_String'Access);
      Result := Result & " )";

      return To_String (Result);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : Dict_Entry_Type) return String
   is
   begin
      return "{ key:" & Arg.First_Element.To_String
        & ", value:" & Arg.Last_Element.To_String & " }";
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Arg : Variant_Type) return String
   is
      Contained : constant Argument_Type'Class := Arg.Get_Argument;
   begin
      return Contained.Get_Tag & ", " & Contained.To_String;
   end To_String;

end D_Bus.Arguments.Containers;

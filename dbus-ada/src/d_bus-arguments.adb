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

with Ada.Tags;

with Interfaces.C;

with dbus_types_h;

package body D_Bus.Arguments is

   package C renames Interfaces.C;

   -------------------------------------------------------------------------

   procedure Append
     (List     : in out Argument_List_Type;
      New_Item :        Argument_Type'Class)
   is
   begin
      List.Data.Append (New_Item => New_Item);
   end Append;

   -------------------------------------------------------------------------

   function Deserialize
     (D_Args : not null access dbus_message_h.DBusMessageIter)
      return Argument_List_Type
   is
      use type C.int;
      use type dbus_types_h.dbus_bool_t;

      Result : Arguments.Argument_List_Type;
   begin
      loop
         declare
            Type_Code : C.int;
            Type_Char : String (1 .. 1);
            Type_Tag  : Ada.Tags.Tag;
         begin
            Type_Code := dbus_message_h.dbus_message_iter_get_arg_type
              (iter => D_Args);

            if Type_Code /= 0 then
               Type_Char (1) := Character'Val (Type_Code);

               begin
                  Type_Tag := Ada.Tags.Internal_Tag (Type_Char);

               exception
                  when Ada.Tags.Tag_Error =>
                     raise D_Bus_Error with "Unknown type code '"
                       & Type_Char & "' in message";
               end;

               Arguments.Append
                 (List     => Result,
                  New_Item => Argument_Type'Class
                    (Marshaling.Make_Object
                       (The_Tag => Type_Tag,
                        Params  => D_Args)));
            end if;

            exit when dbus_message_h.dbus_message_iter_next
              (iter => D_Args) = 0;
         end;
      end loop;

      return Result;
   end Deserialize;

   -------------------------------------------------------------------------

   function First_Element
     (List : Argument_List_Type)
      return Argument_Type'Class
   is
   begin
      return List.Data.First_Element;

   exception
      when Constraint_Error =>
         raise Arguments_Error with "Argument list is empty";
   end First_Element;

   -------------------------------------------------------------------------

   function Get_Code (Arg : Argument_Type'Class) return Integer
   is
      Tag  : constant String := Get_Tag (Arg);
      Code : ASCII_Code;
   begin
      begin
         Code := ASCII_Code'Value (Tag);

      exception
         when Constraint_Error =>
            raise D_Bus_Error with "Argument tag '" & Tag
              & "' not found in type code table";
      end;

      return Code_Table (Code);
   end Get_Code;

   -------------------------------------------------------------------------

   function Get_Count (List : Argument_List_Type) return Natural
   is
   begin
      return Natural (List.Data.Length);
   end Get_Count;

   -------------------------------------------------------------------------

   function Get_Element
     (List : Argument_List_Type;
      Pos  : Positive)
      return Argument_Type'Class
   is
      Cursor : ALP.Cursor := List.Data.First;
   begin
      if Pos > Get_Count (List => List) then
         raise Arguments_Error with "No element at position" & Pos'Img;
      end if;

      for I in 1 .. Pos - 1 loop
         Cursor := ALP.Next (Position => Cursor);
      end loop;

      return ALP.Element (Position => Cursor);
   end Get_Element;

   -------------------------------------------------------------------------

   function Get_Signature (Arg : Basic_Type) return String
   is
   begin
      return Get_Tag (Arg => Argument_Type'Class (Arg));
   end Get_Signature;

   -------------------------------------------------------------------------

   function Get_Tag (Arg : Argument_Type'Class) return String
   is
   begin
      return Ada.Tags.External_Tag (T => Arg'Tag);
   end Get_Tag;

   -------------------------------------------------------------------------

   function Is_Empty (List : Argument_List_Type) return Boolean
   is
   begin
      return List.Data.Is_Empty;
   end Is_Empty;

   -------------------------------------------------------------------------

   procedure Iterate
     (List    : Argument_List_Type;
      Process : not null access procedure (Arg : Argument_Type'Class))
   is
      procedure Process_Arg (Position : ALP.Cursor);
      --  Call the process procedure for an arg in the list.

      procedure Process_Arg (Position : ALP.Cursor)
      is
         Arg : constant Argument_Type'Class := ALP.Element (Position);
      begin
         Process (Arg);
      end Process_Arg;
   begin
      List.Data.Iterate (Process_Arg'Access);
   end Iterate;

   -------------------------------------------------------------------------

   function Last_Element
     (List : Argument_List_Type)
      return Argument_Type'Class
   is
   begin
      return List.Data.Last_Element;

   exception
      when Constraint_Error =>
         raise Arguments_Error with "Argument list is empty";
   end Last_Element;

   -------------------------------------------------------------------------

   procedure Serialize
     (Args   : Argument_List_Type;
      D_Args : not null access dbus_message_h.DBusMessageIter)
   is
      procedure To_D_Bus (Arg : Argument_Type'Class);
      --  Append given argument to D-Bus message arguments.

      procedure To_D_Bus (Arg : Argument_Type'Class)
      is
      begin
         Arg.Serialize (D_Msg => D_Args);
      end To_D_Bus;
   begin
      Arguments.Iterate
        (List    => Args,
         Process => To_D_Bus'Access);
   end Serialize;

end D_Bus.Arguments;

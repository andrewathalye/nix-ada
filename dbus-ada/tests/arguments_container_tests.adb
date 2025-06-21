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

with Ahven;

with Config;

with D_Bus.Types;
with D_Bus.Connection;
with D_Bus.Arguments.Basic;
with D_Bus.Arguments.Containers;

package body Arguments_Container_Tests is

   use Ahven;
   use D_Bus;
   use D_Bus.Arguments.Basic;
   use D_Bus.Arguments.Containers;
   use type D_Bus.Types.Obj_Path;

   -------------------------------------------------------------------------

   procedure Append_To_Array
   is
      A : Array_Type;
   begin
      A.Append (New_Item => +Unsigned_32'(1232));

      begin
         A.Append (New_Item => +"testmessage");
         Fail (Message => "Expected D-Bus error");

      exception
         when D_Bus_Error => null;
      end;
   end Append_To_Array;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Container arguments handling");
      T.Add_Test_Routine
        (Routine => Append_To_Array'Access,
         Name    => "Append to array");
      T.Add_Test_Routine
        (Routine => Marshal_Array_Of_Strings'Access,
         Name    => "Marshal/unmarshal array of strings");
      T.Add_Test_Routine
        (Routine => Marshal_Array_Of_Arrays_Of_Strings'Access,
         Name    => "Marshal/unmarshal array of arrays of strings");
      T.Add_Test_Routine
        (Routine => Marshal_Struct'Access,
         Name    => "Marshal/unmarshal struct");
      T.Add_Test_Routine
        (Routine => Marshal_Dict_Entries'Access,
         Name    => "Marshal/unmarshal dict entries");
      T.Add_Test_Routine
        (Routine => Marshal_Dict_Entries_With_Variant_Value'Access,
         Name    => "Marshal/unmarshal dict entries with variants");
      T.Add_Test_Routine
        (Routine => Marshal_Variant'Access,
         Name    => "Marshal/unmarshal variant");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Marshal_Array_Of_Arrays_Of_Strings
   is
      --  Marshal/unmarshal an array of arrays of strings.

      Conn   : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result : Arguments.Argument_List_Type;
      A1     : Array_Type;
      A2     : Array_Type;
   begin
      A2.Append (New_Item => +"string1");
      A2.Append (New_Item => +"string2");
      A2.Append (New_Item => +"string3");

      A1.Append (New_Item => A2);
      A1.Append (New_Item => A2);

      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +A1);

      declare
         R_A : constant Array_Type := Array_Type (Result.First_Element);
      begin
         Assert (Condition => R_A.Get_Count = 2,
                 Message   => "Count not 2");
         Assert (Condition => Array_Type (R_A.First_Element).Get_Count = 3,
                 Message   => "String count not 3 (1)");
         Assert (Condition => Array_Type (R_A.Last_Element).Get_Count = 3,
                 Message   => "String count not 3 (2)");
      end;
   end Marshal_Array_Of_Arrays_Of_Strings;

   -------------------------------------------------------------------------

   procedure Marshal_Array_Of_Strings
   is
      Conn   : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result : Arguments.Argument_List_Type;
      A      : Array_Type;
   begin
      A.Append (New_Item => +"string1");
      A.Append (New_Item => +"string2");
      A.Append (New_Item => +"string3");

      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +A);

      declare
         R_A : constant Array_Type := Array_Type (Result.First_Element);
      begin
         Assert (Condition => R_A.Get_Count = 3,
                 Message   => "Count not 3");
         Assert (Condition => String_Type
                 (R_A.First_Element).To_String = "string1",
                 Message   => "First element not string1");
         Assert (Condition => String_Type
                 (R_A.Last_Element).To_String = "string3",
                 Message   => "Last element not string3");
      end;
   end Marshal_Array_Of_Strings;

   -------------------------------------------------------------------------

   procedure Marshal_Dict_Entries
   is
      Conn   : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result : Arguments.Argument_List_Type;
      A      : Array_Type;
      D      : Dict_Entry_Type;
   begin
      D := Create (Key   => +Signed_16'(1234),
                   Value => +"value");

      A.Append (New_Item => D);
      A.Append (New_Item => D);

      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +A);

      declare
         R_A    : constant Array_Type := Array_Type (Result.First_Element);
         D1, D2 : Dict_Entry_Type;
      begin
         Assert (Condition => R_A.Get_Count = 2,
                 Message   => "Count not 2");

         D1 := Dict_Entry_Type (R_A.First_Element);
         D2 := Dict_Entry_Type (R_A.Last_Element);

         Assert (Condition => Int16_Type (D1.Get_Key).To_Ada = 1234,
                 Message   => "D1 key not 1234");
         Assert (Condition => Int16_Type (D2.Get_Key).To_Ada = 1234,
                 Message   => "D2 key not 1234");
         Assert (Condition => String_Type (D1.Get_Value).To_String = "value",
                 Message   => "D1 value mismatch");
         Assert (Condition => String_Type (D2.Get_Value).To_String = "value",
                 Message   => "D2 value mismatch");
      end;
   end Marshal_Dict_Entries;

   -------------------------------------------------------------------------

   procedure Marshal_Dict_Entries_With_Variant_Value
   is
      Conn   : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result : Arguments.Argument_List_Type;
      A      : Array_Type;
      D      : Dict_Entry_Type;
      V      : Variant_Type;
   begin
      V := Create (Source => +Unsigned_32'(12));

      D := Create (Key   => +Signed_16'(1234),
                   Value => V);

      A.Append (New_Item => D);
      A.Append (New_Item => D);

      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +A);

      declare
         R_A    : constant Array_Type := Array_Type (Result.First_Element);
         D1, D2 : Dict_Entry_Type;
      begin
         Assert (Condition => R_A.Get_Count = 2,
                 Message   => "Count not 2");

         D1 := Dict_Entry_Type (R_A.First_Element);
         D2 := Dict_Entry_Type (R_A.Last_Element);

         Assert (Condition => Int16_Type (D1.Get_Key).To_Ada = 1234,
                 Message   => "D1 key not 1234");
         Assert (Condition => Int16_Type (D2.Get_Key).To_Ada = 1234,
                 Message   => "D2 key not 1234");
         Assert (Condition => Variant_Type (D1.Get_Value).To_String = "u, 12",
                 Message   => "D1 value mismatch");
         Assert (Condition => Variant_Type (D2.Get_Value).To_String = "u, 12",
                 Message   => "D2 value mismatch");
      end;
   end Marshal_Dict_Entries_With_Variant_Value;

   -------------------------------------------------------------------------

   procedure Marshal_Struct
   is
      Conn   : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result : Arguments.Argument_List_Type;
      R      : Struct_Type;
   begin
      R.Append (New_Item => +Signed_32'(-45653));
      R.Append (New_Item => +Boolean'(False));
      R.Append (New_Item => +Byte'(254));

      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +R);

      declare
         R_R : constant Struct_Type := Struct_Type (Result.First_Element);
      begin
         Assert (Condition => R_R.Get_Count = 3,
                 Message   => "Count not 3");
         Assert (Condition => Int32_Type (R_R.First_Element).To_Ada = -45653,
                 Message   => "First element mismatch");
         Assert (Condition => Byte_Type (R_R.Last_Element).To_Ada = 254,
                 Message   => "Last element mismatch");
      end;
   end Marshal_Struct;

   -------------------------------------------------------------------------

   procedure Marshal_Variant
   is
      Conn   : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result : Arguments.Argument_List_Type;
      V      : Variant_Type;
   begin
      V := Create (Source => +Unsigned_32'(123456));

      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +V);

      declare
         R_V : constant Variant_Type := Variant_Type (Result.First_Element);
      begin
         Assert (Condition => U_Int32_Type (R_V.Get_Argument).To_Ada = 123456,
                 Message   => "Arg mismatch");
      end;
   end Marshal_Variant;

end Arguments_Container_Tests;

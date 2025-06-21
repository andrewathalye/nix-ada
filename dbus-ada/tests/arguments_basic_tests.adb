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

package body Arguments_Basic_Tests is

   use Ahven;
   use D_Bus;
   use D_Bus.Arguments;
   use D_Bus.Arguments.Basic;
   use type D_Bus.Types.Obj_Path;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Basic arguments handling");
      T.Add_Test_Routine
        (Routine => Marshal_String_Type'Access,
         Name    => "Marshal/unmarshal string type");
      T.Add_Test_Routine
        (Routine => Marshal_Object_Path_Type'Access,
         Name    => "Marshal/unmarshal object path type");
      T.Add_Test_Routine
        (Routine => Marshal_Boolean_Type'Access,
         Name    => "Marshal/unmarshal boolean type");
      T.Add_Test_Routine
        (Routine => Marshal_U_Int64_Type'Access,
         Name    => "Marshal/unmarshal uint64 type");
      T.Add_Test_Routine
        (Routine => Marshal_Int64_Type'Access,
         Name    => "Marshal/unmarshal int64 type");
      T.Add_Test_Routine
        (Routine => Marshal_U_Int32_Type'Access,
         Name    => "Marshal/unmarshal uint32 type");
      T.Add_Test_Routine
        (Routine => Marshal_Int32_Type'Access,
         Name    => "Marshal/unmarshal int32 type");
      T.Add_Test_Routine
        (Routine => Marshal_U_Int16_Type'Access,
         Name    => "Marshal/unmarshal uint16 type");
      T.Add_Test_Routine
        (Routine => Marshal_Int16_Type'Access,
         Name    => "Marshal/unmarshal int16 type");
      T.Add_Test_Routine
        (Routine => Marshal_Byte_Type'Access,
         Name    => "Marshal/unmarshal byte type");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Marshal_Boolean_Type
   is
      Conn   : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result : Arguments.Argument_List_Type;
   begin
      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +True);

      declare
         Bool : constant Boolean_Type := Boolean_Type (Result.First_Element);
      begin
         Assert (Condition => Bool.To_Ada,
                 Message   => "Result not True");
      end;
   end Marshal_Boolean_Type;

   -------------------------------------------------------------------------

   procedure Marshal_Byte_Type
   is
      Conn   : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result : Argument_List_Type;
   begin
      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Byte'Last);

      declare
         B : constant Byte_Type := Byte_Type (Result.First_Element);
      begin
         Assert (Condition => B.To_Ada = Byte'Last,
                 Message   => "Incorrect upper bound");
      end;

      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Byte'First);

      declare
         B : constant Byte_Type := Byte_Type (Result.First_Element);
      begin
         Assert (Condition => B.To_Ada = Byte'First,
                 Message   => "Incorrect lower bound");
      end;
   end Marshal_Byte_Type;

   -------------------------------------------------------------------------

   procedure Marshal_Int16_Type
   is
      Conn   : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result : Argument_List_Type;
   begin
      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Signed_16'Last);

      declare
         Int16 : constant Int16_Type
           := Int16_Type (Result.First_Element);
      begin
         Assert (Condition => Int16.To_Ada = Signed_16'Last,
                 Message   => "Incorrect upper bound");
      end;

      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Signed_16'First);

      declare
         Int16 : constant Int16_Type
           := Int16_Type (Result.First_Element);
      begin
         Assert (Condition => Int16.To_Ada = Signed_16'First,
                 Message   => "Incorrect lower bound");
      end;
   end Marshal_Int16_Type;

   -------------------------------------------------------------------------

   procedure Marshal_Int32_Type
   is
      Conn   : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result : Argument_List_Type;
   begin
      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Signed_32'Last);

      declare
         Int32 : constant Int32_Type := Int32_Type (Result.First_Element);
      begin
         Assert (Condition => Int32.To_Ada = Signed_32'Last,
                 Message   => "Incorrect upper bound");
      end;

      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Signed_32'First);

      declare
         Int32 : constant Int32_Type := Int32_Type (Result.First_Element);
      begin
         Assert (Condition => Int32.To_Ada = Signed_32'First,
                 Message   => "Incorrect lower bound");
      end;
   end Marshal_Int32_Type;

   -------------------------------------------------------------------------

   procedure Marshal_Int64_Type
   is
      Conn   : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result : Argument_List_Type;
   begin
      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Signed_64'Last);

      declare
         Int64 : constant Int64_Type := Int64_Type (Result.First_Element);
      begin
         Assert (Condition => Int64.To_Ada = Signed_64'Last,
                 Message   => "Incorrect upper bound");
      end;

      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Signed_64'First);

      declare
         Int64 : constant Int64_Type := Int64_Type (Result.First_Element);
      begin
         Assert (Condition => Int64.To_Ada = Signed_64'First,
                 Message   => "Incorrect lower bound");
      end;
   end Marshal_Int64_Type;

   -------------------------------------------------------------------------

   procedure Marshal_Object_Path_Type
   is
      Ref_Path : constant Types.Obj_Path := +"/com/example/MusicPlayer1";
      Conn     : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result   : Argument_List_Type;
   begin
      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Ref_Path);

      declare
         Path_Arg : constant Object_Path_Type
           := Object_Path_Type (Result.First_Element);
      begin
         Assert (Condition => Path_Arg.To_Ada = Ref_Path,
                 Message   => "Object path mismatch");
      end;
   end Marshal_Object_Path_Type;

   -------------------------------------------------------------------------

   procedure Marshal_String_Type
   is
      Ref_Str : constant String := "this is a teststring";
      Conn    : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result  : Argument_List_Type;
   begin
      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Ref_Str);

      Assert (Condition => Ref_Str = Result.First_Element.To_String,
              Message   => "String type mismatch");
   end Marshal_String_Type;

   -------------------------------------------------------------------------

   procedure Marshal_U_Int16_Type
   is
      Conn   : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result : Argument_List_Type;
   begin
      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Unsigned_16'Last);

      declare
         U_Int16 : constant U_Int16_Type
           := U_Int16_Type (Result.First_Element);
      begin
         Assert (Condition => U_Int16.To_Ada = Unsigned_16'Last,
                 Message   => "Incorrect upper bound");
      end;

      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Unsigned_16'First);

      declare
         U_Int16 : constant U_Int16_Type
           := U_Int16_Type (Result.First_Element);
      begin
         Assert (Condition => U_Int16.To_Ada = Unsigned_16'First,
                 Message   => "Incorrect lower bound");
      end;
   end Marshal_U_Int16_Type;

   -------------------------------------------------------------------------

   procedure Marshal_U_Int32_Type
   is
      Conn   : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result : Argument_List_Type;
   begin
      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Unsigned_32'Last);

      declare
         U_Int32 : constant U_Int32_Type
           := U_Int32_Type (Result.First_Element);
      begin
         Assert (Condition => U_Int32.To_Ada = Unsigned_32'Last,
                 Message   => "Incorrect upper bound");
      end;

      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Unsigned_32'First);

      declare
         U_Int32 : constant U_Int32_Type
           := U_Int32_Type (Result.First_Element);
      begin
         Assert (Condition => U_Int32.To_Ada = Unsigned_32'First,
                 Message   => "Incorrect lower bound");
      end;
   end Marshal_U_Int32_Type;

   -------------------------------------------------------------------------

   procedure Marshal_U_Int64_Type
   is
      Conn   : constant Connection.Connection_Type
        := Connection.Connect (Address => Config.Service_Addr);
      Result : Argument_List_Type;
   begin
      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Unsigned_64'Last);

      declare
         U_Int64 : constant U_Int64_Type
           := U_Int64_Type (Result.First_Element);
      begin
         Assert (Condition => U_Int64.To_Ada = Unsigned_64'Last,
                 Message   => "Incorrect upper bound");
      end;

      Result := Connection.Call_Blocking
        (Connection  => Conn,
         Destination => "dbus.ada.server",
         Path        => +"/",
         Iface       => "dbus.ada.server.type",
         Method      => "rebound",
         Args        => +Unsigned_64'First);

      declare
         U_Int64 : constant U_Int64_Type
           := U_Int64_Type (Result.First_Element);
      begin
         Assert (Condition => U_Int64.To_Ada = Unsigned_64'First,
                 Message   => "Incorrect lower bound");
      end;
   end Marshal_U_Int64_Type;

end Arguments_Basic_Tests;

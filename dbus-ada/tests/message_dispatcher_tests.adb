--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
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

with D_Bus.Messages;
with D_Bus.Connection;
with D_Bus.Arguments.Basic;
with D_Bus.Message_Dispatcher.Proc;
with D_Bus.Message_Dispatcher.Object;
with D_Bus.Types;

package body Message_Dispatcher_Tests is

   use Ahven;
   use D_Bus;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Message dispatchers");
      T.Add_Test_Routine
        (Routine => Proc_Dispatcher'Access,
         Name    => "Procedure-based dispatching");
      T.Add_Test_Routine
        (Routine => OO_Dispatcher'Access,
         Name    => "Object-based dispatching");
   end Initialize;

   -------------------------------------------------------------------------

   procedure OO_Dispatcher
   is
      type Object_Type is new
        D_Bus.Message_Dispatcher.Object.Handler_Type
      with record
         Called : Boolean := False;
      end record;

      ----------------------------------------------------------------------

      overriding
      procedure Dispatch
        (O     : in out Object_Type;
         In_Mg :        D_Bus.Messages.Message_Type);

      procedure Dispatch
        (O     : in out Object_Type;
         In_Mg :        D_Bus.Messages.Message_Type)
      is
         pragma Unreferenced (In_Mg);
      begin
         O.Called := True;
      end Dispatch;

      use D_Bus.Arguments;
      use D_Bus.Arguments.Basic;
      use type D_Bus.Types.Obj_Path;

      Object  : aliased Object_Type;
      Success : Boolean;
      D       : Message_Dispatcher.Object.OO_Dispatcher_Type;
      Dummy   : Argument_List_Type;
      Conn    : constant Connection.Connection_Type
        := Connection.Connect (Bus => Bus_Session);
   begin
      Connection.Add_Match
        (Connection => Conn,
         Rule       => "type='signal'");
      D.Register_Signal_Handler
        (Iface       => "dbus.ada.msg_dispatcher",
         Signal_Name => "event",
         Handler     => Object'Unchecked_Access);

      Connection.Send_Signal
        (Connection  => Conn,
         Object_Name => +"/",
         Iface       => "dbus.ada.msg_dispatcher",
         Name        => "event",
         Args        => +True);

      for I in 1 .. 10 loop
         D.Peek (Conn    => Conn,
                 Success => Success);
         exit when not Success or Object.Called;
         delay 0.1;
      end loop;

      Assert (Condition => Success,
              Message   => "Not success");
      Assert (Condition => Object.Called,
              Message   => "Object not called");

      Object.Called := False;
      D.Unregister_Signal_Handler
        (Iface       => "dbus.ada.msg_dispatcher",
         Signal_Name => "event",
         Handler     => Object'Unchecked_Access);

      Connection.Send_Signal
        (Connection  => Conn,
         Object_Name => +"/",
         Iface       => "dbus.ada.msg_dispatcher",
         Name        => "event",
         Args        => +True);

      for I in 1 .. 10 loop
         D.Peek (Conn    => Conn,
                 Success => Success);
         exit when not Success or Object.Called;
         delay 0.1;
      end loop;

      Assert (Condition => Success,
              Message   => "Not success");
      Assert (Condition => not Object.Called,
              Message   => "Object called");
   end OO_Dispatcher;

   -------------------------------------------------------------------------

   procedure Proc_Dispatcher
   is
      Handled : Boolean := False;

      ----------------------------------------------------------------------

      procedure Handle (In_Msg : D_Bus.Messages.Message_Type);
      procedure Handle (In_Msg : D_Bus.Messages.Message_Type)
      is
         pragma Unreferenced (In_Msg);
      begin
         Handled := True;
      end Handle;

      use D_Bus.Arguments;
      use D_Bus.Arguments.Basic;
      use type D_Bus.Types.Obj_Path;

      Success : Boolean;
      D       : Message_Dispatcher.Proc.Proc_Dispatcher_Type;
      Dummy   : Argument_List_Type;
      Conn    : constant Connection.Connection_Type
        := Connection.Connect (Bus => Bus_Session);
   begin
      Connection.Add_Match
        (Connection => Conn,
         Rule       => "type='signal'");
      D.Register_Signal_Handler
        (Iface       => "dbus.ada.msg_dispatcher",
         Signal_Name => "event",
         Handler     => Handle'Unrestricted_Access);

      Connection.Send_Signal
        (Connection  => Conn,
         Object_Name => +"/",
         Iface       => "dbus.ada.msg_dispatcher",
         Name        => "event",
         Args        => +True);

      for I in 1 .. 10 loop
         D.Peek (Conn    => Conn,
                 Success => Success);
         exit when not Success or Handled;
         delay 0.1;
      end loop;

      Assert (Condition => Success,
              Message   => "Not success");
      Assert (Condition => Handled,
              Message   => "Not handled");

      Handled := False;
      D.Unregister_Signal_Handler
        (Iface       => "dbus.ada.msg_dispatcher",
         Signal_Name => "event",
         Handler     => Handle'Unrestricted_Access);

      Connection.Send_Signal
        (Connection  => Conn,
         Object_Name => +"/",
         Iface       => "dbus.ada.msg_dispatcher",
         Name        => "event",
         Args        => +True);

      for I in 1 .. 10 loop
         D.Peek (Conn    => Conn,
                 Success => Success);
         exit when not Success or Handled;
         delay 0.1;
      end loop;

      Assert (Condition => Success,
              Message   => "Not success");
      Assert (Condition => not Handled,
              Message   => "Handled");
   end Proc_Dispatcher;

end Message_Dispatcher_Tests;

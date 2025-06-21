--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2019  Tobias Brunner <tbrunner@hsr.ch>
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

with D_Bus.Arguments.Basic;
with D_Bus.Messagebox;

package body D_Bus.Message_Dispatcher
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Peek
     (Dispatcher   :     Dispatcher_Type'Class;
      Conn         :     Connection.Connection_Type;
      Timeout_Msec :     Integer := 100;
      Success      : out Boolean)
   is
      use D_Bus.Arguments.Basic;
      use type D_Bus.Messages.Message_Variant;

      Handled         : Boolean;
      In_Msg, Out_Msg : Messages.Message_Type;
   begin
      Success := Connection.Read_Write
        (Connection   => Conn,
         Timeout_Msec => Timeout_Msec);

      Connection.Pop_Message (Connection => Conn,
                              Message    => In_Msg);

      if not Messages.Is_Null (Msg => In_Msg) then
         if Messages.Is_Method_Call
           (Msg    => In_Msg,
            Iface  => "org.freedesktop.DBus.Introspectable",
            Method => "Introspect")
         then
            Out_Msg := Messages.New_Method_Return
              (Method_Call => In_Msg);
            Messages.Add_Arguments
              (Msg  => Out_Msg,
               Args => +To_String (Dispatcher.Introspect));
            D_Bus.Messagebox.Enqueue (M => Out_Msg);
            return;
         end if;

         case Messages.Get_Type (Msg => In_Msg)
         is
            when Messages.Method_Call =>
               Dispatcher.Handle_Message
                 (In_Msg  => In_Msg,
                  Signal  => False,
                  Success => Handled);
            when Messages.Signal =>
               Dispatcher.Handle_Message
                 (In_Msg  => In_Msg,
                  Signal  => True,
                  Success => Handled);
            when others => null;
         end case;

         if not Handled then
            if Messages.Get_Type (Msg => In_Msg) = Messages.Method_Call
            then
               Out_Msg := Messages.New_Error
                 (Reply_To      => In_Msg,
                  Error_Name    => "org.freedesktop.resolve1.UnknownMethod",
                  Error_Message => "Unknown method call");
               D_Bus.Messagebox.Enqueue (M => Out_Msg);
            end if;
         end if;

         D_Bus.Messages.Unref (Msg => In_Msg);
      end if;
   end Peek;

end D_Bus.Message_Dispatcher;

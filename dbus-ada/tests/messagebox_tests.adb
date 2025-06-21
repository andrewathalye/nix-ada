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

with dbus_message_h;

with D_Bus.Messages;
with D_Bus.Messagebox;

package body Messagebox_Tests is

   use Ahven;
   use D_Bus;

   -------------------------------------------------------------------------

   procedure Enqueue_Consume_Messages
   is
      use type D_Bus.Messages.Message_Type;

      Box  : Messagebox.Msg_Box_Type;
      M    : Messages.Message_Type;
      Thin : constant access dbus_message_h.DBusMessage
        := dbus_message_h.dbus_message_new (message_type => 2);
      Ref  : Messages.Message_Type := Messages.To_Thick
        (Thin => Thin);
   begin
      Assert (Condition => not Messages.Is_Null (Msg => Ref),
              Message   => "Ref message nil");

      Box.Consume (M => M);
      Assert (Condition => M = Messages.Null_Message,
              Message   => "Null_Message expected (1)");

      Box.Enqueue (M => Ref);
      Box.Enqueue (M => Ref);

      Box.Consume (M => M);
      Assert (Condition => M = Ref,
              Message   => "Ref message expected (1)");
      Box.Consume (M => M);
      Assert (Condition => M = Ref,
              Message   => "Ref message expected (2)");
      Box.Consume (M => M);
      Assert (Condition => M = Messages.Null_Message,
              Message   => "Null_Message expected (2)");

      Messages.Unref (Msg => Ref);
   end Enqueue_Consume_Messages;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Messagebox");
      T.Add_Test_Routine
        (Routine => Enqueue_Consume_Messages'Access,
         Name    => "Enqueue/consume messages");
   end Initialize;

end Messagebox_Tests;

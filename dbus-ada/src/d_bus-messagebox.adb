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

package body D_Bus.Messagebox
is

   -------------------------------------------------------------------------

   procedure Consume (M : out Messages.Message_Type)
   is
   begin
      Msg_Box.Consume (M => M);
   end Consume;

   -------------------------------------------------------------------------

   procedure Enqueue (M : Messages.Message_Type)
   is
   begin
      Msg_Box.Enqueue (M => M);
   end Enqueue;

   -------------------------------------------------------------------------

   protected body Msg_Box_Type is

      ----------------------------------------------------------------------

      procedure Consume (M : out Messages.Message_Type)
      is
         Pos : ML.Cursor := Data.First;
      begin
         M := Messages.Null_Message;

         if ML.Has_Element (Position => Pos) then
            M := ML.Element (Position => Pos);
            Data.Delete (Position => Pos);
         end if;
      end Consume;

      ----------------------------------------------------------------------

      procedure Enqueue (M : Messages.Message_Type)
      is
      begin
         Data.Append (New_Item => M);
      end Enqueue;

   end Msg_Box_Type;

end D_Bus.Messagebox;

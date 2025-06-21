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

with Ada.Text_IO;

with D_Bus.Arguments;

package body D_Bus.Callbacks is

   -------------------------------------------------------------------------

   procedure Print (Msg : Messages.Message_Type)
   is
      use D_Bus.Messages;
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("--");
      Ada.Text_IO.Put_Line ("Type   : " & Get_Type (Msg)'Img);
      Ada.Text_IO.Put_Line ("Serial :"  & Get_Serial (Msg)'Img);
      Ada.Text_IO.Put_Line ("Sender : " & Get_Sender (Msg));
      Ada.Text_IO.Put_Line ("Dest   : " & Get_Destination (Msg));
      Ada.Text_IO.Put_Line ("Path   : " & Get_Path (Msg));
      Ada.Text_IO.Put_Line ("Iface  : " & Get_Interface (Msg));
      Ada.Text_IO.Put_Line ("Member : " & Get_Member (Msg));
      Ada.Text_IO.Put_Line ("Args   : ");

      declare
         procedure Print (Argument : Arguments.Argument_Type'Class);
         --  Print out argument code and string representation.

         procedure Print (Argument : Arguments.Argument_Type'Class)
         is
         begin
            Ada.Text_IO.Set_Col (To => 1);
            Ada.Text_IO.Put ("(" & Argument.Get_Code'Img & " )");
            Ada.Text_IO.Set_Col (To => 10);
            Ada.Text_IO.Put_Line (Argument.To_String);
         end Print;
      begin
         Messages.Get_Arguments (Msg).Iterate
           (Process => Print'Access);
      end;
   end Print;

end D_Bus.Callbacks;

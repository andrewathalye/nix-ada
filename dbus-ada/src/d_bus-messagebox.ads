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

with Ada.Containers.Doubly_Linked_Lists;

with D_Bus.Messages;

package D_Bus.Messagebox
is

   --  Enqueue a message in the messagebox pre-instantiated in this package.
   procedure Enqueue (M : Messages.Message_Type);

   --  Get the next queued message for further processing from the messagebox
   --  instance provided in this package. Returns Null_Message if none is
   --  available.
   procedure Consume (M : out Messages.Message_Type);

   package ML is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Messages.Message_Type,
      "="          => Messages."=");

   type Msg_List is new ML.List with null record;

   --  Protected message box type.
   protected type Msg_Box_Type
   is
      --  Add a message to the box.
      procedure Enqueue (M : Messages.Message_Type);

      --  Remove the first message from the messagebox, or return Null_Message.
      procedure Consume (M : out Messages.Message_Type);

   private

      Data : Msg_List;

   end Msg_Box_Type;

private

   --  Ready-to-use instance of the message box.
   Msg_Box : Msg_Box_Type;

end D_Bus.Messagebox;

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

with Ada.Tags.Generic_Dispatching_Constructor;

with dbus_message_h;

package D_Bus.Marshaling is

   type Object is interface;
   --  Serializable object interface. Objects which are serialized to or
   --  deserialized from D-Bus messages must implement this interface.

   procedure Serialize
     (Obj   : Object;
      D_Msg : not null access dbus_message_h.DBusMessageIter) is abstract;
   --  Serialize given Ada object to low-level D-Bus message iterator.

   function Deserialize
     (D_Msg : not null access dbus_message_h.DBusMessageIter)
      return Object
      is abstract;
   --  Create new Ada object from low-level D-Bus message iterator.

   function Make_Object is new Ada.Tags.Generic_Dispatching_Constructor
     (T           => Object,
      Parameters  => dbus_message_h.DBusMessageIter,
      Constructor => Deserialize);
   --  Object dispatching function.

end D_Bus.Marshaling;

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

with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;

with D_Bus.Messages;

package D_Bus.Service is

   type Method_Handle is access procedure
     (Request :     Messages.Message_Type;
      Reply   : out Messages.Message_Type);
   --  Method handler.

   type Object is abstract tagged private;
   --  D-Bus service object. Extend this type to provide your own services on
   --  the bus.

   procedure Initialize (Obj : in out Object) is abstract;
   --  Initialize a service object.

   procedure Register
     (Obj    : in out Object;
      Name   :        String;
      Method :        Method_Handle);
   --  Register a service method with given name.

   procedure Call
     (Obj     :     Object;
      Name    :     String;
      Request :     Messages.Message_Type;
      Reply   : out Messages.Message_Type);
   --  Call method with given name.

   Duplicate_Method : exception;
   Unknown_Method   : exception;

private

   use Ada.Strings.Unbounded;

   package Methods_Map is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Method_Handle);

   package MM renames Methods_Map;

   type Object is abstract tagged record
      Methods : MM.Map;
   end record;

end D_Bus.Service;

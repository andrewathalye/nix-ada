--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2012  Reto Buerki <reet@codelabs.ch>
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

package D_Bus.Types is

   type Obj_Path is private;
   --  D-Bus object path.

   function "+" (Path : String) return Obj_Path;
   --  Create a new object path from given string. If the path is not valid, an
   --  exception is raised.

   function To_String (Path : Obj_Path) return String;
   --  Convert given object path to string.

   function Is_Valid_Obj_Path (Path : String) return Boolean;
   --  Return True if path given as string is a valid D-Bus object path.

   type Signature is private;
   --  D-Bus signature

   function "+" (Sig : String) return Signature;
   --  Convert `Sig` to `Signature`.

   function To_String (Sig : Signature) return String;
   --  Convert `Sig` to `String`.

   function Is_Valid_Signature (Sig : String) return Boolean;
   --  Return True if `Sig` would be a valid `Signature`

private
   Impl_Null_Obj_Path : constant Ada.Strings.Unbounded.Unbounded_String :=
      Ada.Strings.Unbounded.To_Unbounded_String ("/");

   type Obj_Path is record
      Value : Ada.Strings.Unbounded.Unbounded_String := Impl_Null_Obj_Path;
   end record;

   type Signature is record
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end D_Bus.Types;

--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2012  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2024  Andrew Athalye
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

with GNAT.Regpat;

with Interfaces.C.Strings;

with dbus_signature_h;
with dbus_types_h;
with dbus_errors_h;

package body D_Bus.Types is

   -------------------------------------------------------------------------

   function "+" (Path : String) return Obj_Path
   is
   begin
      if not Is_Valid_Obj_Path (Path => Path) then
         raise D_Bus_Error with "Invalid D-Bus object path: '" & Path & "'";
      end if;

      return P : Obj_Path do
         P.Value := Ada.Strings.Unbounded.To_Unbounded_String (Path);
      end return;
   end "+";

   -------------------------------------------------------------------------

   function "+" (Sig : String) return Signature
   is
   begin
      if not Is_Valid_Signature (Sig) then
         raise D_Bus_Error with "Invalid D-Bus signature: '" & Sig & "'";
      end if;

      return S : Signature do
         S.Value := Ada.Strings.Unbounded.To_Unbounded_String (Sig);
      end return;
   end "+";

   -------------------------------------------------------------------------

   function Is_Valid_Obj_Path (Path : String) return Boolean
   is
      use type GNAT.Regpat.Match_Location;

      Path_Regex : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile
        (Expression => "^/([-_a-zA-Z0-9]+(/[-_a-zA-Z0-9]+)*)?$");
      Matches    : GNAT.Regpat.Match_Array (0 .. 1);
   begin
      GNAT.Regpat.Match (Self    => Path_Regex,
                         Data    => Path,
                         Matches => Matches);
      return Matches (0) /= GNAT.Regpat.No_Match;
   end Is_Valid_Obj_Path;

   -------------------------------------------------------------------------

   function Is_Valid_Signature (Sig : String) return Boolean
   is
      use type dbus_types_h.dbus_bool_t;

      Sig_C : aliased Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (Sig);

      D_Err : aliased dbus_errors_h.DBusError;
      D_Res : dbus_types_h.dbus_bool_t;
   begin
      D_Res := dbus_signature_h.dbus_signature_validate (Sig_C, D_Err'Access);
      Interfaces.C.Strings.Free (Sig_C);

      --  Free memory used for the error
      if dbus_errors_h.dbus_error_is_set (D_Err'Access) = 1 then
         dbus_errors_h.dbus_error_free (D_Err'Access);
      end if;

      return D_Res = 1;
   end Is_Valid_Signature;

   -------------------------------------------------------------------------

   function To_String (Path : Obj_Path) return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Source => Path.Value);
   end To_String;

   -------------------------------------------------------------------------

   function To_String (Sig : Signature) return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Sig.Value);
   end To_String;

   -------------------------------------------------------------------------
end D_Bus.Types;

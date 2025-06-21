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

package body D_Bus.Service is

   -------------------------------------------------------------------------

   procedure Call
     (Obj     :     Object;
      Name    :     String;
      Request :     Messages.Message_Type;
      Reply   : out Messages.Message_Type)
   is
      Method : Method_Handle;
   begin
      Method := Obj.Methods.Element (Key => To_Unbounded_String (Name));

      Method (Request => Request,
              Reply   => Reply);

   exception
      when Constraint_Error =>
         raise Unknown_Method with "Method '" & Name & "' is not registered";
   end Call;

   -------------------------------------------------------------------------

   procedure Register
     (Obj    : in out Object;
      Name   :        String;
      Method :        Method_Handle)
   is
   begin
      Obj.Methods.Insert (Key      => To_Unbounded_String (Name),
                          New_Item => Method);

   exception
      when Constraint_Error =>
         raise Duplicate_Method with "Method '" & Name & "' is already"
           & " registered";
   end Register;

end D_Bus.Service;

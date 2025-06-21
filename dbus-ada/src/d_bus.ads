--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2011, 2012  Reto Buerki <reet@codelabs.ch>
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

with Interfaces;

package D_Bus is

   pragma Pure;

   type Bus_Type is
     (Bus_Session,
      Bus_System,
      Bus_Starter);
   --  D-Bus bus types.

   type Byte is mod 2 ** 8;
   for Byte'Size use 8;
   --  One byte.

   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;
   --  16 bit unsigned number.

   type Signed_16 is range -2 ** 15 .. 2 ** 15 - 1;
   for Signed_16'Size use 16;
   --  16 bit signed number.

   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;
   --  32 bit unsigned number.

   type Signed_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for Signed_32'Size use 32;
   --  32 bit signed number.

   type Unsigned_64 is mod 2 ** 64;
   for Unsigned_64'Size use 64;
   --  64 bit unsigned number.

   type Signed_64 is range -2 ** 63 .. 2 ** 63 - 1;
   for Signed_64'Size use 64;
   --  64 bit signed number.

   type Double is new Interfaces.IEEE_Float_64;
   for Double'Size use 64;
   --  Double-precision real number
   --  Definition would be implementation-dependent

   type File_Descriptor is mod 2 ** 32;
   for File_Descriptor'Size use 32;
   --  UNIX File Descriptor

   D_Bus_Error : exception;

end D_Bus;

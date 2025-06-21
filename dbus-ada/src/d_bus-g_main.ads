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

private with System;

package D_Bus.G_Main is
   type Main_Context is private;
   --  A Glib MainContext type. Each thread has a default main context,
   --  but it is also possible to make additional ones.

   function Default_Context return Main_Context;
   --  Get the default context for the current thread.
   --  It is valid _only_ on the current thread.

   procedure Start (Context : Main_Context := Default_Context);
   --  Run the main loop associated with `Context`.

   procedure Quit;
   --  Quit the most deeply-nested main loop running on the current
   --  thread.

   function Create return Main_Context;
   pragma Import
     (C, Create, "g_main_context_new");
   --  Return a new main context.

   procedure Destroy (Context : in out Main_Context);
   --  Release all resources held by the main context.
   --  `Context` is invalid after calling `Destroy`
private
   type Main_Context is new System.Address;
end D_Bus.G_Main;

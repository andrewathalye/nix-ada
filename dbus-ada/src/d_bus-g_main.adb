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

with Ada.Task_Attributes;

with Interfaces.C;

package body D_Bus.G_Main is
   ---------------
   -- Constants --
   ---------------
   Null_Context : constant Main_Context := Main_Context (System.Null_Address);

   ------------------------
   -- Imported from Glib --
   ------------------------
   function g_main_context_acquire
     (Context : Main_Context) return Interfaces.C.int;
   pragma Import (C, g_main_context_acquire);

   procedure g_main_context_unref (Context : Main_Context);
   pragma Import (C, g_main_context_unref);

   procedure g_main_context_release (Context : Main_Context);
   pragma Import (C, g_main_context_release);

   function g_main_context_iteration
     (Context : Main_Context;
      Blocking : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, g_main_context_iteration);

   ------------------
   -- Thread-Local --
   ------------------
   package Thread_Local_Quit is new Ada.Task_Attributes (Boolean, False);
   package Thread_Local_Context is new Ada.Task_Attributes
     (Main_Context, Null_Context);

   ---------------------
   -- Default_Context --
   ---------------------
   function Default_Context return Main_Context is
   begin
      if Thread_Local_Context.Value = Null_Context then
         Thread_Local_Context.Set_Value (Create);
      end if;

      return Thread_Local_Context.Value;
   end Default_Context;

   -------------
   -- Destroy --
   -------------
   procedure Destroy (Context : in out Main_Context) is
   begin
      g_main_context_unref (Context);

      Context := Null_Context;
   end Destroy;

   ----------
   -- Quit --
   ----------
   procedure Quit
   is
   begin
      Thread_Local_Quit.Set_Value (True);
   end Quit;

   -----------
   -- Start --
   -----------
   procedure Start (Context : Main_Context := Default_Context)
   is
      use type Interfaces.C.int;

      Result, Discard : Interfaces.C.int;
   begin
      --  Sanity checks
      if Context = Null_Context then
         raise D_Bus_Error with "Context was null";
      end if;

      --  Lock the context to this thread only
      Result := g_main_context_acquire (Context);

      if Result = 1 then
         while not Thread_Local_Quit.Value loop
            Discard := g_main_context_iteration (Context, 1);
         end loop;

         Thread_Local_Quit.Set_Value (False);
      else
         raise D_Bus_Error with "Could not claim ownership of main context.";
      end if;

      g_main_context_release (Context);
   end Start;
end D_Bus.G_Main;

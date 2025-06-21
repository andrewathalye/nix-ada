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

package body D_Bus.Message_Dispatcher.Object
is

   use Ada.Strings.Unbounded;

   procedure Unregister_Handler
     (List    : in out List_Of_Handlers_Pkg.List;
      Iface   :        String;
      Name    :        String;
      Handler :        Handler_Access);

   -------------------------------------------------------------------------

   function Create (Introspect : String) return OO_Dispatcher_Type
   is
   begin
      return OO_Dispatcher_Type'
        (Introspect => To_Unbounded_String (Introspect),
         others     => <>);
   end Create;

   -------------------------------------------------------------------------

   procedure Handle_Message
     (Dispatcher :     OO_Dispatcher_Type;
      In_Msg     :     D_Bus.Messages.Message_Type;
      Signal     :     Boolean;
      Success    : out Boolean)
   is
      List : constant List_Of_Handlers_Pkg.List
        := (if Signal then Dispatcher.Signal_Handlers else
               Dispatcher.Method_Handlers);
      Check : constant Msg_Dst_Check_Func
        := (if Signal then Messages.Is_Signal'Access else
               Messages.Is_Method_Call'Access);
   begin
      Success := False;

      for H of List loop
         if Check (In_Msg, To_String (H.Iface), To_String (H.Method))
         then
            Success := True;
            H.Handler.Dispatch (In_Msg => In_Msg);
         end if;
      end loop;
   end Handle_Message;

   -------------------------------------------------------------------------

   procedure Register_Method_Handler
     (Dispatcher : in out OO_Dispatcher_Type;
      Iface      :        String;
      Method     :        String;
      Handler    :        Handler_Access)
   is
   begin
      Dispatcher.Method_Handlers.Append
        (New_Item => (Iface   => To_Unbounded_String (Iface),
                      Method  => To_Unbounded_String (Method),
                      Handler => Handler));
   end Register_Method_Handler;

   -------------------------------------------------------------------------

   procedure Register_Signal_Handler
     (Dispatcher  : in out OO_Dispatcher_Type;
      Iface       :        String;
      Signal_Name :        String;
      Handler     :        Handler_Access)
   is
   begin
      Dispatcher.Signal_Handlers.Append
        (New_Item => (Iface   => To_Unbounded_String (Iface),
                      Method  => To_Unbounded_String (Signal_Name),
                      Handler => Handler));
   end Register_Signal_Handler;

   -------------------------------------------------------------------------

   procedure Unregister_Handler
     (List    : in out List_Of_Handlers_Pkg.List;
      Iface   :        String;
      Name    :        String;
      Handler :        Handler_Access)
   is
      use List_Of_Handlers_Pkg;

      Elem : constant Registration_Type
         := (Iface   => To_Unbounded_String (Iface),
             Method  => To_Unbounded_String (Name),
             Handler => Handler);
      Pos  : Cursor := Find (List, Elem);
   begin
      while Has_Element (Pos) loop
         Delete (List, Pos);
         Pos := Find (List, Elem);
      end loop;
   end Unregister_Handler;

   -------------------------------------------------------------------------

   procedure Unregister_Method_Handler
     (Dispatcher : in out OO_Dispatcher_Type;
      Iface      :        String;
      Method     :        String;
      Handler    :        Handler_Access)
   is
   begin
      Unregister_Handler
        (List    => Dispatcher.Method_Handlers,
         Iface   => Iface,
         Name    => Method,
         Handler => Handler);
   end Unregister_Method_Handler;

   -------------------------------------------------------------------------

   procedure Unregister_Signal_Handler
     (Dispatcher  : in out OO_Dispatcher_Type;
      Iface       :        String;
      Signal_Name :        String;
      Handler     :        Handler_Access)
   is
   begin
      Unregister_Handler
        (List    => Dispatcher.Signal_Handlers,
         Iface   => Iface,
         Name    => Signal_Name,
         Handler => Handler);
   end Unregister_Signal_Handler;

end D_Bus.Message_Dispatcher.Object;

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

package D_Bus.Message_Dispatcher.Object
is

   type Handler_Type is interface;

   type Handler_Access is access all Handler_Type'Class;

   --  Dispatch procedure to handle incoming messages. The In_Msg must not be
   --  unref'd, this is done by the Peek procedure. Use the Messages.Ref
   --  function to store an In_Msg for further use.
   procedure Dispatch
     (H      : in out Handler_Type;
      In_Msg :        D_Bus.Messages.Message_Type)
   is abstract;

   type OO_Dispatcher_Type is new Dispatcher_Type with private;

   --  Create new object-oriented message dispatcher.
   --  The introspection data is handed out to clients if the 'Introspect'
   --  method is called on the 'org.freedesktop.DBus.Introspectable' interface.
   function Create (Introspect : String) return OO_Dispatcher_Type;

   --  Register dispatcher procedure for given Interface/Method call. Multiple
   --  registrations for the same Interface/Method are possible.
   procedure Register_Method_Handler
     (Dispatcher : in out OO_Dispatcher_Type;
      Iface      :        String;
      Method     :        String;
      Handler    :        Handler_Access);

   --  Register dispatcher procedure for given Interface/Signal. Multiple
   --  registrations for the same Interface/Signal are possible.
   procedure Register_Signal_Handler
     (Dispatcher  : in out OO_Dispatcher_Type;
      Iface       :        String;
      Signal_Name :        String;
      Handler     :        Handler_Access);

   --  Unregister dispatcher procedure from given Interface/Method call.
   procedure Unregister_Method_Handler
     (Dispatcher : in out OO_Dispatcher_Type;
      Iface      :        String;
      Method     :        String;
      Handler    :        Handler_Access);

   --  Unregister dispatcher procedure from given Interface/Signal.
   procedure Unregister_Signal_Handler
     (Dispatcher  : in out OO_Dispatcher_Type;
      Iface       :        String;
      Signal_Name :        String;
      Handler     :        Handler_Access);

   --  Handle method call or signal designated by Msg.In_Msg. Returns True if a
   --  handler is registered for the requested method call or signal, False if
   --  not.
   overriding
   procedure Handle_Message
     (Dispatcher :     OO_Dispatcher_Type;
      In_Msg     :     D_Bus.Messages.Message_Type;
      Signal     :     Boolean;
      Success    : out Boolean);

private

   type Registration_Type is new Base_Registration_Type with record
      Handler : Handler_Access;
   end record;

   package List_Of_Handlers_Pkg is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Registration_Type);

   type OO_Dispatcher_Type is new Dispatcher_Type with record
      Method_Handlers : List_Of_Handlers_Pkg.List;
      Signal_Handlers : List_Of_Handlers_Pkg.List;
   end record;

end D_Bus.Message_Dispatcher.Object;

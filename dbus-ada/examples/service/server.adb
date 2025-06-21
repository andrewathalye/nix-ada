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

with D_Bus.Messages;
with D_Bus.Arguments.Basic;

package body Server is

   use D_Bus;

   procedure Get_Name
     (Request :     Messages.Message_Type;
      Reply   : out Messages.Message_Type);
   --  Get server name.

   procedure Introspect
     (Request :     Messages.Message_Type;
      Reply   : out Messages.Message_Type);
   --  Return introspection data of this service.

   Intro_Spect : constant String :=
   "<!DOCTYPE node PUBLIC ""-//freedesktop//DTD D-BUS Object "                &
   "Introspection 1.0//EN"""                                       & ASCII.LF &
   """http://www.freedesktop.org/standards/dbus/"                             &
   "1.0/introspect.dtd"">"                                         & ASCII.LF &
   "<node>"                                                        & ASCII.LF &
   "  <interface name=""org.freedesktop.DBus.Introspectable"">"    & ASCII.LF &
   "    <method name=""Introspect"">"                              & ASCII.LF &
   "      <arg name=""introspection_xml"" direction=""out"" type=""s""/>"
     & ASCII.LF &
   "    </method>"                                                 & ASCII.LF &
   "  </interface>"                                                & ASCII.LF &
   "  <interface name=""dbus.ada.service"">"                       & ASCII.LF &
   "    <method name=""GetName"">"                                 & ASCII.LF &
   "      <arg name=""name"" direction=""out"" type=""s""/>"       & ASCII.LF &
   "    </method>"                                                 & ASCII.LF &
   "  </interface>"                                                & ASCII.LF &
   "</node>";
   --  Introspection data.

   -------------------------------------------------------------------------

   procedure Get_Name
     (Request :     Messages.Message_Type;
      Reply   : out Messages.Message_Type)
   is
      use D_Bus.Arguments.Basic;
   begin
      Reply := Messages.New_Method_Return (Method_Call => Request);
      Messages.Add_Arguments (Msg  => Reply,
                              Args => +"Lovelace");
   end Get_Name;

   -------------------------------------------------------------------------

   procedure Initialize (Server : in out Server_Type)
   is
   begin
      Server.Register (Name   => "Introspect",
                       Method => Introspect'Access);
      Server.Register (Name   => "GetName",
                       Method => Get_Name'Access);
   end Initialize;

   -------------------------------------------------------------------------

   procedure Introspect
     (Request :     Messages.Message_Type;
      Reply   : out Messages.Message_Type)
   is
      use D_Bus.Arguments.Basic;
   begin
      Reply := Messages.New_Method_Return (Method_Call => Request);
      Messages.Add_Arguments (Msg  => Reply,
                              Args => +Intro_Spect);
   end Introspect;

end Server;

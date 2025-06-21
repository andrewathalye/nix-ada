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

with Ahven;

with D_Bus.Service;
with D_Bus.Messages;

package body Service_Tests is

   use Ahven;
   use D_Bus;
   use D_Bus.Service;

   type Test_Object is new Object with null record;

   procedure Initialize (Obj : in out Test_Object);
   --  Initialize test object.

   procedure Test_Method
     (Request :     Messages.Message_Type;
      Reply   : out Messages.Message_Type);
   --  Method used for testing the service object.

   Called_Counter : Natural := 0;

   -------------------------------------------------------------------------

   procedure Duplicate_Methods
   is
      Obj : Test_Object;
   begin
      Obj.Register (Name   => "TestMethod",
                    Method => Test_Method'Access);

      begin
         Obj.Register (Name   => "TestMethod",
                       Method => Test_Method'Access);
         Fail (Message => "Exception expected");

      exception
         when Duplicate_Method => null;
      end;
   end Duplicate_Methods;

   -------------------------------------------------------------------------

   procedure Initialize (Obj : in out Test_Object)
   is
   begin
      Obj.Register (Name   => "TestMethod",
                    Method => Test_Method'Access);
   end Initialize;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase) is
   begin
      T.Set_Name (Name => "Service objects");
      T.Add_Test_Routine
        (Routine => Register_Method'Access,
         Name    => "Register service method");
      T.Add_Test_Routine
        (Routine => Unknown_Method'Access,
         Name    => "Call unknown method");
      T.Add_Test_Routine
        (Routine => Duplicate_Methods'Access,
         Name    => "Register same method twice");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Register_Method
   is
      Obj : Test_Object;
      Req : Messages.Message_Type;
      Rep : Messages.Message_Type;

      pragma Unreferenced (Rep);
   begin
      Obj.Initialize;
      Obj.Call (Name    => "TestMethod",
                Request => Req,
                Reply   => Rep);

      Assert (Condition => Called_Counter = 1,
              Message   => "Method not called");
   end Register_Method;

   -------------------------------------------------------------------------

   procedure Test_Method
     (Request :     Messages.Message_Type;
      Reply   : out Messages.Message_Type)
   is
      pragma Unreferenced (Request, Reply);
   begin
      Called_Counter := Called_Counter + 1;
   end Test_Method;

   -------------------------------------------------------------------------

   procedure Unknown_Method
   is
      Obj : Test_Object;
      Req : Messages.Message_Type;
      Rep : Messages.Message_Type;

      pragma Unreferenced (Rep);
   begin
      Obj.Call (Name    => "Unknown",
                Request => Req,
                Reply   => Rep);

   exception
      when Service.Unknown_Method => null;
   end Unknown_Method;

end Service_Tests;

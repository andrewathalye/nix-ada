--
--  D_Bus/Ada - An Ada binding to D-Bus
--
--  Copyright (C) 2011, 2012  Reto Buerki <reet@codelabs.ch>
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

with Ahven.Text_Runner;
with Ahven.Framework;

with Config;

with Arguments_Tests;
with Arguments_Basic_Tests;
with Arguments_Container_Tests;
with Service_Tests;
with Types_Tests;
with Messagebox_Tests;
with Message_Dispatcher_Tests;

procedure Runner is
   S : constant Ahven.Framework.Test_Suite_Access :=
     Ahven.Framework.Create_Suite (Suite_Name => "dbus-ada tests");
begin
   Config.Init;

   Ahven.Framework.Add_Test
     (Suite => S.all,
      T     => new Arguments_Tests.Testcase);
   Ahven.Framework.Add_Test
     (Suite => S.all,
      T     => new Arguments_Basic_Tests.Testcase);
   Ahven.Framework.Add_Test
     (Suite => S.all,
      T     => new Arguments_Container_Tests.Testcase);
   Ahven.Framework.Add_Test
     (Suite => S.all,
      T     => new Service_Tests.Testcase);
   Ahven.Framework.Add_Test
     (Suite => S.all,
      T     => new Types_Tests.Testcase);
   Ahven.Framework.Add_Test
     (Suite => S.all,
      T     => new Messagebox_Tests.Testcase);
   Ahven.Framework.Add_Test
     (Suite => S.all,
      T     => new Message_Dispatcher_Tests.Testcase);

   Ahven.Text_Runner.Run (Suite => S);
   Ahven.Framework.Release_Suite (T => S);
end Runner;

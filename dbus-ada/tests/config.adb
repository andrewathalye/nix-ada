with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Unbounded;

with Utils;

pragma Elaborate_All (Utils);

package body Config is

   use Ada.Strings.Unbounded;

   Addr_File   : constant String := "./dbusada.addr";
   Remote_Addr : Unbounded_String;

   -------------------------------------------------------------------------

   procedure Init
   is
      Counter : Natural := 0;
   begin
      loop
         exit when Ada.Directories.Exists (Name => Addr_File);
         if Counter > 400 then
            raise Config_Error with "D-Bus test server not available";
         end if;

         delay 0.01;
         Counter := Counter + 1;
      end loop;

      Remote_Addr := To_Unbounded_String
        (Utils.Read_Line_From_File (Filename => Addr_File));
      Ada.Text_IO.Put_Line ("* Using test server at " & Service_Addr);
   end Init;

   -------------------------------------------------------------------------

   function Service_Addr return String
   is
   begin
      return To_String (Remote_Addr);
   end Service_Addr;

end Config;

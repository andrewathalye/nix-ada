with Ada.Text_IO;
with Ada.IO_Exceptions;

package body Utils is

   -------------------------------------------------------------------------

   function Read_Line_From_File (Filename : String) return String
   is
      File : Ada.Text_IO.File_Type;
   begin
      begin
         Ada.Text_IO.Open
           (File => File,
            Mode => Ada.Text_IO.In_File,
            Name => Filename,
            Form => "shared=no");

      exception
         when others =>
            raise Open_File_Error with
              "Unable to open file '" & Filename & "'";
      end;

      Read_Line :
      begin
         declare
            Line : constant String := Ada.Text_IO.Get_Line (File => File);
         begin
            Ada.Text_IO.Close (File);
            return Line;
         end;

      exception
         when Ada.IO_Exceptions.End_Error =>
            Ada.Text_IO.Close (File);
            raise IO_Error with "Unable to read line from empty "
              & "file '" & Filename & "'";
         when others =>
            Ada.Text_IO.Close (File);
            raise IO_Error with "Error reading data from file '"
              & Filename & "'";
      end Read_Line;
   end Read_Line_From_File;

end Utils;

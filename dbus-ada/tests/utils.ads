package Utils is

   function Read_Line_From_File (Filename : String) return String;
   --  Return first text line read from the file given by Filename.

   Open_File_Error, IO_Error : exception;

end Utils;

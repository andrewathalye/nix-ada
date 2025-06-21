package Config is

   procedure Init;
   --  Wait for D-Bus test server and read remote address from file.

   function Service_Addr return String;
   --  Return remote D-Bus address of the test server.

   Config_Error : exception;

end Config;

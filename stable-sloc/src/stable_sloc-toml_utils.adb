package body Stable_Sloc.TOML_Utils is

   function Get_Or_Null
     (Annot : TOML_Value; Field : String) return Unbounded_String
   is
   begin
      if Annot.Has (Field) then
         return Annot.As_Unbounded_String;
      else
         return Null_Unbounded_String;
      end if;
   end Get_Or_Null;

   function Get_Or_Default
     (Annot : TOML_Value; Field : String; Default : Boolean) return Boolean
   is
   begin
      if Annot.Has (Field) then
         return Annot.As_Boolean;
      else
         return Default;
      end if;
   end Get_Or_Default;

end Stable_Sloc.TOML_Utils;

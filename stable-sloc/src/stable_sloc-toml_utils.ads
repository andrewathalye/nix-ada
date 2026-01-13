with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with TOML; use TOML;

package Stable_Sloc.TOML_Utils is

   function Get_Or_Null
     (Annot : TOML_Value; Field : String) return Unbounded_String;

   function Get_Or_Default
     (Annot : TOML_Value; Field : String; Default : Boolean) return Boolean;

end Stable_Sloc.TOML_Utils;

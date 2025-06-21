pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with dbus_arch_deps_h;
with Interfaces.C.Strings;
with dbus_types_h;
limited with dbus_errors_h;

package dbus_signature_h is

   --  skipped anonymous struct anon_anon_7

   type DBusSignatureIter is record
      dummy1 : System.Address;  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:46
      dummy2 : System.Address;  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:47
      dummy8 : aliased dbus_arch_deps_h.dbus_uint32_t;  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:48
      dummy12 : aliased int;  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:49
      dummy17 : aliased int;  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:50
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:51

   procedure dbus_signature_iter_init (iter : access DBusSignatureIter; signature : Interfaces.C.Strings.chars_ptr)  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_signature_iter_init";

   function dbus_signature_iter_get_current_type (iter : access constant DBusSignatureIter) return int  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:58
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_signature_iter_get_current_type";

   function dbus_signature_iter_get_signature (iter : access constant DBusSignatureIter) return Interfaces.C.Strings.chars_ptr  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:61
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_signature_iter_get_signature";

   function dbus_signature_iter_get_element_type (iter : access constant DBusSignatureIter) return int  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:64
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_signature_iter_get_element_type";

   function dbus_signature_iter_next (iter : access DBusSignatureIter) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:67
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_signature_iter_next";

   procedure dbus_signature_iter_recurse (iter : access constant DBusSignatureIter; subiter : access DBusSignatureIter)  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:70
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_signature_iter_recurse";

   function dbus_signature_validate (signature : Interfaces.C.Strings.chars_ptr; error : access dbus_errors_h.DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:74
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_signature_validate";

   function dbus_signature_validate_single (signature : Interfaces.C.Strings.chars_ptr; error : access dbus_errors_h.DBusError) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:78
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_signature_validate_single";

   function dbus_type_is_valid (typecode : int) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:82
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_type_is_valid";

   function dbus_type_is_basic (typecode : int) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:85
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_type_is_basic";

   function dbus_type_is_container (typecode : int) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:87
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_type_is_container";

   function dbus_type_is_fixed (typecode : int) return dbus_types_h.dbus_bool_t  -- /usr/include/dbus-1.0/dbus/dbus-signature.h:89
   with Import => True, 
        Convention => C, 
        External_Name => "dbus_type_is_fixed";

end dbus_signature_h;

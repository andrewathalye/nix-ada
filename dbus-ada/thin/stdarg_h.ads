pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package stdarg_h is

   --  arg-macro: procedure va_start (v, l)
   --    __builtin_va_start(v,l)
   --  arg-macro: procedure va_end (v)
   --    __builtin_va_end(v)
   --  arg-macro: procedure va_arg (v, l)
   --    __builtin_va_arg(v,l)
   --  arg-macro: procedure va_copy (d, s)
   --    __builtin_va_copy(d,s)
   subtype uu_gnuc_va_list is System.Address;  -- /usr/lib/gcc/x86_64-linux-gnu/10/include/stdarg.h:40

   subtype va_list is uu_gnuc_va_list;  -- /usr/lib/gcc/x86_64-linux-gnu/10/include/stdarg.h:99

end stdarg_h;

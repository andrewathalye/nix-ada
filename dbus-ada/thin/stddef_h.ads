pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;

package stddef_h is

   --  unsupported macro: NULL __null
   --  arg-macro: procedure offsetof (TYPE, MEMBER)
   --    __builtin_offsetof (TYPE, MEMBER)
   subtype ptrdiff_t is long;  -- /usr/lib/gcc/x86_64-linux-gnu/10/include/stddef.h:143

   subtype size_t is unsigned_long;  -- /usr/lib/gcc/x86_64-linux-gnu/10/include/stddef.h:209

   --  skipped anonymous struct anon_anon_0

   type max_align_t is record
      uu_max_align_ll : aliased Long_Long_Integer;  -- /usr/lib/gcc/x86_64-linux-gnu/10/include/stddef.h:416
      uu_max_align_ld : aliased long_double;  -- /usr/lib/gcc/x86_64-linux-gnu/10/include/stddef.h:417
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/lib/gcc/x86_64-linux-gnu/10/include/stddef.h:426

   subtype nullptr_t is System.Address;  -- /usr/lib/gcc/x86_64-linux-gnu/10/include/stddef.h:433

end stddef_h;

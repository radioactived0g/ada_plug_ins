
with Interfaces.C.Strings;

package body Shared_Lib is
   
   use Interfaces;

   function dlerror return C.Strings.Chars_Ptr;
   pragma Import (C, dlerror, "dlerror");
   
   --------------------
   -- File_Extension --
   --------------------
   
   function File_Extension return String is
   begin
      return "so";
   end File_Extension;
   
   ----------
   -- Load --
   ----------

   function Load (Path : String) return Handle is
      
      function dlopen
        (Lib_Name : String; Mode : Interfaces.C.int) return Handle;
      pragma Import(C, dlopen, "dlopen");

      RTLD_LAZY : constant := 1;
      C_Path    : constant String := Path & ASCII.Nul;
      Result    : Handle;
   begin
      Result := dlopen (C_Path, RTLD_LAZY);

      if Result = Handle (System.Null_Address) then
         raise Not_Found with C.Strings.Value (dlerror);
      else
	 return Result;
      end if;
   end Load;

   ------------
   -- Unload --
   ------------

   procedure Unload (P : in out Handle) is
      function dlclose (H : Handle) return C.int;
      pragma Import(C, dlclose, "dlclose");

      Ignored : C.int;
      pragma Unreferenced (Ignored);
   begin
      Ignored := dlclose (P);
      P := Handle (System.Null_Address);
   end Unload;

end Shared_Lib;

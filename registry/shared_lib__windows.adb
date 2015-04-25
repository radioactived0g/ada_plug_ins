
with Ada.Unchecked_Conversion;

with Interfaces.C;
with System;

package body Shared_Lib is
   
   use Ada;

   subtype HINSTANCE is Handle;

   type LPCSTR is access constant Interfaces.C.char;
   pragma Convention (C, LPCSTR);
   
   type BOOL is new Interfaces.C.int;

   function LoadLibrary (lpLibFileName : LPCSTR) return HINSTANCE;
   pragma Import (Stdcall, LoadLibrary, "LoadLibraryA");

   function FreeLibrary (hModule : HINSTANCE) return BOOL;
   pragma Import (Stdcall, FreeLibrary, "FreeLibrary");
   
   function As_LPCSTR is new Unchecked_Conversion
     (Source => System.Address, Target => LPCSTR);

   --------------------
   -- File_Extension --
   --------------------
   
   function File_Extension return String is
   begin
      return "dll";
   end File_Extension;
   
   ----------
   -- Load --
   ----------

   function Load (Path : String) return Handle is
      Result     : Handle;
      Local_Path : aliased constant String := Path & ASCII.Nul;
   begin
      Result := LoadLibrary (As_LPCSTR (Local_Path'Address));
      
      if Result = Handle (System.Null_Address) then
         raise Not_Found with Path;
      end if;
      return Result;
   end Load;

   ------------
   -- Unload --
   ------------

   procedure Unload (P : in out Handle) is
      Res : constant BOOL := FreeLibrary (P);
   begin
      P := Handle (System.Null_Address);
   end Unload;

end Shared_Lib;

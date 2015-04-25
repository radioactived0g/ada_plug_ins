
with System;

package Shared_Lib is

   type Handle is new System.Address;
   
   Not_Found : exception;
   --  Raised when a plug-in is not found
   
   function Load (Path : String) return Handle;
   --  Attempts to load the plugin located at Path.
   --  Raises Not_Found with Path as the message if no plugin is
   --  located at Path.

   procedure Unload (P : in out Handle);
   --  Remove the plugin from service.  Note the actual effect is
   --  operating-system dependent.
   
   function File_Extension return String;
   --  Extension for the OS shared lib
   
end Shared_Lib;

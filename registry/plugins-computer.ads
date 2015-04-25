
package Plugins.Computer is
   
   Service_Name : constant String := "COMPUTER";
   
   type Handle is abstract new Any with null record;
   type Ref is access all Handle'Class;
   
   function Call
     (H : not null access Handle; A, B : Integer) return Integer is abstract;
   
end Plugins.Computer;


with Ada.Finalization;

with Registry;

package body Computer is
   
   use Ada;
   
   type Life_Controller is new Finalization.Limited_Controlled with null record;
   overriding procedure Initialize (LC : in out Life_Controller);
   overriding procedure Finalize (LC : in out Life_Controller);
   
   H : aliased Handle;
   
   ----------
   -- Call --
   ----------
   
   overriding function Call 
     (H : not null access Handle; A, B : Integer) return Integer is
   begin
      return A + B;
   end Call;
   
   --------------
   -- Finalize --
   --------------
   
   overriding procedure Finalize (LC : in out Life_Controller) is
   begin
      Registry.Unregister (Plugins.Computer.Service_Name);
   end Finalize;
   
   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (LC : in out Life_Controller) is
   begin
      Registry.Register (Plugins.Computer.Service_Name, H'Access);
   end Initialize;

   LC : Life_Controller;
   
end Computer;

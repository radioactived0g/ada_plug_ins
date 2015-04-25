
with Plugins.Computer;

package Computer is
   
   type Handle is new Plugins.Computer.Handle with null record;
   
   overriding function Call 
     (H : not null access Handle; A, B : Integer) return Integer;
   
end Computer;

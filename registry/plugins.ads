
package Plugins is
   
   type Any is abstract tagged null record;
   
   type Ref is access all Any'Class;
   --  A reference to any loaded plug-in
   
end Plugins;

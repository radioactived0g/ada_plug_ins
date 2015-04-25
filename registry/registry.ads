
with Plugins;

package Registry is
   
   procedure Discover_Plugins;
   --  This routine is not thread-safe
   
   procedure Register
     (Service_Name : String; Handle : not null access Plugins.Any'Class);
   
   procedure Unregister (Service_Name : String);
   
   function Get (Service_Name : String) return access Plugins.Any'Class;
   
end Registry;

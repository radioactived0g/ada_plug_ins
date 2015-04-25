
with Ada.Calendar;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Text_IO;

with Shared_Lib;

package body Registry is
   
   use Ada;
   
   --  Record plug-in name (shared library name) and corresponding OS handle

   package Plugin_Map is new Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Shared_Lib.Handle,
      Hash            => Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Shared_Lib."=");
   
   Loaded_Plugins : Plugin_Map.Map;
   
   --  Record plug-in service name and corresponding Ada reference

   package Service_Map is new Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Plugins.Ref,
      Hash            => Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Plugins."=");
   
   Loaded_Services : Service_Map.Map;

   ----------------------
   -- Discover_Plugins --
   ----------------------
   
   procedure Discover_Plugins is
      
      -----------------
      -- Plugin_Name --
      -----------------

      function Plugin_Name (Name : String) return String is
	 K : Integer := Strings.Fixed.Index (Name, "plugin_");
      begin
	 return Name (Name'First .. K -1) & Name (K + 7 .. Name'Last);
      end Plugin_Name;
      
      use Directories;
      use type Calendar.Time;
      
      S          : Search_Type;
      D          : Directory_Entry_Type;
      Only_Files : constant Filter_Type :=
                     (Ordinary_File => True, others => False);
      Any_Plugin : constant String :=
                     "libplugin_*." & Shared_Lib.File_Extension;
   begin
      Start_Search (S, "plugins/", Any_Plugin, Only_Files);

      while More_Entries (S) loop
	 Get_Next_Entry (S, D);
	 
	 declare
	    P     : Shared_Lib.Handle;
	    Name  : constant String := Simple_Name (D);
	    Fname : constant String := Full_Name (D);
	    Pname : constant String := Plugin_Name (Fname);
	 begin
	    --  Plug-in file is older than 5 seconds, we do not want to try
	    --  loading a plug-in not yet fully compiled/linked.

	    if Modification_Time (D) < Calendar.Clock - 5.0 then
	       Text_IO.Put_Line ("Plug-in " & Name);

	       if Loaded_Plugins.Contains (Pname) then
		  Text_IO.Put_Line ("... already loaded, unload now");
		  P := Loaded_Plugins.Element (Pname);
		  Shared_Lib.Unload (P);
	       end if;

	       --  Rename plug-in (remove pluging_)

	       if Exists (Pname) then
		  Delete_File (Pname);
	       end if;

	       Rename (Fname, Pname);

	       --  Load it

	       P := Shared_Lib.Load (Pname);
	       Loaded_Plugins.Include (Pname, P);
	    end if;
	 end;
      end loop;
   end Discover_Plugins;
   
   --------------
   -- Register --
   --------------
   
   procedure Register
     (Service_Name : String; Handle : not null access Plugins.Any'Class) is
   begin
      Text_IO.Put_Line ("Register : " & Service_Name);
      Loaded_Services.Include (Service_Name, Plugins.Ref (Handle));
   end Register;
   
   ----------------
   -- Unregister --
   ----------------
   
   procedure Unregister (Service_Name : String) is
   begin
      Text_IO.Put_Line ("Unregister : " & Service_Name);
      Loaded_Services.Exclude (Service_Name);
   end Unregister;
   
   ---------
   -- Get --
   ---------
   
   function Get (Service_Name : String) return access Plugins.Any'Class is
      C : constant Service_Map.Cursor := Loaded_Services.Find (Service_Name);
   begin
      if Service_Map.Has_Element (C) then
	 return Service_Map.Element (C);
      else
	 return null;
      end if;
   end Get;
   
end Registry;

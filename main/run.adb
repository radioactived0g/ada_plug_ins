
with Ada.Text_IO;

with Plugins.Computer;
with Registry;

procedure Run is
   use Ada;
   use type Plugins.Ref;
   
   H      : Plugins.Ref;
   Result : Integer;
   
begin
   loop
      Text_IO.Put_Line ("loop...");
      Registry.Discover_Plugins;
      H := Plugins.Ref (Registry.Get (Plugins.Computer.Service_Name));
      if H /= null then
	 Result := Plugins.Computer.Ref (H).Call (5, 7);
	 Text_IO.Put_Line ("Result : " & Integer'Image (Result));
      end if;
      delay 1.0;
   end loop;
end Run;

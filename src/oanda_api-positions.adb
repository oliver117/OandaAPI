package body Oanda_API.Positions is

   -------------------
   -- Open_Position --
   -------------------

   function Open_Position return Position is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Open_Position unimplemented");
      raise Program_Error with "Unimplemented function Open_Position";
      return Open_Position;
   end Open_Position;

   --------------------
   -- Open_Positions --
   --------------------

   function Open_Positions return Position_Array is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Open_Positions unimplemented");
      raise Program_Error with "Unimplemented function Open_Positions";
      return Open_Positions;
   end Open_Positions;

   --------------------
   -- Close_Position --
   --------------------

   function Close_Position
     (Instrument : Instrument_T)
      return Close_Position_Response
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Close_Position unimplemented");
      raise Program_Error with "Unimplemented function Close_Position";
      return Close_Position (Instrument);
   end Close_Position;

end Oanda_API.Positions;

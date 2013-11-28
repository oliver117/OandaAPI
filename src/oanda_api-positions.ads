

package Oanda_API.Positions is

   type Position is record
      Side : Side_T;
      Instrument : Instrument_Identifier;
      Units : Natural;
      Average_Price : Rate;
   end record;

   type Position_Array is array (Integer range <>) of Position;

   function Open_Position return Position;

   function Open_Positions return Position_Array;

   type Close_Position_Response is record
      IDs : Transaction_ID_Array;
      Instrument : Instrument_Identifier;
      Total_Units : Natural;
      Price : Rate;
   end record;

   function Close_Position (Instrument : Instrument_Identifier) return Close_Position_Response;

end Oanda_API.Positions;

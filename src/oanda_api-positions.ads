

package Oanda_API.Positions is

   type Position is record
      Side : Side_T;
      Instrument : Instrument_T;
      Units : Natural;
      Average_Price : Rate;
   end record;

   type Position_Array is array (Integer range <>) of Position;

   function Open_Position return Position;

   function Open_Positions return Position_Array;

   type Close_Position_Response is record
      IDs : ID_Array(0 .. 1);
      Instrument : Instrument_T;
      Total_Units : Natural;
      Price : Rate;
   end record;

   function Close_Position (Instrument : Instrument_T) return Close_Position_Response;

end Oanda_API.Positions;

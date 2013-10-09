--  Copyright (c) 2013 Oliver Kleinke
--
--  Permission is hereby granted, free of charge, to any person obtaining
--  a copy of this software and associated documentation files (the
--  "Software"), to deal in the Software without restriction, including
--  without limitation the rights to use, copy, modify, merge, publish,
--  distribute, sublicense, and/or sell copies of the Software, and to
--  permit persons to whom the Software is furnished to do so, subject to
--  the following conditions:
--
--  The above copyright notice and this permission notice shall be included
--  in all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
--  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
--  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
--  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
--  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
--  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Strings.Bounded;

package Oanda_API is

   -- Note: A big chunk of the API is not working / undocumented, therefore
   -- some of the functions remain unimplemented for now.

   -- general definitions
   -- constants
   No_Time : constant Ada.Calendar.Time;
   Debug : constant Boolean := False;

   -- price / rate
   type Rate is delta 0.00_00_1 range 0.0 .. 1.0e6;
   type Balance is delta 0.00_00_1 range 0.0 .. 1.0e12;

   type Pips is delta 0.1 range 0.0 .. 1.0e6;
   type Pipettes is delta 0.01 range 0.0 .. 1.0e6;

   function Pips_To_Pipettes (P : in Pips) return Pipettes is (P * 10.0);
   function Pipettes_To_Pips (P : in Pipettes) return Pips is (P / 10.0);

   type Margin_Rate_T is delta 0.001 range 0.0 .. 100.0; -- percent

   -- instrument
   package Bounded_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (
                                                                              Max => 32);

   -- TODO: private type?
   subtype Instrument_Identifier is Bounded_Strings.Bounded_String;

   Null_Instrument_Identifier : constant Instrument_Identifier;


   type Instrument_Identifier_Array is
     array (Integer range <>) of Instrument_Identifier;

   type Instrument is record
      Identifier        : Instrument_Identifier;
      Display_Name      : Bounded_Strings.Bounded_String;
      Pip               : Rate;
      Max_Trade_Units   : Positive;
      Precision         : Rate;
      Max_Trailing_Stop : Pips;
      Min_Trailing_Stop : Pips;
      Margin_Rate       : Margin_Rate_T;
   end record;

   type Instrument_Array is array (Integer range <>) of Instrument;

   function To_Identifier_Array
     (Instruments : in Instrument_Array)
      return        Instrument_Identifier_Array;

   function Get_Instruments return Instrument_Array;

   -- quote
   type Quote is record
      Instrument : Instrument_Identifier;
      Time       : Ada.Calendar.Time;
      Bid        : Rate;
      Ask        : Rate;
      Halted     : Boolean;
   end record;

   type Quote_Array is array (Integer range <>) of Quote;

   function Get_Quote (Instrument : in Instrument_Identifier) return Quote;

   function Get_Quotes
     (Instruments : in Instrument_Identifier_Array)
      return        Quote_Array;

   -- history
   type Granularity_T is (S5, S10, S15, S30, M1,
     M2, M3, M5, M10, M15, M30, H1,
     H2, H3, H4, H6, H8, H12, D,
     W,
     M);

   type Candle_Format_T is (Bid_Ask, Midpoint);

   type Candlestick (Format : Candle_Format_T := Bid_Ask) is record
      Time     : Ada.Calendar.Time;
      Volume   : Natural;
      Complete : Boolean;

      case Format is
         when Bid_Ask =>
            Open_Bid  : Rate;
            Open_Ask  : Rate;
            High_Bid  : Rate;
            High_Ask  : Rate;
            Low_Bid   : Rate;
            Low_Ask   : Rate;
            Close_Bid : Rate;
            Close_Ask : Rate;
         when Midpoint =>
            Open_Mid  : Rate;
            High_Mid  : Rate;
            Low_Mid   : Rate;
            Close_Mid : Rate;

      end case;
   end record;

   type Candlestick_Array is array (Integer range <>) of Candlestick;

   function Get_History
     (Instrument    : in Instrument_Identifier;
      Granularity   : in Granularity_T   := S5;
      Count         : in Positive        := 500;
      Start_Time    : in Ada.Calendar.Time    := No_Time;
      End_Time      : in Ada.Calendar.Time    := No_Time;
      Candle_Format : in Candle_Format_T := Bid_Ask;
      Include_First : in Boolean         := True)
      return          Candlestick_Array;

   -- account
   type Account is private;

   type Account_Array is array (Integer range <>) of Account;

   function Display_Name (Acc : in Account) return String;

   -- trade
   type Side_T is (Buy, Sell);

   type Trade_ID is new Long_Integer;

   type Trade_ID_Array is array (Integer range <>) of Trade_ID;

   Null_Trade_ID_Array : constant Trade_ID_Array;

   type Trade is record
      ID            : Trade_ID;
      Units         : Positive;
      Side          : Side_T;
      Instrument    : Instrument_Identifier;
      Time          : Ada.Calendar.Time;
      Price         : Rate;
      Stop_Loss     : Pips;
      Take_Profit   : Pips;
      Trailing_Stop : Pips;
   end record;

   type Trade_Array is array (Integer range <>) of Trade;

   -- unimplemented
   function Get_Open_Trades
     (Acc        : in Account;
      Max_ID     : in Integer := Integer'Last;
      Count      : in Positive := 500;
      Instrument : in Instrument_Identifier := Null_Instrument_Identifier;
      IDs        : in Trade_ID_Array := Null_Trade_ID_Array)
      return       Trade_Array;

   type Open_Trade_Response (Num_Closed : Integer; Num_Interest : Integer) is
      record
         Opened     : Trade_ID;
         Updated    : Trade_ID;
         Closed     : Trade_ID_Array (1 .. Num_Closed);
         Interest   : Trade_ID_Array (1 .. Num_Interest);
         Instrument : Instrument_Identifier;
         Units      : Positive;
         Price      : Rate;
         MarginUsed : Margin_Rate_T;
         Side       : Side_T;
         Time       : Ada.Calendar.Time;
      end record;

   -- unimplemented
   procedure Open_Trade
     (Acc           : in Account;
      Instrument    : in Instrument_Identifier;
      Units         : in Positive;
      Side          : in Side_T;
      Lower_Bound   : in Rate;
      Upper_Bound   : in Rate;
      Stop_Loss     : in Rate;
      Take_Profit   : in Rate;
      Trailing_Stop : in Pipettes);

   function Get_Trade (Acc : in Account; T_ID : in Trade_ID) return Trade;

   -- unimplemented
   function Modify_Trade
     (Acc           : in Account;
      T_ID          : in Trade_ID;
      Stop_Loss     : in Rate;
      Take_Profit   : in Rate;
      Trailing_Stop : in Pipettes)
      return          Trade;

   type Close_Trade_Response is record
      ID         : Trade_ID;
      Price      : Rate;
      Instrument : Instrument_Identifier;
      Profit     : Balance;
      Side       : Side_T;
      Time       : Ada.Calendar.Time;
   end record;

   -- unimplemented
   function Close_Trade
     (Acc  : in Account;
      T_ID : in Trade_ID)
      return Close_Trade_Response;

   -- error handling
   API_Error : exception;

private
   function From_RFC3339 (TStr : in String) return Ada.Calendar.Time;

   function To_RFC3339 (Time : in Ada.Calendar.Time) return String;

   Base_Url : constant String       := "http://api-sandbox.oanda.com/v1/";
   No_Time  : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year    => 1901,
                                                                  Month   => 1,
                                                                  Day     => 1,
                                                                  Seconds => 0.0);

   Null_Instrument_Identifier : constant Instrument_Identifier := Bounded_Strings.Null_Bounded_String;

   type Account is new Positive;

   Null_Trade_ID_Array : constant Trade_ID_Array := (2 .. 1 => <>);

   procedure Raise_API_Error (Code      : in String;
                              Message   : in String;
                              More_Info : in String);

end Oanda_API;

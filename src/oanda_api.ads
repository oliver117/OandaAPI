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
with Ada.Strings.Bounded;

package Oanda_API is

   -- Note: A big chunk of the API is not working / undocumented, therefore
   -- some of the functions remain unimplemented for now.

   -- time
   type Hour_Number is range 0 .. 23;
   type Minute_Number is range 0 .. 59;
   type Second_Number is delta 0.00_00_01 range 0.0 .. 60.0;

   type RFC3339_Time is record
      Year   : Ada.Calendar.Year_Number;
      Month  : Ada.Calendar.Month_Number;
      Day    : Ada.Calendar.Day_Number;
      Hour   : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
   end record;

   No_Time : constant RFC3339_Time;

   function To_RFC3339_Time (TStr : in String) return RFC3339_Time;

   function To_String (Time : in RFC3339_Time) return String;

   -- general type definitions

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
      Time       : RFC3339_Time;
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
   type Granularity_T is (
     S5,
     S10,
     S15,
     S30,
     M1,
     M2,
     M3,
     M5,
     M10,
     M15,
     M30,
     H1,
     H2,
     H3,
     H4,
     H6,
     H8,
     H12,
     D,
     W,
     M);

   type Candle_Format_T is (Midpoint, Bid_Ask);

   type Candlestick (Format : Candle_Format_T) is record
      Time     : RFC3339_Time;
      Volume   : Natural;
      Complete : Boolean;

      case Format is
         when Midpoint =>
            Open_Mid  : Rate;
            High_Mid  : Rate;
            Low_Mid   : Rate;
            Close_Mid : Rate;

         when Bid_Ask =>
            Open_Bid  : Rate;
            Open_Ask  : Rate;
            High_Bid  : Rate;
            High_Ask  : Rate;
            Low_Bid   : Rate;
            Low_Ask   : Rate;
            Close_Bid : Rate;
            Close_Ask : Rate;
      end case;

   end record;

   function Get_History
     (Instrument    : in Instrument_Identifier;
      Granularity   : in Granularity_T   := S5;
      Count         : in Positive        := 500;
      Start_Time    : in RFC3339_Time    := No_Time;
      End_Time      : in RFC3339_Time    := No_Time;
      Candle_Format : in Candle_Format_T := Bid_Ask;
      Include_First : in Boolean         := True)
      return          Candlestick;

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
      Time          : RFC3339_Time;
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
         Time       : RFC3339_Time;
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
      Time       : RFC3339_Time;
   end record;

   -- unimplemented
   function Close_Trade
     (Acc  : in Account;
      T_ID : in Trade_ID)
      return Close_Trade_Response;

   -- error handling
   API_Error : exception;

   procedure Raise_API_Error
     (Code      : in String;
      Message   : in String;
      More_Info : in String);

private
   Base_Url : constant String       := "http://api-sandbox.oanda.com/v1/";
   No_Time  : constant RFC3339_Time :=
     RFC3339_Time'
     (Year   => 1901,
      Month  => 1,
      Day    => 1,
      Hour   => 0,
      Minute => 0,
      Second => 0.0);

   Null_Instrument_Identifier : constant Instrument_Identifier := Bounded_Strings.Null_Bounded_String;

   type Account is new Positive;

   Null_Trade_ID_Array : constant Trade_ID_Array := (2 .. 1 => <>);

end Oanda_API;

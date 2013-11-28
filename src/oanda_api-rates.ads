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

with Ada.Containers.Hashed_Maps;

package Oanda_API.Rates is

   function To_Instrument_Array (Instr_Info : in Instrument_Information_Array) return Instrument_Array;

   function Get_Instruments (Acc : in Account) return Instrument_Array;

   function Get_Instrument_Information (Acc : in Account) return Instrument_Information_Array;

   -- quote
   type Quote is record
      Instrument : Instrument_T;
      Time       : Ada.Calendar.Time;
      Bid        : Rate;
      Ask        : Rate;
      Halted     : Boolean;
   end record;

   type Quote_Array is array (Integer range <>) of Quote;

   package Quote_Maps is new Ada.Containers.Hashed_Maps (Key_Type        => Instrument_T,
                                                         Element_Type    => Quote,
                                                         Hash            => Instrument_Hash ,
                                                         Equivalent_Keys => "=");

   function Get_Quote (Instrument : in Instrument_T) return Quote;

   function Get_Quotes
     (Instruments : in Instrument_Array)
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
     (Instrument    : in Instrument_T;
      Granularity   : in Granularity_T   := S5;
      Count         : in Positive        := 500;
      Start_Time    : in Ada.Calendar.Time    := No_Time;
      End_Time      : in Ada.Calendar.Time    := No_Time;
      Candle_Format : in Candle_Format_T := Bid_Ask;
      Include_First : in Boolean         := True)
      return          Candlestick_Array;
end Oanda_API.Rates;

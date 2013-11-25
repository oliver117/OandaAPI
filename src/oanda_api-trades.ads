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

package Oanda_API.Trades is

   -- trade
   type Side_T is (Buy, Sell);

   type Trade_ID is new Natural;

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

   function Get_Open_Trades
     (Acc        : in Account;
      Max_ID     : in Integer := Integer'Last;
      Count      : in Positive := 500;
      Instrument : in Instrument_Identifier := Null_Instrument_Identifier;
      IDs        : in Trade_ID_Array := Null_Trade_ID_Array)
      return       Trade_Array;

   type Affected_Trade is
      record
         ID : Trade_ID;
         Units : Positive; -- Units remaining in updated trade / Units in closed trade
         Side : Side_T;
      end record;

   type Affected_Trade_Array is array (Integer range <>) of Affected_Trade;

   type Create_Trade_Response (Num_Closed : Integer; Num_Changed : Integer) is
      record
         Instrument : Instrument_Identifier;
         Time       : Ada.Calendar.Time;
         Price      : Rate;
         Opened     : Trade_ID;
         Units      : Positive;
         Side       : Side_T;
         Take_Profit   : Pips;
         Stop_Loss     : Pips;
         Trailing_Stop : Pips;
         Closed     : Affected_Trade_Array (1 .. Num_Closed);
         Changed   : Affected_Trade_Array (1 .. Num_Changed);
      end record;

   procedure Create_Trade
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
      Profit     : Money;
      Side       : Side_T;
      Time       : Ada.Calendar.Time;
   end record;

   function Close_Trade
     (Acc  : in Account;
      T_ID : in Trade_ID)
      return Close_Trade_Response;

private

   Null_Trade_ID_Array : constant Trade_ID_Array := (2 .. 1 => <>);

end Oanda_API.Trades;
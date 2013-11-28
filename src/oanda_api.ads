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
with Ada.Containers;
with Ada.Strings.Unbounded;
with AWS.Headers;

package Oanda_API is

   -- Note: A big chunk of the API is not working / undocumented, therefore
   -- some of the functions remain unimplemented for now.

   -- general definitions
   -- constants
   No_Time : constant Ada.Calendar.Time;
   Debug : constant Boolean := False;

   type ID_T is new Long_Long_Integer;
   type ID_Array is array (Integer range <>) of ID_T;

   Null_ID_Array : constant ID_Array;

   type Side_T is (Buy, Sell);

   -- price / rate
   type Rate is delta 0.00_00_1 range 0.0 .. 1.0e6;
   type Money is delta 0.00_00_1 range 0.0 .. 1.0e12;

   type Pips is delta 0.1 range 0.0 .. 1.0e6;
   type Pipettes is delta 1.0 range 0.0 .. 1.0e6;

   function Pips_To_Pipettes (P : in Pips) return Pipettes is (P * 10.0);
   function Pipettes_To_Pips (P : in Pipettes) return Pips is (P / 10.0);

   type Margin_Rate_T is delta 0.001 range 0.0 .. 100.0; -- percent

   -- instrument
   type Instrument_T is private;

   type Instrument_Array is array (Integer range <>) of Instrument_T;

   Null_Instrument : constant Instrument_T;

   function Instrument_Hash (Instrument : in Instrument_T) return Ada.Containers.Hash_Type;

   function To_Instrument (Str : in String) return Instrument_T;

   function To_String (Instrument : in Instrument_T) return String;

   type Instrument_Information is record
      Instrument        : Instrument_T;
      Display_Name      : Ada.Strings.Unbounded.Unbounded_String;
      Pip               : Rate;
      Max_Trade_Units   : Positive;
      Precision         : Rate;
      Max_Trailing_Stop : Pips;
      Min_Trailing_Stop : Pips;
      Margin_Rate       : Margin_Rate_T;
   end record;

   type Instrument_Information_Array is array (Integer range <>) of Instrument_Information;

   -- account
   type Account is private;

   Test_Account : constant Account;

   type Account_Array is array (Integer range <>) of Account;

   function Display_Name (Acc : in Account) return String;


   -- error handling
   API_Error : exception;

private
   Null_ID_Array : constant ID_Array (1 .. 0) := (others => <>);

   function From_RFC3339 (TStr : in String) return Ada.Calendar.Time;

   function To_RFC3339 (Time : in Ada.Calendar.Time) return String;

   Base_Url : constant String       := "http://api-sandbox.oanda.com/v1/";
   No_Time  : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (Year    => 1901,
                                                                  Month   => 1,
                                                                  Day     => 1,
                                                                  Seconds => 0.0);

   type Instrument_T is new Ada.Strings.Unbounded.Unbounded_String;

   Null_Instrument : constant Instrument_T := Instrument_T (Ada.Strings.Unbounded.Null_Unbounded_String);


   type Account is new ID_T;

   Test_Account : constant Account := 2578685;

   procedure Raise_API_Error (Code      : in String;
                              Message   : in String;
                              More_Info : in String);

   GET_Headers : AWS.Headers.List;

end Oanda_API;

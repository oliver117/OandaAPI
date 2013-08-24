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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with AWS.Client;
with AWS.Response;

with GNATCOLL.JSON;

package body Oanda_API is

   ---------------------
   -- To_RFC3339_Time --
   ---------------------

   function To_RFC3339_Time (TStr : in String) return RFC3339_Time is
      Time  : RFC3339_Time;
      First : constant Integer := TStr'First;
   begin
      -- RFC3339 format: YYYY-MM-DDTHH:MM:SS.SSSSSSZ
      -- example: 2013-08-24T15:43:27.384729Z

      Time.Year   :=
         Ada.Calendar.Year_Number'Value (TStr (First .. First + 3));
      Time.Month  :=
         Ada.Calendar.Month_Number'Value (TStr (First + 5 .. First + 6));
      Time.Day    :=
         Ada.Calendar.Day_Number'Value (TStr (First + 8 .. First + 9));
      Time.Hour   := Hour_Number'Value (TStr (First + 11 .. First + 12));
      Time.Minute := Minute_Number'Value (TStr (First + 14 .. First + 15));
      Time.Second :=
         Second_Number'Value (TStr (First + 17 .. TStr'Last - 1));

      return Time;
   end To_RFC3339_Time;

   ---------------
   -- To_String --
   ---------------

   function To_String (Time : in RFC3339_Time) return String is
      use Ada.Strings;
      function T (Source : String; Side : Trim_End := Left) return String
         renames Fixed.Trim;

      -- trimmed image strings
      Year   : constant String :=
        T (Ada.Calendar.Year_Number'Image (Time.Year));
      Month  : constant String :=
        T (Ada.Calendar.Month_Number'Image (Time.Month));
      Day    : constant String :=
        T (Ada.Calendar.Day_Number'Image (Time.Day));
      Hour   : constant String := T (Hour_Number'Image (Time.Hour));
      Minute : constant String := T (Minute_Number'Image (Time.Minute));
      Second : constant String := T (Second_Number'Image (Time.Second));

      -- leading zeros
      Month_LZ  : constant String := (if Time.Month < 10 then "0" else "");
      Day_LZ    : constant String := (if Time.Day < 10 then "0" else "");
      Hour_LZ   : constant String := (if Time.Hour < 10 then "0" else "");
      Second_LZ : constant String := (if Time.Second < 10.0 then "0" else "");
      Minute_LZ : constant String := (if Time.Minute < 10 then "0" else "");

   begin
      return Year &
             "-" &
             Month_LZ &
             Month &
             "-" &
             Day_LZ &
             Day &
             "T" &
             Hour_LZ &
             Hour &
             ":" &
             Minute_LZ &
             Minute &
             ":" &
             Second_LZ &
             Second &
             "Z";
   end To_String;

   -------------------------
   -- To_Identifier_Array --
   -------------------------

   function To_Identifier_Array
     (Instruments : in Instrument_Array)
      return        Instrument_Identifier_Array
   is
      Identifiers : Instrument_Identifier_Array (Instruments'Range);
   begin
      for I in Instruments'Range loop
         Identifiers (I) := Instruments (I).Identifier;
      end loop;

      return Identifiers;
   end To_Identifier_Array;

   ---------------------
   -- Get_Instruments --
   ---------------------

   function Get_Instruments return Instrument_Array is
      use GNATCOLL.JSON;

      Response : AWS.Response.Data;
      JSON     : JSON_Value;
   begin
      Response :=
         AWS.Client.Get
           (URL => Base_Url &
                   "instruments?fields=displayName%2Cpip%2CmaxTradeUnits%2C" &
"precision%2CmaxTrailingStop%2CminTrailingStop%2CmarginRate");

      JSON := Read (AWS.Response.Message_Body (Response), "");

      declare
         use Bounded_Strings;

         Instruments     : constant JSON_Array := Get (JSON, "instruments");
         Num_Instruments : constant Natural    := Length (Instruments);
         Instr           : JSON_Value;
         Instr_Array     : Instrument_Array (1 .. Num_Instruments);
      begin
         for I in Instr_Array'Range loop
            Instr := Get (Instruments, I);

            Instr_Array (I) :=
              Instrument'
              (Identifier        => To_Bounded_String
                                      (Get (Instr, "instrument")),
               Display_Name      => To_Bounded_String
                                      (Get (Instr, "displayName")),
               Pip               => Rate (Float'(Get (Instr, "pip"))),
               Max_Trade_Units   => Get (Instr, "maxTradeUnits"),
               Precision         =>
              Rate (Float'(Get (Instr, "precision"))),
               Max_Trailing_Stop =>
              Pips (Float'(Get (Instr, "maxTrailingStop"))),
               Min_Trailing_Stop =>
              Pips (Float'(Get (Instr, "minTrailingStop"))),
               Margin_Rate       =>
              Margin_Rate_T (Float'(Get (Instr, "marginRate"))));
         end loop;

         return Instr_Array;
      end;
   end Get_Instruments;

   ---------------
   -- Get_Quote --
   ---------------

   function Get_Quote (Instrument : in Instrument_Identifier) return Quote is
      Quotes : constant Quote_Array :=
        Get_Quotes (Instrument_Identifier_Array'(1 => Instrument));
   begin
      return Quotes (Quotes'First);
   end Get_Quote;

   ----------------
   -- Get_Quotes --
   ----------------

   function Get_Quotes
     (Instruments : in Instrument_Identifier_Array)
      return        Quote_Array
   is
      use Ada.Strings.Unbounded;
      use GNATCOLL.JSON;

      Request  : Unbounded_String :=
        To_Unbounded_String ("quote?instruments=");
      Response : AWS.Response.Data;
      JSON     : JSON_Value;
   begin
      for I in Instruments'Range loop
         Request := Request & Bounded_Strings.To_String (Instruments (I));
         if I < Instruments'Last then
            Request := Request & "%2C";
         end if;
      end loop;

      Response := AWS.Client.Get (URL => Base_Url & To_String (Request));

      JSON := Read (AWS.Response.Message_Body (Response), "");

      declare
         use Bounded_Strings;

         Prices      : constant JSON_Array := Get (JSON, "prices");
         Num_Prices  : constant Natural    := Length (Prices);
         Price       : JSON_Value;
         Price_Array : Quote_Array (1 .. Num_Prices); -- TODO : prices or
                                                      --quotes?
      begin
         for I in Price_Array'Range loop
            Price := Get (Prices, I);

            Price_Array (I) :=
              Quote'
              (Instrument => To_Bounded_String (Get (Price, "instrument")),
               Time       => To_RFC3339_Time (Get (Price, "time")),
               Bid        => Rate (Float'(Get (Price, "bid"))),
               Ask        => Rate (Float'(Get (Price, "ask"))),
               Halted     => False);
            if Has_Field (Price, "halted") then
               Price_Array (I).Halted := True;
            end if;
         end loop;

         return Price_Array;
      end;
   end Get_Quotes;

   -----------------
   -- Get_History --
   -----------------

   function Get_History
     (Instrument    : in Instrument_Identifier;
      Granularity   : in Granularity_T   := S5;
      Count         : in Positive        := 500;
      Start_Time    : in RFC3339_Time    := No_Time;
      End_Time      : in RFC3339_Time    := No_Time;
      Candle_Format : in Candle_Format_T := Bid_Ask;
      Include_First : in Boolean         := True)
      return          Candlestick
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning
        (Standard.True,
         "Get_History unimplemented");
      raise Program_Error;
      return Get_History
        (Instrument,
         Granularity,
                Count,
                Start_Time,
                End_Time,
                Candle_Format,
                Include_First);
   end Get_History;

   ------------------
   -- Display_Name --
   ------------------

   function Display_Name (Acc : in Account) return String is
   begin
      return Account'Image (Acc);
   end Display_Name;

   ---------------------
   -- Get_Open_Trades --
   ---------------------

   function Get_Open_Trades
     (Acc        : in Account;
      Max_ID     : in Integer := Integer'Last;
      Count      : in Positive := 500;
      Instrument : in Instrument_Identifier := Null_Instrument_Identifier;
      IDs        : in Trade_ID_Array := Null_Trade_ID_Array)
      return       Trade_Array
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning
        (Standard.True,
         "Get_Open_Trades unimplemented");
      raise Program_Error;
      return Get_Open_Trades (Acc, Max_ID, Count, Instrument, IDs);
   end Get_Open_Trades;

   ----------------
   -- Open_Trade --
   ----------------

   procedure Open_Trade
     (Acc           : in Account;
      Instrument    : in Instrument_Identifier;
      Units         : in Positive;
      Side          : in Side_T;
      Lower_Bound   : in Rate;
      Upper_Bound   : in Rate;
      Stop_Loss     : in Rate;
      Take_Profit   : in Rate;
      Trailing_Stop : in Pipettes)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Open_Trade unimplemented");
      raise Program_Error;
   end Open_Trade;

   ---------------
   -- Get_Trade --
   ---------------

   function Get_Trade (Acc : in Account; T_ID : in Trade_ID) return Trade is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Trade unimplemented");
      raise Program_Error;
      return Get_Trade (Acc, T_ID);
   end Get_Trade;

   ------------------
   -- Modify_Trade --
   ------------------

   function Modify_Trade
     (Acc           : in Account;
      T_ID          : in Trade_ID;
      Stop_Loss     : in Rate;
      Take_Profit   : in Rate;
      Trailing_Stop : in Pipettes)
      return          Trade
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning
        (Standard.True,
         "Modify_Trade unimplemented");
      raise Program_Error;
      return Modify_Trade (Acc, T_ID, Stop_Loss, Take_Profit, Trailing_Stop);
   end Modify_Trade;

   -----------------
   -- Close_Trade --
   -----------------

   function Close_Trade
     (Acc  : in Account;
      T_ID : in Trade_ID)
      return Close_Trade_Response
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning
        (Standard.True,
         "Close_Trade unimplemented");
      raise Program_Error;
      return Close_Trade (Acc, T_ID);
   end Close_Trade;

   ---------------------
   -- Raise_API_Error --
   ---------------------

   procedure Raise_API_Error
     (Code      : in String;
      Message   : in String;
      More_Info : in String)
   is
   begin
      Ada.Exceptions.Raise_Exception
        (API_Error'Identity,
         Code & ", " & Message & ", " & More_Info);
   end Raise_API_Error;

end Oanda_API;

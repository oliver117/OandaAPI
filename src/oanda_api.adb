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

with Ada.Calendar.Formatting;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Client;
with AWS.Messages;
with AWS.Response;
with AWS.URL;

with GNATCOLL.JSON;

package body Oanda_API is

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

      use type AWS.Messages.Status_Code;

      Request  : constant String :=
        "instruments?fields=displayName%2C" &
        "pip%2CmaxTradeUnits%2C" &
        "precision%2CmaxTrailingStop%2C" &
        "minTrailingStop%2CmarginRate";
      Response : AWS.Response.Data;
      JSON     : JSON_Value;
   begin
      if Debug then
         Ada.Text_IO.Put_Line ("DEBUG: " & Request);
      end if;

      Response := AWS.Client.Get (URL => Base_Url & Request);

      JSON :=
         Read (AWS.Response.Message_Body (Response), "json.instruments");

      if AWS.Response.Status_Code (Response) /= AWS.Messages.S200 then
         Raise_API_Error
           (Code      => Integer'Image (JSON.Get ("code")),
            Message   => JSON.Get ("message"),
            More_Info => JSON.Get ("moreInfo"));

         -- exception raised
      end if;

      -- evaluate the response
      declare
         use Bounded_Strings;

         Instruments     : constant JSON_Array := JSON.Get ("instruments");
         Num_Instruments : constant Natural    := Length (Instruments);
         Instr           : JSON_Value;
         Instr_Array     : Instrument_Array (1 .. Num_Instruments);
      begin
         for I in Instr_Array'Range loop
            Instr := Get (Instruments, I);

            Instr_Array (I) :=
              (Identifier        => To_Bounded_String
                                      (Instr.Get ("instrument")),
               Display_Name      => To_Bounded_String
                                      (Instr.Get ("displayName")),
               Pip               => Rate (Float'(Instr.Get ("pip"))),
               Max_Trade_Units   => Instr.Get ("maxTradeUnits"),
               Precision         => Rate (Float'(Instr.Get ("precision"))),
               Max_Trailing_Stop =>
              Pips (Float'(Instr.Get ("maxTrailingStop"))),
               Min_Trailing_Stop =>
              Pips (Float'(Instr.Get ("minTrailingStop"))),
               Margin_Rate       =>
              Margin_Rate_T (Float'(Instr.Get ("marginRate"))));
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

      use type AWS.Messages.Status_Code;

      Request  : Unbounded_String :=
        To_Unbounded_String ("quote?instruments=");
      Response : AWS.Response.Data;
      JSON     : JSON_Value;
   begin
      -- compose the query string
      for I in Instruments'Range loop
         Request := Request & Bounded_Strings.To_String (Instruments (I));
         if I < Instruments'Last then
            Request := Request & "%2C";
         end if;
      end loop;

      if Debug then
         Ada.Text_IO.Put_Line ("DEBUG: " & To_String (Request));
      end if;

      Response := AWS.Client.Get (URL => Base_Url & To_String (Request));

      JSON := Read (AWS.Response.Message_Body (Response), "json.quote");

      if AWS.Response.Status_Code (Response) /= AWS.Messages.S200 then
         Raise_API_Error
           (Code      => Integer'Image (JSON.Get ("code")),
            Message   => JSON.Get ("message"),
            More_Info => JSON.Get ("moreInfo"));

         -- exception raised
      end if;

      -- evaluate the response
      declare
         use Bounded_Strings;

         Prices      : constant JSON_Array := JSON.Get ("prices");
         Num_Prices  : constant Natural    := Length (Prices);
         Price       : JSON_Value;
         Price_Array : Quote_Array (1 .. Num_Prices); -- TODO : prices or
                                                      --quotes?
      begin
         for I in Price_Array'Range loop
            Price := Get (Prices, I);

            Price_Array (I) :=
              (Instrument => To_Bounded_String (Price.Get ("instrument")),
               Time       => From_RFC3339 (Price.Get ("time")),
               Bid        => Rate (Float'(Price.Get ("bid"))),
               Ask        => Rate (Float'(Price.Get ("ask"))),
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
      Granularity   : in Granularity_T     := S5;
      Count         : in Positive          := 500;
      Start_Time    : in Ada.Calendar.Time := No_Time;
      End_Time      : in Ada.Calendar.Time := No_Time;
      Candle_Format : in Candle_Format_T   := Bid_Ask;
      Include_First : in Boolean           := True)
      return          Candlestick_Array
   is
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      use GNATCOLL.JSON;

      use type Ada.Calendar.Time;
      use type AWS.Messages.Status_Code;

      function T (Source : String; Side : Trim_End := Left) return String
         renames Fixed.Trim;

      Request  : Unbounded_String :=
        To_Unbounded_String ("history?instrument=");
      Response : AWS.Response.Data;
      JSON     : JSON_Value;
   begin
      -- compose the query string
      Request := Request & Bounded_Strings.To_String (Instrument);
      Request := Request &
                 "&granularity=" &
        T (Granularity_T'Image (Granularity));

      if Start_Time /= No_Time then
         Request := Request &
                    "&start=" &
                    AWS.URL.Encode (T (To_RFC3339 (Start_Time)));
      end if;

      if End_Time /= No_Time then
         Request := Request &
                    "&end=" &
                    AWS.URL.Encode (T (To_RFC3339 (End_Time)));
      end if;

      if not (Start_Time /= No_Time and End_Time /= No_Time) then
          Request := Request & "&count=" & T (Positive'Image (Count));
      end if;

      -- can only be specified if Start_Time is specified
      if Start_Time /= No_Time then
         if Include_First then
            Request := Request & "&includeFirst=true";
         else
            Request := Request & "&includeFirst=false";
         end if;
      end if;

      case Candle_Format is
         when Bid_Ask =>
            Request := Request & "&candleFormat=bidask";
         when Midpoint =>
            Request := Request & "&candleFormat=midpoint";
      end case;

      if Debug then
         Ada.Text_IO.Put_Line ("DEBUG: " & To_String (Request));
      end if;

      Response := AWS.Client.Get (URL => Base_Url & To_String (Request));

      JSON := Read (AWS.Response.Message_Body (Response), "json.history");

      if AWS.Response.Status_Code (Response) /= AWS.Messages.S200 then
         Raise_API_Error
           (Code      => Integer'Image (JSON.Get ("code")),
            Message   => JSON.Get ("message"),
            More_Info => JSON.Get ("moreInfo"));

         -- exception raised
      end if;

      -- evaluate the response
      declare
         Candles      : constant JSON_Array := JSON.Get ("candles");
         Num_Candles  : constant Natural    := Length (Candles);
         Candle       : JSON_Value;
         Candlesticks : Candlestick_Array (1 .. Num_Candles);
      begin
         for I in Candlesticks'Range loop
            Candle := Get (Candles, I);

            if Candle_Format = Bid_Ask then
               Candlesticks (I) :=
                 (Format    => Bid_Ask,
                  Time      => From_RFC3339 (Candle.Get ("time")),
                  Volume    => Candle.Get ("volume"),
                  Complete  => Candle.Get ("complete"),
                  Open_Bid  => Rate (Float'(Candle.Get ("openBid"))),
                  Open_Ask  => Rate (Float'(Candle.Get ("openAsk"))),
                  High_Bid  => Rate (Float'(Candle.Get ("highBid"))),
                  High_Ask  => Rate (Float'(Candle.Get ("highAsk"))),
                  Low_Bid   => Rate (Float'(Candle.Get ("lowBid"))),
                  Low_Ask   => Rate (Float'(Candle.Get ("lowAsk"))),
                  Close_Bid => Rate (Float'(Candle.Get ("closeBid"))),
                  Close_Ask => Rate (Float'(Candle.Get ("closeAsk"))));

            elsif Candle_Format = Midpoint then
               Candlesticks (I) :=
                 (Format    => Midpoint,
                  Time      => From_RFC3339 (Candle.Get ("time")),
                  Volume    => Candle.Get ("volume"),
                  Complete  => Candle.Get ("complete"),
                  Open_Mid  => Rate (Float'(Candle.Get ("openMid"))),
                  High_Mid  => Rate (Float'(Candle.Get ("highMid"))),
                  Low_Mid   => Rate (Float'(Candle.Get ("lowMid"))),
                  Close_Mid => Rate (Float'(Candle.Get ("closeMid"))));
            end if;
         end loop;

         return Candlesticks;
      end;
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
      Max_ID     : in Integer               := Integer'Last;
      Count      : in Positive              := 500;
      Instrument : in Instrument_Identifier := Null_Instrument_Identifier;
      IDs        : in Trade_ID_Array        := Null_Trade_ID_Array)
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

   -- private

   ------------------
   -- From_RFC3339 --
   ------------------

   function From_RFC3339 (TStr : in String) return Ada.Calendar.Time is
      use Ada.Calendar;
      use Ada.Calendar.Formatting;

      First : constant Integer := TStr'First;
   begin
      -- RFC3339 format: YYYY-MM-DDTHH:MM:SS.SSSSSSZ
      -- example: 2013-08-24T15:43:27.384729Z

      return Time_Of
               (Year       => Year_Number'Value (TStr (First .. First + 3)),
                Month      =>
                   Month_Number'Value (TStr (First + 5 .. First + 6)),
                Day        =>
                   Day_Number'Value (TStr (First + 8 .. First + 9)),
                Hour       =>
                   Hour_Number'Value (TStr (First + 11 .. First + 12)),
                Minute     =>
                   Minute_Number'Value (TStr (First + 14 .. First + 15)),
                Second     =>
                   Second_Number'Value (TStr (First + 17 .. First + 18)),
                Sub_Second =>
                   Second_Duration'Value
                     ("0" & TStr (First + 19 .. TStr'Last - 1)));

      -- last character is assumed to be 'Z'
   end From_RFC3339;

   ----------------
   -- To_RFC3339 --
   ----------------

   function To_RFC3339 (Time : in Ada.Calendar.Time) return String is
      use Ada.Calendar;
      use Ada.Strings;

      function T (Source : String; Side : Trim_End := Left) return String
         renames Fixed.Trim;

      -- trimmed image strings
      Year_Str       : constant String :=
        T (Year_Number'Image (Year (Time)));
      Month_Str      : constant String :=
        T (Month_Number'Image (Month (Time)));
      Day_Str        : constant String := T (Day_Number'Image (Day (Time)));
      Hour_Str       : constant String :=
        T (Formatting.Hour_Number'Image (Formatting.Hour (Time)));
      Minute_Str     : constant String :=
        T (Formatting.Minute_Number'Image (Formatting.Minute (Time)));
      Second_Str     : constant String :=
        T (Formatting.Second_Number'Image (Formatting.Second (Time)));
      Sub_Second_Str : constant String :=
        T (Formatting.Second_Duration'Image (Formatting.Sub_Second (Time)));

      -- leading zeros
      Month_LZ  : constant String := (if Month (Time) < 10 then "0" else "");
      Day_LZ    : constant String := (if Day (Time) < 10 then "0" else "");
      Hour_LZ   : constant String :=
        (if Formatting.Hour (Time) < 10 then "0" else "");
      Minute_LZ : constant String :=
        (if Formatting.Minute (Time) < 10 then "0" else "");
      Second_LZ : constant String :=
        (if Formatting.Second (Time) < 10 then "0" else "");

   begin
      return Year_Str &
             "-" &
             Month_LZ &
             Month_Str &
             "-" &
             Day_LZ &
             Day_Str &
             "T" &
             Hour_LZ &
             Hour_Str &
             ":" &
             Minute_LZ &
             Minute_Str &
             ":" &
             Second_LZ &
             Second_Str &
      -- cut out the leading zero
             Sub_Second_Str (Sub_Second_Str'First + 1 .. Sub_Second_Str'Last) &
             "Z";
   end To_RFC3339;

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

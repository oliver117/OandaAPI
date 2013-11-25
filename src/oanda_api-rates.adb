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

with Ada.Strings.Fixed;
with Ada.Text_IO;
with AWS.Client;
with AWS.Messages;
with AWS.Response;
with AWS.URL;
with GNATCOLL.JSON;

package body Oanda_API.Rates is

   -----------------
   -- Instruments --
   -----------------

   function Get_Instruments (Acc : in Account) return Instrument_Array is
      use Ada.Strings;
      use GNATCOLL.JSON;

      use type AWS.Messages.Status_Code;

      Request  : constant String :=
        "instruments?accountId=" & Fixed.Trim (Account'Image (Acc), Left) &
        "&fields=displayName%2C" &
        "pip%2CmaxTradeUnits%2C" &
        "precision%2CmaxTrailingStop%2C" &
        "minTrailingStop%2CmarginRate";
      Response : AWS.Response.Data;
      JSON     : JSON_Value;
   begin
      if Debug then
         Ada.Text_IO.Put_Line ("DEBUG: " & Request);
      end if;

      Response := AWS.Client.Get (URL => Base_Url & Request,
                                  Headers => GET_Headers);

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
         Instruments     : constant JSON_Array := JSON.Get ("instruments");
         Num_Instruments : constant Natural    := Length (Instruments);
         Instr           : JSON_Value;
         Instr_Array     : Instrument_Array (1 .. Num_Instruments);
      begin
         for I in Instr_Array'Range loop
            Instr := Get (Instruments, I);

            Instr_Array (I) :=
              (Identifier        => To_Identifier (Instr.Get ("instrument")),
               Display_Name      => Unbounded.To_Unbounded_String (Source => Instr.Get ("displayName")),
               Pip               => Rate (Float'(Instr.Get ("pip"))),
               Max_Trade_Units   => Instr.Get ("maxTradeUnits"),
               Precision         => Rate (Float'(Instr.Get ("precision"))),
               Max_Trailing_Stop => Pips (Float'(Instr.Get ("maxTrailingStop"))),
               Min_Trailing_Stop => Pips (Float'(Instr.Get ("minTrailingStop"))),
               Margin_Rate       => Margin_Rate_T (Float'(Instr.Get ("marginRate"))));
         end loop;

         return Instr_Array;
      end;
   end Get_Instruments;

   ---------------
   -- Get_Quote --
   ---------------

   function Get_Quote (Instr : in Instrument) return Quote is
      Quotes : constant Quote_Array :=
        Get_Quotes (Instrument_Array'(1 => Instr));
   begin
      return Quotes (Quotes'First);
   end Get_Quote;

   ----------------
   -- Get_Quotes --
   ----------------

   function Get_Quotes
     (Instruments : in Instrument_Array)
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
         Request := Request & To_String (Instruments (I).Identifier);
         if I < Instruments'Last then
            Request := Request & "%2C";
         end if;
      end loop;

      if Debug then
         Ada.Text_IO.Put_Line ("DEBUG: " & To_String (Request));
      end if;

      Response := AWS.Client.Get (URL => Base_Url & To_String (Request),
                                  Headers => GET_Headers);

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
         Prices      : constant JSON_Array := JSON.Get ("prices");
         Num_Prices  : constant Natural    := Length (Prices);
         Price       : JSON_Value;
         Price_Array : Quote_Array (1 .. Num_Prices); -- TODO : prices or
                                                      --quotes?
      begin
         for I in Price_Array'Range loop
            Price := Get (Prices, I);

            Price_Array (I) :=
              (Instrument => To_Identifier (Price.Get ("instrument")),
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
      Request := Request & To_String (Instrument);
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

      Response := AWS.Client.Get (URL => Base_Url & To_String (Request),
                                  Headers => GET_Headers);

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

end Oanda_API.Rates;

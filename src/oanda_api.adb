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
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;

with AWS.Headers.Set;

package body Oanda_API is

   use Ada.Strings.Unbounded;

   ---------------------
   -- Instrument_Hash --
   ---------------------

   function Instrument_Hash (Instrument : in Instrument_T) return Ada.Containers.Hash_Type is (Hash (Instrument));

   -------------------
   -- To_Instrument --
   -------------------

   function To_Instrument (Str : in String) return Instrument_T is (Instrument_T'(To_Unbounded_String (Str)));

   ---------------
   -- To_String --
   ---------------

   function To_String (Instrument : in Instrument_T) return String is (To_String (Source => Unbounded_String (Instrument)));

   ------------------
   -- Display_Name --
   ------------------

   function Display_Name (Acc : in Account) return String is
   begin
      return Account'Image (Acc);
   end Display_Name;


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
      if Debug then
         Ada.Text_IO.Put_Line ("DEBUG: Raising API Error");
      end if;

      Ada.Exceptions.Raise_Exception
        (API_Error'Identity,
         Code & ", " & Message & ", " & More_Info);
   end Raise_API_Error;

begin
   AWS.Headers.Set.Add (Headers => GET_Headers,
                        Name    => "Content-Type",
                        Value   => "application/x-www-form-urlencoded");

end Oanda_API;

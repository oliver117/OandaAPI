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

package Oanda_API.Accounts is

   function Get_Accounts (Username : in String) return Account_Array;

   procedure Create_Test_Account (Username : out String;
                                  Password : out String;
                                  Acc : out Account);

   type Account_Information is
      record
         Account_ID : Integer;
         Account_Name : Ada.Strings.Unbounded.Unbounded_String;
         Balance : Money;
         Unrealized_PL : Money; -- unrealized profit/loss
         Realized_PL : Money; -- realized profit/loss
         Margin_Used : Money;
         Margin_Available : Money;
         Open_Trades : Natural;
         Open_Orders : Natural;
         Margin_Rate : Margin_Rate_T;
         Account_Currency : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function Get_Account_Information (Acc : in Account) return Account_Information;

end Oanda_API.Accounts;

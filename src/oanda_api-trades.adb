package body Oanda_API.Trades is

   ---------------------
   -- Get_Open_Trades --
   ---------------------

   function Get_Open_Trades
     (Acc        : in Account;
      Max_ID     : in Integer := Integer'Last;
      Count      : in Positive := 500;
      Instrument : in Instrument_T := Null_Instrument;
      IDs        : in ID_Array := Null_ID_Array)
      return Trade_Array
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Open_Trades unimplemented");
      raise Program_Error with "Unimplemented function Get_Open_Trades";
      return Get_Open_Trades (Acc, Max_ID, Count, Instrument, IDs);
   end Get_Open_Trades;

   ------------------
   -- Create_Trade --
   ------------------

   procedure Create_Trade
     (Acc           : in Account;
      Instrument    : in Instrument_T;
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
      pragma Compile_Time_Warning (Standard.True, "Create_Trade unimplemented");
      raise Program_Error with "Unimplemented procedure Create_Trade";
   end Create_Trade;

   ---------------
   -- Get_Trade --
   ---------------

   function Get_Trade (Acc : in Account; Trade_ID : in ID_T) return Trade is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Trade unimplemented");
      raise Program_Error with "Unimplemented function Get_Trade";
      return Get_Trade (Acc, Trade_ID);
   end Get_Trade;

   ------------------
   -- Modify_Trade --
   ------------------

   function Modify_Trade
     (Acc           : in Account;
      Trade_ID          : in ID_T;
      Stop_Loss     : in Rate;
      Take_Profit   : in Rate;
      Trailing_Stop : in Pipettes)
      return Trade
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Modify_Trade unimplemented");
      raise Program_Error with "Unimplemented function Modify_Trade";
      return Modify_Trade (Acc, Trade_ID, Stop_Loss, Take_Profit, Trailing_Stop);
   end Modify_Trade;

   -----------------
   -- Close_Trade --
   -----------------

   function Close_Trade
     (Acc  : in Account;
      Trade_ID : in ID_T)
      return Close_Trade_Response
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Close_Trade unimplemented");
      raise Program_Error with "Unimplemented function Close_Trade";
      return Close_Trade (Acc, Trade_ID);
   end Close_Trade;

end Oanda_API.Trades;

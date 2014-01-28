package body Oanda_API.Transactions is

   -----------------------------
   -- Get_Transaction_History --
   -----------------------------

   function Get_Transaction_History
     (Max_ID : ID_T;
      Min_ID : ID_T;
      Count : Natural;
      IDs : ID_Array)
      return Transaction_Array
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Transaction_History unimplemented");
      raise Program_Error with "Unimplemented function Get_Transaction_History";
      return Get_Transaction_History (Max_ID, Min_ID, Count, IDs);
   end Get_Transaction_History;

   ---------------------------------
   -- Get_Transaction_Information --
   ---------------------------------

   function Get_Transaction_Information
     (Acc : Account;
      Transaction_ID : ID_T)
      return Transaction
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Transaction_Information unimplemented");
      raise Program_Error with "Unimplemented function Get_Transaction_Information";
      return Get_Transaction_Information (Acc, Transaction_ID);
   end Get_Transaction_Information;

end Oanda_API.Transactions;

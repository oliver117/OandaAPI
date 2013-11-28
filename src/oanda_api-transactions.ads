

package Oanda_API.Transactions is

   type Transaction_Action is (Open, Close, Update, Flag_To_Close,
                               Correct, Create, Enter, Resolve,
                               Debit, Deposit, Credit, Withdraw, Reset);

   type Transaction_Reason is (Cancelled, Stop_Loss_Cancelled, Take_Profit_Cancelled,
                               Market_Fill_Cancelled, Correction, User_Submitted,
                               Expired, Stop_Loss, Take_Profit, Margin_Closeout,
                               Market_Filled, Order_Filled, Non_Sufficient_Funds,
                               Bounds_Violation, Transfer, Account_Migration,
                               Prime_Brokerage_Giveup, Trailing_Stop, Limit_Stop_FOK,
                               Limit_Stop_IOC, Merge, Flagged_for_Closing, Bulk_Close,
                               Account_Created, Margin_Alert_Entere, Margin_Alert_Resolved,
                               Account_Transfer, Invalid_Units, Stop_Loss_Violation,
                               Take_Profit_Violation, Trailing_Stop_Violation, Instrument_Halted,
                               Account_Non_Tradable, No_New_Position_Allowed, Insufficient_Liquidity);

   type Transaction is record
      ID : ID_T;
      Acc : Account;
      Typ : Order_Type;
      Units : Natural;
      Side : Side_T;
      Action : Transaction_Action;
      Reason : Transaction_Reason;
      Instrument : Instrument_Identifier;
      Time : Ada.Calendar.Time;
      Price : Rate;
      Balance : Money;
      Profit_Loss : Money;
      Margin_Used : Margin_Rate_T;
   end Transaction;

   type Transaction_Array is array (Integer range <>) of Transaction;

   function Get_Transaction_History (Max_ID : ID_T;
                                     Min_ID : ID_T;
                                     Count : Natural;
                                     IDs : ID_Array) return Transaction_Array;

   function Get_Transaction_Information (Acc : Account;
                                         Transaction_ID : ID_T) return Transaction;
end Oanda_API.Transactions;

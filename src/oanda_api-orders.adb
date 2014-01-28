package body Oanda_API.Orders is

   ----------------
   -- Get_Orders --
   ----------------

   function Get_Orders
     (Acc : in Account;
      Max_ID : in Natural;
      Count : in Positive := 50;
      Instrument : in Instrument_T;
      IDs : in ID_Array)
      return Order_Array
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Orders unimplemented");
      raise Program_Error with "Unimplemented function Get_Orders";
      return Get_Orders (Acc, Max_ID, Count, Instrument, IDs);
   end Get_Orders;

   ------------------
   -- Create_Order --
   ------------------

   function Create_Order
     (Acc : Account;
      Instrument : Instrument_T;
      Units : Natural;
      Expiry : Ada.Calendar.Time;
      Price : Rate;
      Side : Side_T;
      Typ : Order_Type;
      Lower_Bound : Rate;
      Upper_Bound : Rate;
      Stop_Loss : Rate;
      Take_Profit : Rate;
      Trailing_Stop : Pips)
      return Create_Order_Response
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Create_Order unimplemented");
      raise Program_Error with "Unimplemented function Create_Order";
      return Create_Order (Acc, Instrument, Units, Expiry, Price, Side, Typ,
         Lower_Bound, Upper_Bound, Stop_Loss, Take_Profit, Trailing_Stop);
   end Create_Order;

   ---------------------------
   -- Get_Order_Information --
   ---------------------------

   function Get_Order_Information
     (Acc : Account;
      Order_ID : ID_T)
      return Order_Information_Response
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Order_Information unimplemented");
      raise Program_Error with "Unimplemented function Get_Order_Information";
      return Get_Order_Information (Acc, Order_ID);
   end Get_Order_Information;

   ------------------
   -- Modify_Order --
   ------------------

   function Modify_Order
     (Acc : Account;
      Order_ID : ID_T;
      Units : Natural;
      Price : Rate;
      Expiry : Ada.Calendar.Time;
      Lower_Bound : Rate;
      Upper_Bound : Rate;
      Stop_Loss : Rate;
      Take_Profit : Rate;
      Trailing_Stop : Pipettes)
      return Order_Information_Response
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Modify_Order unimplemented");
      raise Program_Error with "Unimplemented function Modify_Order";
      return Modify_Order (Acc, Order_ID, Units, Price, Expiry, Lower_Bound,
         Upper_Bound, Stop_Loss, Take_Profit, Trailing_Stop);
   end Modify_Order;

   -----------------
   -- Close_Order --
   -----------------

   function Close_Order
     (Acc : Account;
      Order_ID : ID_T)
      return Close_Order_Response
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Close_Order unimplemented");
      raise Program_Error with "Unimplemented function Close_Order";
      return Close_Order (Acc, Order_ID);
   end Close_Order;

end Oanda_API.Orders;



package Oanda_API.Orders is

   type Order_Type is (Market_If_Touched, Limit, Stop);

   type Order is record
      ID : ID_T;
      Typ : Order_Type;
      Side : Side_T;
      Instrument : Instrument_T;
      Units : Natural;
      Time : Ada.Calendar.Time;
      Price : Rate;
      Stop_Loss : Rate;
      Take_Profit : Rate;
      Expiry : Ada.Calendar.Time;
      Upper_Bound : Rate;
      Lower_Bound : Rate;
      Trailing_Stop : Pipettes;
      Oca_Group_ID : ID_T;
   end record;

   type Order_Array is array (Integer range <>) of Order;

   function Get_Orders (Acc : in Account;
                        Max_ID : in Natural;
                        Count : in Positive := 50;
                        Instrument : in Instrument_T;
                        IDs : in ID_Array) return Order_Array;

   type Create_Order_Response is record
      ID : ID_T;
      Instrument : Instrument_T;
      Units : Natural;
      Side : Side_T;
      Time : Ada.Calendar.Time;
      Price : Rate;
      Stop_Loss : Rate;
      Take_Profit : Rate;
      Expiry : Ada.Calendar.Time;
      Upper_Bound : Rate;
      Lower_Bound : Rate;
      Trailing_Stop : Pipettes;
   end record;

   function Create_Order (Acc : Account;
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
                          Trailing_Stop : Pips) return Create_Order_Response;

   type Order_Information_Response is record
      ID : ID_T;
      Typ : Order_Type;
      Side : Side_T;
      Instrument : Instrument_T;
      Units : Natural;
      Time : Ada.Calendar.Time;
      Price : Rate;
      Stop_Loss : Rate;
      Take_Profit : Rate;
      Expiry : Ada.Calendar.Time;
      Upper_Bound : Rate;
      Lower_Bound : Rate;
      Trailing_Stop : Pipettes;
   end record;

   function Get_Order_Information (Acc : Account;
                                   Order_ID : ID_T) return Order_Information_Response;

   function Modify_Order (Acc : Account;
                          Order_ID : ID_T;
                          Units : Natural;
                          Price : Rate;
                          Expiry : Ada.Calendar.Time;
                          Lower_Bound : Rate;
                          Upper_Bound : Rate;
                          Stop_Loss : Rate;
                          Take_Profit : Rate;
                          Trailing_Stop : Pipettes) return Order_Information_Response;

   type Close_Order_Response is record
      ID : ID_T;
      Instrument : Instrument_T;
      Units : Natural;
      Side : Side_T;
      Price : Rate;
      Time : Ada.Calendar.Time;
   end record;

   function Close_Order (Acc : Account;
                         Order_ID : ID_T) return Close_Order_Response;
end Oanda_API.Orders;

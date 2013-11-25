package body Oanda_API.Accounts is

   ------------------
   -- Get_Accounts --
   ------------------

   function Get_Accounts (Username : in String) return Account_Array is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Accounts unimplemented");
      raise Program_Error with "Unimplemented function Get_Accounts";
      return Get_Accounts (Username);
   end Get_Accounts;

   -------------------------
   -- Create_Test_Account --
   -------------------------

   procedure Create_Test_Account
     (Username : out String;
      Password : out String;
      Acc : out Account)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Create_Test_Account unimplemented");
      raise Program_Error with "Unimplemented procedure Create_Test_Account";
   end Create_Test_Account;

   -----------------------------
   -- Get_Account_Information --
   -----------------------------

   function Get_Account_Information
     (Acc : in Account)
      return Account_Information
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Account_Information unimplemented");
      raise Program_Error with "Unimplemented function Get_Account_Information";
      return Get_Account_Information (Acc);
   end Get_Account_Information;

end Oanda_API.Accounts;

package body PasswordManager is

   procedure Init(M : out Manager; P : in PIN.PIN) with SPARK_Mode => Off is
   begin
      PasswordDatabase.Init(M.Database);
      M.Master_Pin := P;
      M.Is_Locked := TRUE;
   end Init;

   procedure Put(M : in out Manager; U : in URL; P : in Password) is
   begin
      PasswordDatabase.Put(M.Database, U, P);
   end Put;

   procedure Remove(M : in out Manager; U : in URL) is
   begin
      PasswordDatabase.Remove(M.Database, U);
   end Remove;

   procedure Lock(M : in out Manager; P : PIN.PIN) is
   begin
      M.Master_Pin := P;
      M.Is_Locked := TRUE;
   end Lock;

   procedure Unlock(M : in out Manager; P : PIN.PIN) is
   begin
      If PIN."="(M.Master_Pin, P) then
         M.Is_Locked := FALSE;
      end if;
   end Unlock;

   function Get(M : in Manager; U : in PasswordDatabase.URL) return
     PasswordDatabase.Password is
   begin
      return PasswordDatabase.Get(M.Database, U);
   end Get;

end PasswordManager;

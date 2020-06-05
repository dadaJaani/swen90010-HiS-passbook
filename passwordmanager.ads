with Ada.Containers.Formal_Ordered_Maps;
with Ada.Containers;
use Ada.Containers;

with PIN;

with PasswordDatabase;
use PasswordDatabase;

with MyString;

package PasswordManager with SPARK_Mode is
   
    type Manager is record
        Database : PasswordDatabase.Database;
        Master_Pin : PIN.PIN;
        Is_Locked : Boolean;
    end record;
   
   procedure Put(M : in out Manager; U : in URL; P : in Password) with
     pre => M.Is_Locked = FALSE;
   
   procedure Remove(M : in out Manager; U : in URL) with
     pre => M.Is_Locked = FALSE;

   procedure Init(M : out Manager; P : in PIN.PIN) with
     post => PIN."="(M.Master_Pin, P) and M.Is_Locked = TRUE;

   procedure Lock(M : in out Manager; P : in PIN.PIN) with
     pre => M.Is_Locked = FALSE, 
     post => PIN."="(M.Master_Pin, P) and M.Is_Locked = TRUE;
     
   procedure Unlock(M : in out Manager; P : in PIN.PIN) with
     pre => M.Is_Locked = TRUE,
     post => (if PIN."="(M.Master_Pin, P) then M.Is_Locked = FALSE else
          M.Is_Locked = TRUE);
   
   function Get(M : in Manager; U : in URL) return Password with 
     pre => M.Is_Locked = FALSE;

end PasswordManager;

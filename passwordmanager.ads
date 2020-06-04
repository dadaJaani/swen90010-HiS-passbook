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
   
    procedure Put(M : in out Manager; U : in URL; P : in Password);
   
    procedure Remove(M : in out Manager; U : in URL);

    procedure Init(M : out Manager; P : in PIN.PIN);

    procedure Lock(M : in out Manager; P : in PIN.PIN);
     
    procedure Unlock(M : in out Manager; P : in PIN.PIN);
   
    function Get(M : in Manager; U : in URL) return Password with 
        Pre => PasswordDatabase.Has_Password_For(M.Database,U);

end PasswordManager;

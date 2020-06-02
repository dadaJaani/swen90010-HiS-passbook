with Ada.Containers.Formal_Ordered_Maps;
with Ada.Containers;
use Ada.Containers;

with PIN;

with MyString;

package PasswordManager with SPARK_Mode is

   Master_Pin : PIN.PIN := PIN.From_String("");
   Is_Locked : Boolean := TRUE;

   type Manager is private;

   procedure Init(Manager : out Manager);

   procedure Lock(M : in out Manager; P : in PIN.PIN);
     

   procedure Unlock(M : in out Manager; P : in PIN.PIN);


end PasswordManager;

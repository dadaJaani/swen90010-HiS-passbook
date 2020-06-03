pragma SPARK_Mode (On);

with PasswordDatabase;
with PasswordManager;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with PIN;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   M : PasswordManager.Manager;

   U : PasswordDatabase.URL;
   P : PasswordDatabase.Password;

   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;

   Command_PUT : Lines.MyString := Lines.From_String("put");
   Command_GET : Lines.MyString := Lines.From_String("get");
   Command_REM : Lines.MyString := Lines.From_String("rem");
   Command_LOCK : Lines.MyString := Lines.From_String("lock");
   Command_UNLOCK : Lines.MyString := Lines.From_String("unlock");

   PIN1  : PIN.PIN;
 
begin
  
   If MyCommandLine.Argument_Count = 1 then
      If (MyCommandLine.Argument(1)'Length = 4) and (for all I in MyCommandLine.Argument(1)'Range => (MyCommandLine.Argument(1)(I) >= '0' and MyCommandLine.Argument(1)(I) <= '9')) then
         PIN1 := PIN.From_String(MyCommandLine.Argument(1));
         PasswordManager.Init(M, PIN1);
      else
         return;
      end if;
   else 
      return; 
   end if;

   loop
      If M.Is_Locked = TRUE then
         Put("locked>   ");
      else 
         Put("unlocked> ");
      end if;
      
      Lines.Get_Line(S);

      declare
         T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
         NumTokens : Natural;
      
      begin
         MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
         if NumTokens > 3 then
            Put_Line("Too many tokens!");
         elsif NumTokens = 0 then
            Put_Line("No tokens!");
         else
            declare
               TokStr : String := Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1));
            begin
               If Lines.Equal(Lines.From_String(TokStr), Command_GET) then
                  U := PasswordDatabase.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                  P := PasswordManager.Get(M, U);
                  Put_Line(PasswordDatabase.To_String(P));
               elsif Lines.Equal(Lines.From_String(TokStr), Command_PUT) then 
                  U := PasswordDatabase.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                  P := PasswordDatabase.From_String(Lines.To_String(Lines.Substring(S,T(3).Start,T(3).Start+T(3).Length-1)));
                  PasswordManager.Put(M, U, P);
               elsif Lines.Equal(Lines.From_String(TokStr), Command_REM) then
                  U := PasswordDatabase.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                  PasswordManager.Remove(M, U);
               elsif Lines.Equal(Lines.From_String(TokStr), Command_LOCK) then 
                  PIN1 := PIN.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                  PasswordManager.Lock(M, PIN1);
               elsif Lines.Equal(Lines.From_String(TokStr), Command_UNLOCK) then 
                  PIN1 := PIN.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                  PasswordManager.Unlock(M, PIN1);
               else
                  return;
               end if;
            end;
         end if;
      end;
   end loop;
   

   

 

   
   
end Main;





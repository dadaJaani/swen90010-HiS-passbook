pragma SPARK_Mode (On);

with PasswordDatabase;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with PIN;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   DB : PasswordDatabase.Database;
   Master_Pin : PIN.PIN; 
   Is_Locked : Boolean := TRUE;

   U : PasswordDatabase.URL;
   P : PasswordDatabase.Password;

   package CommandStrings is new MyString(Max_MyString_Length => 6);

   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;

   Command_PUT : CommandStrings.MyString := CommandStrings.From_String("put");
   Command_GET : CommandStrings.MyString := CommandStrings.From_String("get");
   Command_REM : CommandStrings.MyString := CommandStrings.From_String("rem");
   Command_LOCK : CommandStrings.MyString := CommandStrings.From_String("lock");
   Command_UNLOCK : CommandStrings.MyString := CommandStrings.From_String("unlock");

   PIN1  : PIN.PIN;
 
begin
   If MyCommandLine.Argument_Count = 1 then
      Master_Pin := PIN.From_String(MyCommandLine.Argument(1));
   else 
      return; 
   end if;

   PasswordDatabase.Init(DB);

   loop
      If Is_Locked = TRUE then
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
            Put_Line("It's empty you fucking idiot!");
         else
            declare
               TokStr : String := Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1));
            begin
               If CommandStrings.Equal(CommandStrings.From_String(TokStr), Command_GET) then
                  If Is_Locked = FALSE then
                     U := PasswordDatabase.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                     If PasswordDatabase.Has_Password_For(DB, U) then
                        P := PasswordDatabase.Get(DB, U);
                        Put_Line(PasswordDatabase.To_String(P));
                     end if;
                  end if;
               elsif CommandStrings.Equal(CommandStrings.From_String(TokStr), Command_PUT) then 
                  If Is_Locked = FALSE then
                     U := PasswordDatabase.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                     P := PasswordDatabase.From_String(Lines.To_String(Lines.Substring(S,T(3).Start,T(3).Start+T(3).Length-1)));
                     PasswordDatabase.Put(DB, U, P); 
                  end if;
               elsif CommandStrings.Equal(CommandStrings.From_String(TokStr), Command_REM) then 
                  If Is_Locked = FALSE then
                     U := PasswordDatabase.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                     PasswordDatabase.Remove(DB, U);
                  end if;
               elsif CommandStrings.Equal(CommandStrings.From_String(TokStr), Command_LOCK) then 
                  If Is_Locked = FALSE then 
                     Master_Pin := PIN.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                     Is_Locked := TRUE;
                  end if;
               elsif CommandStrings.Equal(CommandStrings.From_String(TokStr), Command_UNLOCK) then 
                  If Is_Locked = TRUE then
                     PIN1 := PIN.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                     If PIN."="(PIN1,Master_Pin) then 
                        Is_Locked := FALSE;
                     end if;
                  end if;
               else
                  Put("INVALID COMMAND");
               end if;
            end;
         end if;
      end;
   end loop;
   

   

 

   
   
end Main;





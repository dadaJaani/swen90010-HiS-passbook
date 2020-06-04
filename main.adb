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
         elsif NumTokens <= 0 then
            Put_Line("No tokens!");
         else
            declare
               TokStr : String := Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1));
            begin
               -- ----------------------------------------------------------------------------------------------------------------------------------
               -- When Command is to GET a Password for the URL from the Password Manager
               -- ----------------------------------------------------------------------------------------------------------------------------------
               If Lines.Equal(Lines.From_String(TokStr), Command_GET) and NumTokens = 2 then
                  If T(2).Start <= T(2).Start+T(2).Length-1 then
                     declare
                        firstToken:  String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
                     begin  
                        If firstToken'Length <= PasswordDatabase.Max_URL_Length then
                    
                           U := PasswordDatabase.From_String(firstToken);
                           If PasswordDatabase.Has_Password_For(M.Database, U) then -- Satisfies HAS_PASSWORD_FOR precondition
                              P := PasswordManager.Get(M, U);
                              Put_Line(PasswordDatabase.To_String(P));
                           end if;
                     
                        end if;
                     end;	
                  end if;
                  
               -- ----------------------------------------------------------------------------------------------------------------------------------
               -- When Command is to Put a URL into the Password Manager
               -- ----------------------------------------------------------------------------------------------------------------------------------
               elsif Lines.Equal(Lines.From_String(TokStr), Command_PUT) and NumTokens = 3 then 
                  If T(2).Start <= T(2).Start+T(2).Length-1 and T(3).Start <= T(3).Start+T(3).Length-1 then
                  	 declare
                        firstToken: String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
                        secondToken: String := Lines.To_String(Lines.Substring(S,T(3).Start,T(3).Start+T(3).Length-1));
                     begin  
                        If firstToken'Length <= PasswordDatabase.Max_URL_Length and secondToken'Length <= PasswordDatabase.Max_Password_Length then
                           U := PasswordDatabase.From_String(firstToken);
                           P := PasswordDatabase.From_String(secondToken);
                           PasswordManager.Put(M, U, P);
                        end if;
                     end;
                  end if;	
                  
               -- ----------------------------------------------------------------------------------------------------------------------------------
               -- When Command is to Remove a URL from the Password Manager
               -- ----------------------------------------------------------------------------------------------------------------------------------
               elsif Lines.Equal(Lines.From_String(TokStr), Command_REM) and NumTokens = 2 then
                  If T(2).Start <= T(2).Start+T(2).Length-1 then
                     declare
                        firstToken:  String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
                     begin  
                        If firstToken'Length <= PasswordDatabase.Max_URL_Length then
                           U := PasswordDatabase.From_String(firstToken);
                           If PasswordDatabase.Has_Password_For(M.Database, U) then -- Satisfies HAS_PASSWORD_FOR precondition
                              PasswordManager.Remove(M, U);
                           end if;
                        end if;
                     end;	
                  end if;
               
               -- ----------------------------------------------------------------------------------------------------------------------------------
               -- When Command is to Lock the Password Manager
               -- ----------------------------------------------------------------------------------------------------------------------------------
               elsif Lines.Equal(Lines.From_String(TokStr), Command_LOCK) and NumTokens = 2 then 
                  If T(2).Start <= T(2).Start+T(2).Length-1 then
                  	 declare
                        firstToken: String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
                     begin  
                        
                        If firstToken'Length = 4 and (for all I in firstToken'Range => firstToken(I) >= '0' and firstToken(I) <= '9') then
                           PIN1 := PIN.From_String(firstToken);
                           PasswordManager.Lock(M, PIN1);
                        end if;
                     end;
                  end if;	
                  
                  
                  
               -- ----------------------------------------------------------------------------------------------------------------------------------
               -- When Command is to Unlock the Password Manager
               -- ----------------------------------------------------------------------------------------------------------------------------------                  
               elsif Lines.Equal(Lines.From_String(TokStr), Command_UNLOCK) and NumTokens = 2 then 
                  If T(2).Start <= T(2).Start+T(2).Length-1 then
                  	 declare
                        firstToken: String := Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1));
                     begin  
                        
                        If firstToken'Length = 4 and (for all I in firstToken'Range => firstToken(I) >= '0' and firstToken(I) <= '9') then
                           PIN1 := PIN.From_String(firstToken);
                           PasswordManager.Unlock(M, PIN1);
                        end if;
                     end;
                  end if;	
                   
                  
               -- ----------------------------------------------------------------------------------------------------------------------------------
               -- When Command is to Not Recognised by the Password Manager
               -- ----------------------------------------------------------------------------------------------------------------------------------               
               else
                  return;
               end if;
            end;
         end if;
      end;
   end loop;
   

   

 

   
   
end Main;





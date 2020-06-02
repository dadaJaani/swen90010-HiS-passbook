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

   package CommandStrings is new MyString(Max_MyString_Length => 6);
   C  : CommandStrings.MyString;

   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;

   Command_PUT : CommandStrings.MyString := CommandStrings.From_String("put");
   Command_GET : CommandStrings.MyString := CommandStrings.From_String("get");
   Command_REM : CommandStrings.MyString := CommandStrings.From_String("rem");
   Command_LOCK : CommandStrings.MyString := CommandStrings.From_String("lock");
   Command_UNLOCK : CommandStrings.MyString := CommandStrings.From_String("unlock");

   PIN1  : PIN.PIN := PIN.From_String("1234");
 
begin

   Put("I was invoked with "); Put(MyCommandLine.Argument_Count,0); Put_Line(" arguments.");
   If MyCommandLine.Argument_Count = 1 then
      Master_Pin := PIN.From_String(MyCommandLine.Argument(1));
   else 
      return; 
   end if;

   PasswordDatabase.Init(DB);

   loop
      Put_Line("Reading a line of input. Enter some text (at most 3 tokens): ");
      Lines.Get_Line(S);

      Put_Line("Splitting the text into at most 4 tokens");
      declare
         T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
         NumTokens : Natural;
      
      begin
         MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
         Put("You entered "); Put(NumTokens); Put_Line(" tokens.");
         if NumTokens > 3 then
            Put_Line("Too many tokens!");
         else
            for I in 1..NumTokens loop
               declare
                  TokStr : String := Lines.To_String(Lines.Substring(S,T(I).Start,T(I).Start+T(I).Length-1));
               begin
                  If I = 1 then
                     If CommandStrings.Equal(CommandStrings.From_String(TokStr), Command_GET) then
                        C := Command_GET;
                        Put(CommandStrings.To_String(C));
                        -- uncomment when ^ works 
                        -- If Is_Locked = TRUE then
                           -- DB.Get(DB, Tokens[2]; -- second item: url
                     elsif CommandStrings.Equal(CommandStrings.From_String(TokStr), Command_PUT) then 
                        C := Command_PUT;
                        Put(CommandStrings.To_String(C));
                        -- uncomment when ^ works 
                        -- If Is_Locked = TRUE then
                           -- DB.Put(DB, Tokens[2], Tokens[3]; -- second item: url, third: pw
                     elsif CommandStrings.Equal(CommandStrings.From_String(TokStr), Command_REM) then 
                        C := Command_REM;
                        Put(CommandStrings.To_String(C));
                        -- uncomment when ^ works 
                        -- If Is_Locked = TRUE then
                           -- DB.Get(DB, Tokens[2]; -- second item: url
                     elsif CommandStrings.Equal(CommandStrings.From_String(TokStr), Command_LOCK) then 
                        C := Command_LOCK;
                        Put(CommandStrings.To_String(C));
                        Is_Locked := TRUE;
                        -- Master_Pin := *seccond token*
                     elsif CommandStrings.Equal(CommandStrings.From_String(TokStr), Command_UNLOCK) then 
                        C := Command_UNLOCK;
                        Put(CommandStrings.To_String(C));
                        -- *If secondtoken = Master_Pin  then*
                        Is_Locked := FALSE;
                     else
                        Put("INVALID COMMAND");
                     end if;
                  else 
                     Put("Token "); Put(I); Put(" is: """);
                     Put(TokStr); Put_Line("""");
                  
                  end if;
            end;
            end loop;
         


         end if;
      end;
   end loop;
   

   

 

   
   
end Main;





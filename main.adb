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

   Command_PUT : constant String := "put";
   Command_GET : constant String := "get";
   Command_REM : constant String := "rem";
   Command_LOCK : constant String := "lock";
   Command_UNLOCK : constant String := "unlock";

   PIN1  : PIN.PIN := PIN.From_String("1234");
   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;


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

            case *first-TOKENNN* is
               when Command_PUT => 
                  Put(T(1)); Put_Line("Put command "); -- just to check if it works
                  -- uncomment when ^ works 
                  -- If Is_Locked = TRUE then
                     -- DB.Put(DB, Tokens[2], Tokens[3]; -- second item: url, third: pw

               when Command_GET => 
                  Put(T(1)); Put_Line("GET command "); -- just to check if it works

                  -- uncomment when ^ works 
                  -- If Is_Locked = TRUE then
                     -- DB.Get(DB, Tokens[2]; -- second item: url
               when Command_REM => 
                  Put(T(1)); Put_Line("REM command "); -- just to check if it works

                  -- uncomment when ^ works 
                  -- If Is_Locked = TRUE then
                     -- DB.Get(DB, Tokens[2]; -- second item: url
               when Command_LOCK => 
                  Is_Locked := TRUE;
                  -- Master_Pin := *seccond token*

               when Command_UNLOCK => 
                  -- *If secondtoken = Master_Pin  then*
                  Is_Locked := FALSE;

               when others =>
                  null;

            end case;

         


         end if;
      end;
   end loop;
   

   

 

   
   
end Main;





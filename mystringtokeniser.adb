
package body MyStringTokeniser with SPARK_Mode is

   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) is
      Index : Positive;
      Extent : TokenExtent;
      OutIndex : Integer := Tokens'First;
   begin
      Count := 0;
      if (S'First > S'Last) then
         return;
      end if;
      Index := S'First;
      while OutIndex <= Tokens'Last and Index <= S'Last and Count < Tokens'Length loop

         -- This loop invariant has been more or less explained in mystringtokeniser.ads
         pragma Loop_Invariant
           (for all J in Tokens'First..OutIndex-1 =>
              (Tokens(J).Start >= S'First and
                   Tokens(J).Length > 0) and then
            Tokens(J).Length-1 <= S'Last - Tokens(J).Start);
         
         -- Conditions that loop invariant must satisfy:
         --     1. It must be true before the loop.  
         --     2. It must be true while the loop is in progress.  
         --     3. It must be true after the loop. 
         -- This loop variant satisfies all three conditions.
         --
         -- This loop invariant is necessary to explicitly map the relation between
         -- Tokens'First, Count and OutIndex, which ensures that tokens are written 
         -- to Tokens in the order which they are processed. Without this invariant,
         -- given a situation with input string "t1 t2 t3" and Token array T, then
         -- a post situation where T = (t3, t1, t2) would pass. This loop invariant 
         -- ensures only T = (t1, t2, t3) would be a valid post situation.
         pragma Loop_Invariant (OutIndex = Tokens'First + Count);

         -- look for start of next token
         while (Index >= S'First and Index < S'Last) and then Is_Whitespace(S(Index)) loop
            Index := Index + 1;
         end loop;
         if (Index >= S'First and Index <= S'Last) and then not Is_Whitespace(S(Index)) then
            -- found a token
            Extent.Start := Index;
            Extent.Length := 0;

            -- look for end of this token
            while Positive'Last - Extent.Length >= Index and then (Index+Extent.Length >= S'First and Index+Extent.Length <= S'Last) and then not Is_Whitespace(S(Index+Extent.Length)) loop
               Extent.Length := Extent.Length + 1;
            end loop;

            Tokens(OutIndex) := Extent;
            Count := Count + 1;

            -- check for last possible token, avoids overflow when incrementing OutIndex
            if (OutIndex = Tokens'Last) then
               return;
            else
               OutIndex := OutIndex + 1;
            end if;

            -- check for end of string, avoids overflow when incrementing Index
            if S'Last - Extent.Length < Index then
               return;
            else
               Index := Index + Extent.Length;
            end if;
         end if;
      end loop;
   end Tokenise;

end MyStringTokeniser;

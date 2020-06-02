with Ada.Characters.Latin_1;

package MyStringTokeniser with SPARK_Mode is

   type TokenExtent is record
      Start : Positive;
      Length : Natural;
   end record;

   type TokenArray is array(Positive range <>) of TokenExtent;

   function Is_Whitespace(Ch : Character) return Boolean is
     (Ch = ' ' or Ch = Ada.Characters.Latin_1.LF or
        Ch = Ada.Characters.Latin_1.HT);

   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) with
     Pre => (if S'Length > 0 then S'First <= S'Last) and Tokens'First <= Tokens'Last,
     Post => Count <= Tokens'Length and
     (for all Index in Tokens'First..Tokens'First+(Count-1) =>
          (Tokens(Index).Start >= S'First and
          Tokens(Index).Length > 0) and then
            Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start);

      -- Count <= Tokens'Length: This says that 'Count', the variable that tracks the number
      -- of tokens generated in Tokenise, does not exceed 'Tokens'Length', the length of 
      -- the array inputted to store Tokens. Necessary since Count > Tokens'Length implies
      -- that more tokens have been generated than Tokens can store, thus there is a memory leak.

      -- For all Index in Tokens'First..Tokens'First+(Count-1): For all filled indexes in the 
      -- Token array. Necessary so all tokens are checked, instead of a subset or NULL elements.
 
      -- Tokens(Index).Start >= S'First: This says that the starting index of Tokens(index) string lies
      -- somewhere after the starting index of the input string, S'First. Necessary to ensure all tokens
      -- began within the input string S.

      -- Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start: This says that the length of Tokens(Index) 
      -- is not greater than S'Last - Tokens(Index).Start + 1. Necessary to ensure no token exceeded the 
      -- length of the input string S. Combined with previous condition, ensures all tokens are entirely 
      -- derived from the input string S.

      -- Tokens(Index).Length > 0: This says that the length fo Tokens(Index) is a value greater than 0, 
      -- this ensures all tokens feature non-empty substrings of the input string S.
   

end MyStringTokeniser;

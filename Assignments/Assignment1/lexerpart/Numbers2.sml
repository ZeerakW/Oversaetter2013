local open Obj Lexing in


  (* Above the rules, there is a section for SML code. 

     Helper functions, declarations, and necessary "open" statements
     go here.
   *)
   open Lexing (* getLexemeStart, lexbuf, ... see documentation *)

   exception LexicalError of string * int (* position *)

   fun lexerError lexbuf s = 
       raise LexicalError (s, getLexemeStart lexbuf);

   (* Token type, often defined by the next phase *)
   datatype TokenType 
     = Decimal of int | Octal of int | Hex of int
     | Float of real | Scientific of real
     | Symbol of string
     | EOF

   (* helper functions *)
   fun decodeInt lexbuf 
     = let val s = getLexeme lexbuf 
       in case Int.fromString s of 
              NONE   => lexerError lexbuf ( s ^ ": not an int.")
            | SOME n => n
       end

   fun decodeFloat lexbuf
     = let val s = getLexeme lexbuf 
       in case Real.fromString s of 
              NONE   => lexerError lexbuf ( s ^ ": not a float.")
            | SOME x => x
       end

   (* octal and hex need more involved helper functions *)

   fun value (c : char) : int (* return digit value, assume correct input *)
     = let val v = Char.ord c
       in if v >= 48 andalso v <= 57 (* 0 - 9 *) then v - 48
          else if v >= 65 andalso v <= 70 (* A - F *) then v - 55
          else if v >= 97 andalso v <= 102 (* a - f *) then v - 87
          else raise LexicalError ("Illegal digit in conversion", 0)
       end

   fun decodeInBase (base : int) (s : string) : int
     = let fun acc ( c : char, acc : int) : int = acc*base + value c
       in List.foldl acc 0 (String.explode s)
       end

   (* "lex" will later be the generated function "Token" *)
   fun repeat lex b = let val res = lex b
                      in res :: (if res = EOF then [] else repeat lex b)
                      end

   fun Scan lex s = let val buf = createLexerString s
                    in repeat lex buf
                    end
        handle LexicalError (msg,pos) 
           => (TextIO.output (TextIO.stdErr, msg ^ makestring pos ^"\n");[])

   (* what follows is the set of token definitions and actions. Format:

   regular_expression      { returned token, probably using "lexbuf" }

    *)

fun action_10 lexbuf = (
 lexerError lexbuf ("Lexical error at input `"^getLexeme lexbuf^ "`") )
and action_9 lexbuf = (
 EOF )
and action_8 lexbuf = (
 Scientific (decodeFloat lexbuf) )
and action_7 lexbuf = (
 Float (decodeFloat lexbuf) )
and action_6 lexbuf = (
 Symbol (getLexeme lexbuf) )
and action_5 lexbuf = (
 hexToken lexbuf )
and action_4 lexbuf = (
 Octal (decodeInBase 8 (getLexeme lexbuf)) )
and action_3 lexbuf = (
 Decimal (decodeInt lexbuf) )
and action_2 lexbuf = (
 Token lexbuf )
and action_1 lexbuf = (
lexerError lexbuf ("Hex digit expected, found " ^ getLexeme lexbuf) )
and action_0 lexbuf = (
 Hex (decodeInBase 16 (getLexeme lexbuf))  )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_19 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_19 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_19 lexbuf
 else case currChar of
    #"\^@" => backtrack lexbuf
 |  _ => action_1 lexbuf
 end)
and state_1 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"1" andalso currChar <= #"9" then  state_8 lexbuf
 else case currChar of
    #"+" => action_6 lexbuf
 |  #"*" => action_6 lexbuf
 |  #"-" => action_6 lexbuf
 |  #"/" => action_6 lexbuf
 |  #"\n" => action_2 lexbuf
 |  #"\t" => action_2 lexbuf
 |  #"\r" => action_2 lexbuf
 |  #" " => action_2 lexbuf
 |  #"0" => state_7 lexbuf
 |  #"." => state_6 lexbuf
 |  #"\^@" => action_9 lexbuf
 |  _ => action_10 lexbuf
 end)
and state_6 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_10);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else backtrack lexbuf
 end)
and state_7 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_3);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"7" then  state_15 lexbuf
 else case currChar of
    #"X" => action_5 lexbuf
 |  #"x" => action_5 lexbuf
 |  #"E" => state_11 lexbuf
 |  #"e" => state_11 lexbuf
 |  #"9" => state_16 lexbuf
 |  #"8" => state_16 lexbuf
 |  #"." => state_9 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_8 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_3);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_10 lexbuf
 else case currChar of
    #"E" => state_11 lexbuf
 |  #"e" => state_11 lexbuf
 |  #"." => state_9 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_9 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_7);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else case currChar of
    #"E" => state_11 lexbuf
 |  #"e" => state_11 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_10 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_3);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_10 lexbuf
 else case currChar of
    #"E" => state_11 lexbuf
 |  #"e" => state_11 lexbuf
 |  #"." => state_9 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_11 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_13 lexbuf
 else case currChar of
    #"+" => state_12 lexbuf
 |  #"-" => state_12 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_12 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_13 lexbuf
 else backtrack lexbuf
 end)
and state_13 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_8);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_13 lexbuf
 else backtrack lexbuf
 end)
and state_14 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_7);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else case currChar of
    #"E" => state_11 lexbuf
 |  #"e" => state_11 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_15 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_4);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"7" then  state_15 lexbuf
 else case currChar of
    #"E" => state_11 lexbuf
 |  #"e" => state_11 lexbuf
 |  #"9" => state_16 lexbuf
 |  #"8" => state_16 lexbuf
 |  #"." => state_9 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_16 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_16 lexbuf
 else case currChar of
    #"E" => state_11 lexbuf
 |  #"e" => state_11 lexbuf
 |  #"." => state_9 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_19 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_20 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_20 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_20 lexbuf
 else backtrack lexbuf
 end)
and state_20 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_20 lexbuf
 else if currChar >= #"A" andalso currChar <= #"F" then  state_20 lexbuf
 else if currChar >= #"a" andalso currChar <= #"f" then  state_20 lexbuf
 else backtrack lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_1 lexbuf)

and hexToken lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_10, action_9, action_8, action_7, action_6, action_5, action_4, action_3, action_2];
val _ = fn _ => [action_1, action_0];

end

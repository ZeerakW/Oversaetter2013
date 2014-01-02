{
	open Lexing

	exception LexicalError  of string * int (* position *)

	fun lexerError lexbuf s =
		raise LexicalError (s, getLexemeStart lexbuf);

	(* Tokentype *)
	datatype TokenType 
	    = a1 of string | a2 of string | a3 of string | EOF;


	fun repeat lex b = let val res = lex b
                      in res :: (if res = EOF then [] else repeat lex b)
                      end

   	fun Scan lex s = let val buf = createLexerString s
                    in repeat lex buf
                    end
        handle LexicalError (msg,pos) 
           => (TextIO.output (TextIO.stdErr, msg ^ makestring pos ^"\n");[])

}

rule Token = parse
	[` ` `\t` `\n` `\r`]		(* Ignore spacing *)
					{ Token lexbuf }
	| `a``b`*			{a1 (getLexeme lexbuf) }
	| `a`*`b`			{a2 (getLexeme lexbuf) }
	| (`a``b`)*`c`			{a3 (getLexeme lexbuf) }
	|eof                        { EOF }
	| _ { lexerError lexbuf ("Lexical error at input `"^getLexeme lexbuf^ "`") };

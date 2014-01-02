local
in
datatype token =
    aChar of string
end;

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

val yytransl = #[
  257 (* aChar *),
    0];

val yylhs = "\255\255\
\\001\000\001\000\000\000";

val yylen = "\002\000\
\\002\000\000\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\000\000\003\000\001\000";

val yydgoto = "\002\000\
\\004\000";

val yysindex = "\255\255\
\\000\255\000\000\000\255\000\000\000\000";

val yyrindex = "\000\000\
\\002\000\000\000\002\000\000\000\000\000";

val yygindex = "\000\000\
\\001\000";

val YYTABLESIZE = 4;
val yytable = "\001\000\
\\003\000\002\000\000\000\005\000";

val yycheck = "\001\000\
\\001\001\000\000\255\255\003\000";

val yyact = vector_ 4 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file rightrecursive.grm, line 6 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : string
val d__2__ = peekVal 0 : string list
in
( (d__1__) @ [(d__2__)] ) end : string list))
;
(* Rule 2, file rightrecursive.grm, line 7 *)
val _ = update_ yyact 2
(fn () => repr(let
in
( ) end : string list))
;
(* Entry S *)
val _ = update_ yyact 3 (fn () => raise yyexit (peekVal 0));
val yytables : parseTables =
  ( yyact,
    yytransl,
    yylhs,
    yylen,
    yydefred,
    yydgoto,
    yysindex,
    yyrindex,
    yygindex,
    YYTABLESIZE,
    yytable,
    yycheck );
fun S lexer lexbuf = yyparse yytables 1 lexer lexbuf;

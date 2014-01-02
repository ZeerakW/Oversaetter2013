local
in
datatype token =
    aChar of string
end;

val S :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> string list;

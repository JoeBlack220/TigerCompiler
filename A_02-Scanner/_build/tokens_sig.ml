
module type Tiger_TOKENS = sig
  type pos 
  type token
    = IF of pos
    | WHILE of pos
    | FOR of pos
    | TO of pos
    | BREAK of pos
    | LET of pos
    | IN of pos
    | END of pos
    | FUNCTION of pos
    | VAR of pos
    | TYPE of pos
    | ARRAY of pos
    | THEN of pos
    | ELSE of pos
    | DO of pos
    | OF of pos
    | NIL of pos
    | COMMA of pos
    | COLON of pos
    | SEMICOLON of pos
    | LPAREN of pos
    | RPAREN of pos
    | LBRACK of pos
    | RBRACK of pos
    | LBRACE of pos
    | RBRACE of pos
    | DOT of pos
    | PLUS of pos
    | MINUS of pos
    | TIMES of pos
    | DIVIDE of pos
    | EQ of pos
    | NEQ of pos
    | LT of pos
    | LE of pos
    | GT of pos
    | GE of pos
    | AND of pos
    | OR of pos
    | ASSIGN of pos
    | STRING of string * pos
    | INT of int * pos
    | ID of string * pos
    | EOF of pos
  val string_of_token : token -> string
end

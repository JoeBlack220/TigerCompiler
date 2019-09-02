
open Tokens_sig

module Tokens : Tokens_sig.Tiger_TOKENS with type pos = int = struct
  type pos = int
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

  let string_of_token : token -> string = function
    | IF p -> "IF   " ^ string_of_int p
    | WHILE p -> "WHILE   " ^ string_of_int p
    | FOR p -> "FOR   " ^ string_of_int p
    | TO p -> "TO   " ^ string_of_int p
    | BREAK p -> "BREAK   " ^ string_of_int p
    | LET p -> "LET   " ^ string_of_int p
    | IN p -> "IN   " ^ string_of_int p
    | END p -> "END   " ^ string_of_int p
    | FUNCTION p -> "FUNCTION   " ^ string_of_int p
    | VAR p -> "VAR   " ^ string_of_int p
    | TYPE p -> "TYPE   " ^ string_of_int p
    | ARRAY p -> "ARRAY   " ^ string_of_int p
    | THEN p -> "THEN   " ^ string_of_int p
    | ELSE p -> "ELSE   " ^ string_of_int p
    | DO p -> "DO   " ^ string_of_int p
    | OF p -> "OF   " ^ string_of_int p
    | NIL p -> "NIL   " ^ string_of_int p
    | COMMA p -> "COMMA   " ^ string_of_int p
    | COLON p -> "COLON   " ^ string_of_int p
    | SEMICOLON p -> "SEMICOLON   " ^ string_of_int p
    | LPAREN p -> "LPAREN   " ^ string_of_int p
    | RPAREN p -> "RPAREN   " ^ string_of_int p
    | LBRACK p -> "LBRACK   " ^ string_of_int p
    | RBRACK p -> "RBRACK   " ^ string_of_int p
    | LBRACE p -> "LBRACE   " ^ string_of_int p
    | RBRACE p -> "RBRACE   " ^ string_of_int p
    | DOT p -> "DOT   " ^ string_of_int p
    | PLUS p -> "PLUS   " ^ string_of_int p
    | MINUS p -> "MINUS   " ^ string_of_int p
    | TIMES p -> "TIMES   " ^ string_of_int p
    | DIVIDE p -> "DIVIDE   " ^ string_of_int p
    | EQ p -> "EQ   " ^ string_of_int p
    | NEQ p -> "NEQ   " ^ string_of_int p
    | LT p -> "LT   " ^ string_of_int p
    | LE p -> "LE   " ^ string_of_int p
    | GT p -> "GT   " ^ string_of_int p
    | GE p -> "GE   " ^ string_of_int p
    | AND p -> "AND   " ^ string_of_int p
    | OR p -> "OR   " ^ string_of_int p
    | ASSIGN p -> "ASSIGN   " ^ string_of_int p
    | STRING (s,p) -> "STRING(" ^ s ^ ")   " ^ string_of_int p
    | INT (i,p) -> "INT(" ^ string_of_int i ^ ")   " ^ string_of_int p
    | ID (s,p) -> "ID(" ^ s ^ ")   " ^ string_of_int p
    | EOF p -> "EOF   " ^ string_of_int p
end

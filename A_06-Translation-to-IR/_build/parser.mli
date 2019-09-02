type token =
  | INT of (int * Analysis.Ab.pos)
  | STRING of (string * Analysis.Ab.pos)
  | ID of (string * Analysis.Ab.pos)
  | FOR of (Analysis.Ab.pos)
  | WHILE of (Analysis.Ab.pos)
  | BREAK of (Analysis.Ab.pos)
  | LET of (Analysis.Ab.pos)
  | IN of (Analysis.Ab.pos)
  | NIL of (Analysis.Ab.pos)
  | TO of (Analysis.Ab.pos)
  | END of (Analysis.Ab.pos)
  | FUNCTION of (Analysis.Ab.pos)
  | VAR of (Analysis.Ab.pos)
  | TYPE of (Analysis.Ab.pos)
  | ARRAY of (Analysis.Ab.pos)
  | IF of (Analysis.Ab.pos)
  | THEN of (Analysis.Ab.pos)
  | ELSE of (Analysis.Ab.pos)
  | DO of (Analysis.Ab.pos)
  | OF of (Analysis.Ab.pos)
  | LPAREN of (Analysis.Ab.pos)
  | RPAREN of (Analysis.Ab.pos)
  | LBRACK of (Analysis.Ab.pos)
  | RBRACK of (Analysis.Ab.pos)
  | LBRACE of (Analysis.Ab.pos)
  | RBRACE of (Analysis.Ab.pos)
  | DOT of (Analysis.Ab.pos)
  | COLON of (Analysis.Ab.pos)
  | COMMA of (Analysis.Ab.pos)
  | SEMICOLON of (Analysis.Ab.pos)
  | PLUS of (Analysis.Ab.pos)
  | MINUS of (Analysis.Ab.pos)
  | TIMES of (Analysis.Ab.pos)
  | DIVIDE of (Analysis.Ab.pos)
  | EQ of (Analysis.Ab.pos)
  | NEQ of (Analysis.Ab.pos)
  | LT of (Analysis.Ab.pos)
  | LE of (Analysis.Ab.pos)
  | GT of (Analysis.Ab.pos)
  | GE of (Analysis.Ab.pos)
  | AND of (Analysis.Ab.pos)
  | OR of (Analysis.Ab.pos)
  | ASSIGN of (Analysis.Ab.pos)
  | UMINUS of (Analysis.Ab.pos)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Analysis.Ab.exp
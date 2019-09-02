type token =
  | INT of (int * Absyn.Absyn.pos)
  | STRING of (string * Absyn.Absyn.pos)
  | ID of (string * Absyn.Absyn.pos)
  | FOR of (Absyn.Absyn.pos)
  | WHILE of (Absyn.Absyn.pos)
  | BREAK of (Absyn.Absyn.pos)
  | LET of (Absyn.Absyn.pos)
  | IN of (Absyn.Absyn.pos)
  | NIL of (Absyn.Absyn.pos)
  | TO of (Absyn.Absyn.pos)
  | END of (Absyn.Absyn.pos)
  | FUNCTION of (Absyn.Absyn.pos)
  | VAR of (Absyn.Absyn.pos)
  | TYPE of (Absyn.Absyn.pos)
  | ARRAY of (Absyn.Absyn.pos)
  | IF of (Absyn.Absyn.pos)
  | THEN of (Absyn.Absyn.pos)
  | ELSE of (Absyn.Absyn.pos)
  | DO of (Absyn.Absyn.pos)
  | OF of (Absyn.Absyn.pos)
  | LPAREN of (Absyn.Absyn.pos)
  | RPAREN of (Absyn.Absyn.pos)
  | LBRACK of (Absyn.Absyn.pos)
  | RBRACK of (Absyn.Absyn.pos)
  | LBRACE of (Absyn.Absyn.pos)
  | RBRACE of (Absyn.Absyn.pos)
  | DOT of (Absyn.Absyn.pos)
  | COLON of (Absyn.Absyn.pos)
  | COMMA of (Absyn.Absyn.pos)
  | SEMICOLON of (Absyn.Absyn.pos)
  | PLUS of (Absyn.Absyn.pos)
  | MINUS of (Absyn.Absyn.pos)
  | TIMES of (Absyn.Absyn.pos)
  | DIVIDE of (Absyn.Absyn.pos)
  | EQ of (Absyn.Absyn.pos)
  | NEQ of (Absyn.Absyn.pos)
  | LT of (Absyn.Absyn.pos)
  | LE of (Absyn.Absyn.pos)
  | GT of (Absyn.Absyn.pos)
  | GE of (Absyn.Absyn.pos)
  | AND of (Absyn.Absyn.pos)
  | OR of (Absyn.Absyn.pos)
  | ASSIGN of (Absyn.Absyn.pos)
  | UMINUS of (Absyn.Absyn.pos)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absyn.Absyn.exp
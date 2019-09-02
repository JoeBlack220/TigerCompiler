{
open Tokens_sig
open Tokens_mod
open Tokens
open Errormsg

module Scanner = struct

let linenum = ErrorMsg.linenum
let linepos = ErrorMsg.linepos
}

rule scan = parse
  | "if"
    { (IF (Lexing.lexeme_start lexbuf), lexbuf) }

  | "while"
    { (WHILE (Lexing.lexeme_start lexbuf), lexbuf) }
  
  | "for"
    { (FOR (Lexing.lexeme_start lexbuf), lexbuf) }

  | "to"
    { (TO (Lexing.lexeme_start lexbuf), lexbuf) }

  | "break"
    { (BREAK (Lexing.lexeme_start lexbuf), lexbuf) }
  
  | "let"
    { (LET (Lexing.lexeme_start lexbuf), lexbuf) }

  | "in"
    { (IN (Lexing.lexeme_start lexbuf), lexbuf) }

  | "end"
    { (END (Lexing.lexeme_start lexbuf), lexbuf) }

  | "function"
    { (FUNCTION (Lexing.lexeme_start lexbuf), lexbuf) }
 
  | "var"
    { (VAR (Lexing.lexeme_start lexbuf), lexbuf) }

  | "type"
    { (TYPE (Lexing.lexeme_start lexbuf), lexbuf) }

  | "array"
    { (ARRAY (Lexing.lexeme_start lexbuf), lexbuf) }

  | "then"
    { (THEN (Lexing.lexeme_start lexbuf), lexbuf) }

  | "else"
    { (ELSE (Lexing.lexeme_start lexbuf), lexbuf) }

  | "do"
    { (DO (Lexing.lexeme_start lexbuf), lexbuf) }

  | "of"
    { (OF (Lexing.lexeme_start lexbuf), lexbuf) }
  
  | "nil"
    { (NIL (Lexing.lexeme_start lexbuf), lexbuf) }

  | ","
    { (COMMA (Lexing.lexeme_start lexbuf), lexbuf) }

  | ":"
    { (COLON (Lexing.lexeme_start lexbuf), lexbuf) }

  | ";"
    { (SEMICOLON (Lexing.lexeme_start lexbuf), lexbuf) }

  | "("
    { (LPAREN (Lexing.lexeme_start lexbuf), lexbuf) }

  | ")"
    { (RPAREN (Lexing.lexeme_start lexbuf), lexbuf) }

  | "["
    { (LBRACK (Lexing.lexeme_start lexbuf), lexbuf) }

  | "]"
    { (RBRACK (Lexing.lexeme_start lexbuf), lexbuf) }

  | "{"
    { (LBRACE (Lexing.lexeme_start lexbuf), lexbuf) }

  | "}"
    { (RBRACE (Lexing.lexeme_start lexbuf), lexbuf) }

  | "."
    { (DOT (Lexing.lexeme_start lexbuf), lexbuf) }

  | "+"
    { (PLUS (Lexing.lexeme_start lexbuf), lexbuf) }

  | "-"
    { (MINUS (Lexing.lexeme_start lexbuf), lexbuf) }

  | "*"
    { (TIMES (Lexing.lexeme_start lexbuf), lexbuf) }

  | "/"
     { (DIVIDE (Lexing.lexeme_start lexbuf), lexbuf) }
  
  | "="
    { (EQ (Lexing.lexeme_start lexbuf), lexbuf) }

  | "<>"
    { (NEQ (Lexing.lexeme_start lexbuf), lexbuf) }

  | "<="
    { (LE (Lexing.lexeme_start lexbuf), lexbuf) }

  | ">="
    { (GE (Lexing.lexeme_start lexbuf), lexbuf) }

  | "<"
    { (LT (Lexing.lexeme_start lexbuf), lexbuf) }

  | ">"
    { (GT (Lexing.lexeme_start lexbuf), lexbuf) }

  | "&"
    { (AND (Lexing.lexeme_start lexbuf), lexbuf) }

  | "|"
    { (OR (Lexing.lexeme_start lexbuf), lexbuf) }

  | ":="
    { (ASSIGN (Lexing.lexeme_start lexbuf), lexbuf) }

  | ['0'-'9']+
    { (INT (int_of_string (Lexing.lexeme lexbuf), 
            (Lexing.lexeme_start lexbuf)), lexbuf) }

  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*
    { (ID ((Lexing.lexeme lexbuf), (Lexing.lexeme_start lexbuf)),lexbuf) }

  | "/*"
    {comments 0 lexbuf}

  | '\n'
    { linenum := !linenum + 1;
      linepos := Lexing.lexeme_start lexbuf :: !linepos;
      scan lexbuf }

  | [' ' '\t' '\r']
    {scan lexbuf }

  | "\""
    {(STRING(string_ "" lexbuf,(Lexing.lexeme_start lexbuf)), lexbuf)}

  | eof
    { (EOF (Lexing.lexeme_start lexbuf), lexbuf) }

  | _ 
    { ErrorMsg.error (Lexing.lexeme_start lexbuf) "illegal character";
      scan lexbuf }

and comments level = parse
  | "*/"
    { if level = 0 then scan lexbuf
    else comments (level-1) lexbuf}

  | "/*"
    {comments (level+1) lexbuf}

  | _
    {comments level lexbuf}

  | eof
    {ErrorMsg.error (Lexing.lexeme_start lexbuf) "unclosed commnets";
     (EOF (Lexing.lexeme_start lexbuf), lexbuf)}    

and string_ t = parse
  | "\""
    {t}

  | "\\t"
    {string_ (t^"\t") lexbuf}

  | "\\n"
    {string_ (t^"\n") lexbuf}

  | '\\' '\\'
    {string_ (t^"\\") lexbuf}
 
  | '\\' '"'
    {string_ (t^"\"") lexbuf}

  | '\\' ['\n''\r''\t'' ']* '\\'
    {string_ t lexbuf}

  | '\\' (['0'-'9'] ['0'-'9'] ['0'-'9'] as num)
    {string_ (t^ Char.escaped (Char.chr (int_of_string num))) lexbuf}

  | '\\''^' (['@' - '_'] as c)
    {string_ (t^ Char.escaped (Char.chr ((Char.code c) - 64))) lexbuf}

  | eof
    {ErrorMsg.error (Lexing.lexeme_start lexbuf) "unclosed strings";
     t}    
  | _
    {string_ (t^(Lexing.lexeme lexbuf)) lexbuf}



{ 
(* end of the scanner module *)
end
}

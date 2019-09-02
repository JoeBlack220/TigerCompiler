{
open Parser

open Errormsg

let linenum = ErrorMsg.linenum
let linepos = ErrorMsg.linepos
let commentdepth = ref 0
let stringpos = ref 0

exception Lexical_error
}

let digits = ['0'-'9']+

rule scan = parse
  | "if"
    { IF (Lexing.lexeme_start lexbuf)}

  | "while"
    { WHILE (Lexing.lexeme_start lexbuf) }
  
  | "for"
    { FOR (Lexing.lexeme_start lexbuf) }

  | "to"
    { TO (Lexing.lexeme_start lexbuf) }

  | "break"
    { BREAK (Lexing.lexeme_start lexbuf) }
  
  | "let"
    { LET (Lexing.lexeme_start lexbuf) }

  | "in"
    { IN (Lexing.lexeme_start lexbuf) }

  | "end"
    { END (Lexing.lexeme_start lexbuf)}

  | "function"
    { FUNCTION (Lexing.lexeme_start lexbuf) }
 
  | "var"
    { VAR (Lexing.lexeme_start lexbuf) }

  | "type"
    { TYPE (Lexing.lexeme_start lexbuf) }

  | "array"
    { ARRAY (Lexing.lexeme_start lexbuf) }

  | "then"
    { THEN (Lexing.lexeme_start lexbuf) }

  | "else"
    { ELSE (Lexing.lexeme_start lexbuf) }

  | "do"
    { DO (Lexing.lexeme_start lexbuf)}

  | "of"
    { OF (Lexing.lexeme_start lexbuf) }
  
  | "nil"
    { NIL (Lexing.lexeme_start lexbuf)}

  | ","
    { COMMA (Lexing.lexeme_start lexbuf)}

  | ":"
    { COLON (Lexing.lexeme_start lexbuf) }

  | ";"
    { SEMICOLON (Lexing.lexeme_start lexbuf) }

  | "("
    { LPAREN (Lexing.lexeme_start lexbuf)}

  | ")"
    { RPAREN (Lexing.lexeme_start lexbuf)}

  | "["
    { LBRACK (Lexing.lexeme_start lexbuf)}

  | "]"
    { RBRACK (Lexing.lexeme_start lexbuf)}

  | "{"
    { LBRACE (Lexing.lexeme_start lexbuf) }

  | "}"
    { RBRACE (Lexing.lexeme_start lexbuf)}

  | "."
    { DOT (Lexing.lexeme_start lexbuf)}

  | "+"
    { PLUS (Lexing.lexeme_start lexbuf)}

  | "-"
    { MINUS (Lexing.lexeme_start lexbuf)}

  | "*"
    { TIMES (Lexing.lexeme_start lexbuf)}

  | "/"
     { DIVIDE (Lexing.lexeme_start lexbuf)}
  
  | "="
    { EQ (Lexing.lexeme_start lexbuf)}

  | "<>"
    { NEQ (Lexing.lexeme_start lexbuf)}

  | "<="
    { LE (Lexing.lexeme_start lexbuf)}

  | ">="
    { GE (Lexing.lexeme_start lexbuf)}

  | "<"
    { LT (Lexing.lexeme_start lexbuf)}

  | ">"
    { GT (Lexing.lexeme_start lexbuf)}

  | "&"
    { AND (Lexing.lexeme_start lexbuf)}

  | "|"
    { OR (Lexing.lexeme_start lexbuf)}

  | ":="
    { ASSIGN (Lexing.lexeme_start lexbuf)}

  | ['0'-'9']+
    { INT (int_of_string (Lexing.lexeme lexbuf), 
            (Lexing.lexeme_start lexbuf))}

  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as lexeme
    { ID (lexeme, (Lexing.lexeme_start lexbuf)) }

  | "/*"
    {comments 0 lexbuf}

  | '\n'
    { linenum := !linenum + 1;
      linepos := Lexing.lexeme_start lexbuf :: !linepos;
      scan lexbuf }

  | [' ' '\t' '\r']+
    {scan lexbuf }

  | "\""
    {STRING(string_ "" lexbuf,(Lexing.lexeme_start lexbuf))}

  | eof
    { EOF }

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
     (EOF)}    

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
    {string_ (t^ String.make 1  (Char.chr (int_of_string num))) lexbuf}

  | '\\''^' (['@' - '_'] as c)
    {string_ (t^ String.make 1  (Char.chr ((Char.code c) - 64))) lexbuf}

  | eof
    {ErrorMsg.error (Lexing.lexeme_start lexbuf) "unclosed strings";
     t}    
  | _
    {string_ (t^(Lexing.lexeme lexbuf)) lexbuf}





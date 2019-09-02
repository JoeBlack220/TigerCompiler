%{
(* Header - OCaml declarations 
   Unfortunately, OCamlYacc places these declarations only in the
   generated `parser.ml` file and not in the generated `parser.mli`
   file.  Thus, we need fully qualified names in an declaration that
   appears in the `parser.mli` interface file.  
 *)

open Symbol
module A = Absyn.Absyn

%}

/* Declarations */

%token <int * Absyn.Absyn.pos> INT
%token <string * Absyn.Absyn.pos> STRING
%token <string * Absyn.Absyn.pos> ID
%token <Absyn.Absyn.pos> FOR WHILE BREAK LET IN NIL TO END
%token <Absyn.Absyn.pos> FUNCTION VAR TYPE ARRAY IF THEN ELSE DO OF
%token <Absyn.Absyn.pos> LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token <Absyn.Absyn.pos> DOT COLON COMMA SEMICOLON
%token <Absyn.Absyn.pos> PLUS MINUS TIMES DIVIDE
%token <Absyn.Absyn.pos> EQ NEQ LT LE GT GE
%token <Absyn.Absyn.pos> AND OR
%token <Absyn.Absyn.pos> ASSIGN
%token <Absyn.Absyn.pos> UMINUS
%token EOF

%type <A.exp> exp
%type <A.field> field
%type <A.ty> ty
%type <A.fundec> fundec
%type <A.typedec> typedec
%type <Absyn.Absyn.exp> program
%right SEMICOLON
%left OR
%left AND
%nonassoc ELSE DO
%nonassoc ASSIGN
%nonassoc EQ NEQ GT LT GE LE
%left PLUS MINUS
%left TIMES DIVIDE
%left UMINUS

%start program

%%

/* Rules */
program:
  exp EOF { $1 }


exp:
  INT { A.IntExp (fst $1) }
| NIL { A.NilExp }
| STRING {A.StringExp (fst $1, snd $1)}
/*| exp expseq_rest {A.SeqExp (($1,Parsing.rhs_start 1)::$2)} */
| sequence {$1}
| id LBRACK exp RBRACK OF exp { A.ArrayExp{typ = $1; size = $3; init = $6; pos = Parsing.rhs_start 1}}
| id LBRACE field_values RBRACE { A.RecordExp {fields = $3; typ = $1; pos = Parsing.rhs_start 1}}
| lvalue {A.VarExp($1)}
| lvalue ASSIGN exp {A.AssignExp{var = $1;exp = $3;pos = Parsing.rhs_start 1}}
| id LPAREN funcargs RPAREN { A.CallExp {func = $1; args = $3; pos = Parsing.rhs_start 1} }
| MINUS exp %prec UMINUS { A.OpExp {left = A.IntExp 0; oper = A.MinusOp; right = $2;
                 pos = $1 } }
| exp PLUS exp { A.OpExp {left = $1; oper = A.PlusOp; right = $3;
                 pos = $2 } }
| exp MINUS exp { A.OpExp {left = $1; oper = A.MinusOp; right = $3;
                 pos = $2 } }
| exp TIMES exp { A.OpExp {left = $1; oper = A.TimesOp; right = $3;
                 pos = $2 } }
| exp DIVIDE exp { A.OpExp {left = $1; oper = A.DivideOp; right = $3;
                 pos = $2 } }
| exp EQ exp { A.OpExp {left = $1; oper = A.EqOp; right = $3;
                 pos = $2 } }
| exp NEQ exp { A.OpExp {left = $1; oper = A.NeqOp; right = $3;
                 pos = $2 } }
| exp LT exp { A.OpExp {left = $1; oper = A.LtOp; right = $3;
                 pos = $2 } }
| exp LE exp { A.OpExp {left = $1; oper = A.LeOp; right = $3;
                 pos = $2 } }
| exp GT exp { A.OpExp {left = $1; oper = A.GtOp; right = $3;
                 pos = $2 } }
| exp GE exp { A.OpExp {left = $1; oper = A.GeOp; right = $3;
                 pos = $2 } }
| exp AND exp { A.IfExp{test=$1; then' = $3; else' =  Some (A.IntExp 0); pos = Parsing.rhs_start 1} }
| exp OR exp { A.IfExp{test=$1; then' = A.IntExp 1; else' = Some($3); pos = Parsing.rhs_start 1} }
| IF exp THEN exp ELSE exp { A.IfExp {test = $2; then' = $4; else' = Some ($6); pos = $1} }
| IF exp THEN exp { A.IfExp {test = $2; then' = $4; else' = None; pos = $1} }
| WHILE exp DO exp { A.WhileExp { test = $2; body = $4; pos = $1 } }
| FOR id ASSIGN exp TO exp DO exp { A.ForExp{ var = $2; escape = ref true; lo = $4; hi = $6; body = $8;pos = $1 } }
| BREAK { A.BreakExp ($1)}
| LET decs IN sequenceExp END { A.LetExp{decs=$2; body = A.SeqExp($4); pos = $1}}
| LET decs IN exp END { A.LetExp{decs=$2; body = $4; pos = $1}}
| LPAREN exp RPAREN {$2} 
| LPAREN RPAREN {A.SeqExp([])} 

;


arrexp:
  id LBRACK exp RBRACK OF exp { A.ArrayExp{typ = $1; size = $3; init = $6; pos = Parsing.rhs_start 1}}
;
subscrivar:
  lvalue LBRACK exp RBRACK {A.SubscriptVar($1, $3, Parsing.rhs_start 1) }
;

id:
  ID {Symbol.symbol (fst $1)}
;

field_values:
            {[]}
| id EQ exp { ($1, $3, Parsing.rhs_start 1)::[] }
| id EQ exp COMMA field_values {($1, $3, Parsing.rhs_start 1)::$5}
;

/*
lvalue:
  id {A.SimpleVar($1, Parsing.rhs_start 1)}
| lvalue DOT id { A.FieldVar($1, $3, Parsing.rhs_start 1) }
| lvalue LBRACK exp RBRACK {A.SubscriptVar($1, $3, Parsing.rhs_start 1) }
;
*/
lvalue:
  id {A.SimpleVar($1, Parsing.rhs_start 1)}
| lvalue_not_id {$1}
;
lvalue_not_id:
  lvalue DOT id { A.FieldVar($1, $3, Parsing.rhs_start 1) }
| id LBRACK exp RBRACK {A.SubscriptVar(A.SimpleVar($1, Parsing.rhs_start 1), $3, Parsing.rhs_start 1) }
| lvalue_not_id LBRACK exp RBRACK {A.SubscriptVar($1, $3, Parsing.rhs_start 1) }
;

field:
  id COLON id { {fname = $1; escape = ref true; typ = $3; fpos = Parsing.rhs_start 1} }
;

fields:
  {[]}
| field fields_rest { $1::$2 }
;

fields_rest:
  {[]}
| COMMA field fields_rest { $2::$3 }
;


funcargs:
  { [] }
| exp { $1::[] }
| exp COMMA funcargs_rest { $1::$3 }
;

funcargs_rest:
  exp { $1::[] }
| exp COMMA funcargs_rest { $1::$3 }
;

sequence:
  LPAREN sequenceExp RPAREN {A.SeqExp($2)}

sequenceExp:
| exp seqtail {($1, Parsing.rhs_start 1)::$2}

seqtail:
  {[]}
| SEMICOLON exp seqtail {($2, Parsing.rhs_start 2)::$3}


fundec:
  FUNCTION id LPAREN fields RPAREN EQ exp {{ fnname = $2; params = $4; result = None; body = $7; fnpos = $1 }} 
| FUNCTION id LPAREN fields RPAREN COLON id EQ exp {{ fnname = $2; params = $4; result = Some($7, Parsing.rhs_start 7); body = $9; fnpos = $1 }} 
;

vardec:
  VAR id ASSIGN exp { A.VarDec{name = $2; escape = ref true; typ = None; init = $4; pos = $1} }
| VAR id COLON id ASSIGN exp { A.VarDec{name = $2; escape = ref true; typ = Some($4,Parsing.rhs_start 4); init = $6; pos = $1} }
;

typedec:
  TYPE id EQ ty {{ tname = $2; ty = $4; tpos = $1} }
;

ty:
  id { A.NameTy($1, Parsing.rhs_start 1) }
| LBRACE fields RBRACE { A.RecordTy($2)}
| ARRAY OF id {A.ArrayTy($3, Parsing.rhs_start 1)} 
;

decs:
  vardecslist  {$1}
| fundecslist {$1}
| typedecslist {$1}
;


vardecslist:
  vardec {$1::[]}
| vardec vardecslist {$1::$2}
| vardec typedecslist {$1::$2}
| vardec fundecslist {$1::$2}


typedecslist:
  mutualtypedecs {$1::[]}
| mutualtypedecs typedecslist { $1::$2 }
| mutualtypedecs vardecslist { $1::$2 }
| mutualtypedecs fundecslist { $1::$2 }
;

fundecslist:
  mutualfundecs {$1::[]}
| mutualfundecs fundecslist { $1::$2 }
| mutualfundecs vardecslist { $1::$2 }
| mutualfundecs typedecslist { $1::$2 }
;
mutualtypedecs:
  typedecs { A.TypeDec($1)}
;
mutualfundecs:
  fundecs { A.FunctionDec($1) }
;
fundecs:
  fundec fundecs {$1::$2}
| fundec {$1::[]}
;
typedecs:
  typedec typedecs {$1::$2}
| typedec {$1::[]}
;
expseq_rest:
  SEMICOLON exp {($2, Parsing.rhs_start 1)::[]}
| SEMICOLON exp expseq_rest {($2, Parsing.rhs_start 1)::$3}
;

%%
(* Trailer - OCaml declarations *)

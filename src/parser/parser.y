%define parse.error verbose

%{
#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"

extern int linenum;
%}

%token T_and       "and"
%token T_as        "as"
%token T_begin     "begin"
%token T_break     "break"
%token T_byte      "byte"
%token T_continue  "continue"
%token T_decl      "decl"
%token T_def       "def"
%token T_elif      "elif"
%token T_else      "else"
%token T_end       "end"
%token T_exit      "exit"
%token T_false     "false"
%token T_if        "if"
%token T_is        "is"
%token T_int       "int"
%token T_loop      "loop"
%token T_not       "not"
%token T_or        "or"
%token T_ref       "ref"
%token T_return    "return"
%token T_skip      "skip"
%token T_true      "true"
%token T_var       "var"

%token T_assign    ":="
%token T_eq        "="
%token T_neq       "<>"
%token T_lt        "<"
%token T_gt        ">"
%token T_le        "<="
%token T_ge        ">="
%token T_plus      "+"
%token T_minus     "-"
%token T_times     "*"
%token T_div       "/"
%token T_mod       "%"
    
%token T_id        
%token T_string    
%token T_intconst
%token T_charconst

%left "or"
%left "and" 
%right "not"
%nonassoc "=" "<>" "<=" ">=" "<" ">"
%left "+" "-" '|'
%left "*" "/" "%" '&'
%right '!' UMINUS UPLUS

%%

program:
    func_def
;

func_def:
    "def" header local_def block
;

header:
    T_id
|   T_id "is" data_type
|   T_id "is" data_type ':' fpar_def header_aux
|   T_id ':' fpar_def header_aux 
;

header_aux:
    /* empty */
|   ',' fpar_def header_aux
;

fpar_def:
    id_aux "as" fpar_type
;

data_type:
    "int"
|   "byte"
;

type:
    data_type
|   type '[' T_intconst ']'
;

fpar_type:
    type
|   "ref" data_type
|   array_type
;

array_type:
    data_type '[' ']'
|   array_type '[' T_intconst ']'
;

local_def:
    /* empty */
|   func_def local_def
|   func_decl local_def
|   var_def local_def
;

func_decl:
    "decl" header
;

var_def:
    "var" id_aux "is" type
;

id_aux:
    T_id
|   id_aux T_id
;

stmt:
    simple_stmt
|   stmt_list
;

stmt_list:
    simple_stmt simple_stmt
|   stmt_list simple_stmt
;

simple_stmt:
    "skip"
|   l_value ":=" expr
|   proc_call
|   "exit"
|   "return" ':' expr
|   "if" cond ':' block
|   "if" cond ':' block if_aux "else" ':' block
|   "loop" ':' block
|   "loop" T_id ':' block
|   "break"
|   "break" ':' T_id
|   "continue"
|   "continue" ':' T_id
;

if_aux:
    /* empty */
|   "elif" cond ':' block if_aux
;

block:
    "begin" stmt "end"
|   stmt "end"
;

proc_call:
    T_id
|   T_id ':' expr expr_aux
;

expr_aux:
    /* empty */
|   ',' expr expr_aux
;

func_call:
    T_id '(' ')'
|   T_id '(' expr expr_aux ')'
;

l_value:
    T_id
|   T_string
|   l_value '[' expr ']'
;

expr:
    T_intconst
|   T_charconst
|   l_value
|   '(' expr ')'
|   func_call
|   "-" expr %prec UMINUS
|   "+" expr %prec UPLUS
|   expr "+" expr
|   expr "-" expr
|   expr "*" expr
|   expr "/" expr
|   expr "%" expr
|   "true"
|   "false"
|   '!' expr
|   expr '&' expr
|   expr '|' expr
;

cond:
    expr
|   cond_aux
;

cond_aux:
    '(' cond_aux ')'
|   "not" cond
|   cond "and" cond
|   cond "or" cond
|   expr "=" expr
|   expr "<>" expr
|   expr "<" expr
|   expr ">" expr
|   expr "<=" expr
|   expr ">=" expr
;   

%%

void yyerror (const char *msg) {
  fprintf(stderr, "Syntax error: %s in line %d\n", msg, linenum);
  exit(42);
}


int main() { 
    int res = yyparse();
    if (res == 0) 
        printf("Parsing completed successfully.\n");
    return res;
}
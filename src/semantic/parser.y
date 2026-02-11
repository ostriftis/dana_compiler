%define parse.error verbose

%{
#include <string>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <string>
#include "ast.hpp"
#include "lexer.hpp"


extern int linenum;
SymbolTable st;
LoopStack lp;
%}


%union {
    char* var;
    int num;
    char character;
    char* string;
    char* op;
    Stmt *stmt;
    SimpleStmt *simple_stmt;
    Expr *expr;
    L_val *l_val;
    ExprAux *expr_aux;
    FuncCall *func_call;
    Cond *cond;
    ProcCall *proc_call;
    IfAux *if_aux;
    StmtList *stmt_list;
    Block *block;
    Header *header;
    DataType data_type;
    Type *type;
    IdAux *id_aux;
    VarDef *var_def;
    ArrayType *array_type;
    FparType *fpar_type;
    FuncDecl *func_decl;
    FparDef *fpar_def;
    HeaderAux *header_aux;
    LocalDef *local_def;
    FuncDef *func_def;
}


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
    

%token<var> T_id        
%token<string>  T_string    
%token<num> T_intconst
%token<character> T_charconst

%left<op> "or"
%left<op> "and" 
%right<op> "not"
%nonassoc<op> "=" "<>" "<=" ">=" "<" ">"
%left<op> "+" "-" '|'
%left<op> "*" "/" "%" '&'
%right<op> '!' UMINUS UPLUS




%expect 1


%type<simple_stmt> simple_stmt
%type<expr> expr
%type<l_val> l_value
%type<expr_aux> expr_aux
%type<func_call> func_call
%type<cond> cond cond_aux
%type<proc_call> proc_call
%type<if_aux> if_aux
%type<stmt_list> stmt_list
%type<block> block
%type<header> header
%type<data_type> data_type
%type<type> type
%type<id_aux> id_aux
%type<var_def> var_def
%type<array_type> array_type
%type<fpar_type> fpar_type
%type<stmt> stmt
%type<func_decl> func_decl
%type<fpar_def> fpar_def
%type<header_aux> header_aux
%type<local_def> local_def
%type<func_def> func_def



%%

program:
    func_def                                      {std::cout << "AST: " << *$1 << std::endl;
                                                   st.enterScope(); $1->sem_analyze(); st.exitScope();}
;

func_def:
    "def" header local_def block                  {$$ = new FuncDef($2, $3, $4);}
;

header:
    T_id                                          {$$ = new Header(new Id($1), TYPE_void, nullptr);}
|   T_id "is" data_type                           {$$ = new Header(new Id($1), $3, nullptr);}
|   T_id "is" data_type ':' fpar_def header_aux   {$6->insert_front($5); $$ = new Header(new Id($1), $3, $6);}
|   T_id ':' fpar_def header_aux                  {$4->insert_front($3); $$ = new Header(new Id($1), TYPE_void, $4);}
;

header_aux:
    /* empty */                                   {$$ = new HeaderAux();}
|   header_aux ',' fpar_def                       {$1->append($3); $$ = $1;}
;

fpar_def:
    id_aux "as" fpar_type                         {$$ = new FparDef($1, $3);}
;

data_type:
    "int"                                         {$$ = TYPE_int;}
|   "byte"                                        {$$ = TYPE_byte;}
;

type:
    data_type                                     {$$ = new Type($1);}
|   type '[' T_intconst ']'                       {$1->append(new Int($3)); $$ = $1;}
;

fpar_type:
    type                                          {$$ = new FparType($1);}
|   "ref" data_type                               {$$ = new FparType(nullptr, $2);}
|   array_type                                    {$$ = new FparType(nullptr, UNDEFINED, $1);}
;

array_type:
    data_type '[' ']'                             {$$ = new ArrayType($1);}
|   array_type '[' T_intconst ']'                 {$1->append(new Int($3)); $$ = $1;}
;

local_def:
    /* empty */                                   {$$ = new LocalDef();}
|   local_def func_def                            {$1->append($2); $$ = $1;}
|   local_def func_decl                           {$1->append($2); $$ = $1;}
|   local_def var_def                             {$1->append($2); $$ = $1;}
;

func_decl:
    "decl" header                                {$$ = new FuncDecl($2);}
;

var_def:
    "var" id_aux "is" type                       {$$ = new VarDef($2, $4);}
;

id_aux:
    T_id                                         {$$ = new IdAux(new Id($1));} 
|   id_aux T_id                                  {$1->append(new Id($2)); $$ = $1;} 
;

stmt:
    stmt_list simple_stmt                        {$1->append($2); $$ = new Stmt($1);}
;

stmt_list:
    /* empty */                                  {$$ = new StmtList();}
|   stmt_list simple_stmt                        {$1->append($2); $$ = $1;}
;

simple_stmt:
    "skip"                                       {$$ = new Skip();}
|   l_value ":=" expr                            {$$ = new Assign($1, $3);}
|   proc_call                                    {$$ = $1;}
|   "exit"                                       {$$ = new Exit();}
|   "return" ':' expr                            {$$ = new Return($3);}
|   "if" cond ':' block                          {$$ = new If($2, $4);}
|   "if" cond ':' block if_aux "else" ':' block  {$$ = new If($2, $4, $8, $5);}
|   "loop" ':' block                             {$$ = new Loop($3);}
|   "loop" T_id ':' block                        {$$ = new Loop($4, new Id($2));}
|   "break"                                      {$$ = new Break();}
|   "break" ':' T_id                             {$$ = new Break(new Id($3));}
|   "continue"                                   {$$ = new Continue();}
|   "continue" ':' T_id                          {$$ = new Continue(new Id($3));}
;

if_aux:
    /* empty */                                  {$$ = new IfAux();}
|   if_aux "elif" cond ':' block                 {$1->append(new Elif($3, $5)); $$ = $1;} 
;

block:
    "begin" stmt "end"          {$$ = new Block($2);}
|   stmt "end"                  {$$ = new Block($1);}
;

proc_call:
    T_id                        {$$ = new ProcCall(new Id($1));}
|   T_id ':' expr expr_aux      {$4->insert_front($3); $$ = new ProcCall(new Id($1), $4);}
;

expr_aux:
    /* empty */                 {$$ = new ExprAux();}
|   expr_aux ',' expr           {$1->append($3); $$ = $1;}
;

func_call:
    T_id '(' ')'                {$$ = new FuncCall(new Id($1));}
|   T_id '(' expr expr_aux ')'  {$4->insert_front($3); $$ = new FuncCall(new Id($1), $4);}
;

l_value:
    T_id                        {$$ = new L_val(new Id($1));}
|   T_string                    {$$ = new L_val(nullptr, std::string($1));}
|   l_value '[' expr ']'        {$1->append($3); $$ = $1;}
;

expr:
    T_intconst                  {$$ = new Int($1);}
|   T_charconst                 {$$ = new Char($1);}
|   l_value                     {$$ = $1;}
|   '(' expr ')'                {$$ = $2;}
|   func_call                   {$$ = $1;}
|   "-" expr %prec UMINUS       {$$ = new UnOp($1, $2);}
|   "+" expr %prec UPLUS        {$$ = new UnOp($1, $2);}
|   expr "+" expr               {$$ = new BinOp($1, $2, $3);}
|   expr "-" expr               {$$ = new BinOp($1, $2, $3);}
|   expr "*" expr               {$$ = new BinOp($1, $2, $3);}
|   expr "/" expr               {$$ = new BinOp($1, $2, $3);}
|   expr "%" expr               {$$ = new BinOp($1, $2, $3);}
|   "true"                      {$$ = new Bool(true);}
|   "false"                     {$$ = new Bool(false);}
|   '!' expr                    {$$ = new UnOp($1, $2);}
|   expr '&' expr               {$$ = new BinOp($1, $2, $3);}
|   expr '|' expr               {$$ = new BinOp($1, $2, $3);}
;

cond:
    expr                        {$$ = new ExprCond($1);}
|   '(' cond_aux ')'            {$$ = $2;}
|   cond_aux                    {$$ = $1;}
;   

cond_aux:
   "not" cond                   {$$ = new UnOpCond($1, $2);}
|   cond "and" cond             {$$ = new BinOpCond($1, $2, $3);}
|   cond "or" cond              {$$ = new BinOpCond($1, $2, $3);}
|   expr "=" expr               {$$ = new BinOpCond($1, $2, $3);}
|   expr "<>" expr              {$$ = new BinOpCond($1, $2, $3);}
|   expr "<" expr               {$$ = new BinOpCond($1, $2, $3);}
|   expr ">" expr               {$$ = new BinOpCond($1, $2, $3);} 
|   expr "<=" expr              {$$ = new BinOpCond($1, $2, $3);} 
|   expr ">=" expr              {$$ = new BinOpCond($1, $2, $3);}

%%

void yyerror (const std::string msg) {
    std::cerr << "Error: " << msg << " in line " << linenum << "\n";
    exit(42);
}


int main() { 
    int res = yyparse();
    if (res == 0) 
        printf("Parsing completed successfully.\n");
    return res;
}
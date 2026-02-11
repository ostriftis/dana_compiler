%define parse.error verbose

%{
#include <string>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <string>
#include <fstream>

#include "ast.hpp"
#include "lexer.hpp"

extern int linenum;
extern FILE *yyin;
SymbolTable st;
LoopStack lp;

// Global AST pointer and command-line flags
FuncDef *program_ast = nullptr;
bool flag_i = false;
bool flag_f = false;
bool flag_O = false;
std::string output_file = "a.out";
std::string input_file = "";
%}

%union {
    char* var;
    long long num;
    char character;
    char* string;
    char* op;
    Stmt *stmt;
    SimpleStmt *simple_stmt;
    Expr *expr;
    L_val *l_val;
    ExprAux *expr_aux;
    FuncCall *func_call;
    ProcCall *proc_call;
    Cond *cond;
    Block *block;
    Header *header;
    DataType data_type;
    Type_ *type;
    IdAux *id_aux;
    VarDef *var_def;
    ArrayType_ *array_type;
    FparType *fpar_type;
    FparDef *fpar_def;
    HeaderAux *header_aux;
    FuncDecl *func_decl;
    LocalDef *local_def;
    FuncDef *func_def;
    IfAux *if_aux;
    StmtList *stmt_list;
    Elif *elif;
    std::pair<LocalDef*, StmtList*> *local_def_and_stmts_pair;
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

%token T_INDENT    "INDENT"
%token T_DEDENT    "DEDENT"

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

%expect 0

%type<simple_stmt> simple_stmt
%type<expr> expr
%type<l_val> l_value
%type<expr_aux> expr_aux
%type<func_call> func_call
%type<cond> cond cond_aux
%type<proc_call> proc_call
%type<if_aux> if_aux
%type<stmt_list> stmt_list indented_stmt_list
%type<block> block else_part
%type<header> header
%type<data_type> data_type
%type<type> type fpar_sized_array_type
%type<id_aux> id_aux
%type<var_def> var_def
%type<array_type> fpar_unsized_array_type
%type<fpar_type> fpar_type fpar_sized_array fpar_unsized_array
%type<func_decl> func_decl
%type<fpar_def> fpar_def
%type<header_aux> header_aux
%type<local_def> local_def
%type<func_def> func_def
%type<local_def_and_stmts_pair> indented_local_def_and_block local_def_and_stmts  
%type<simple_stmt> simple_stmt_no_colon

%%

program:
    func_def                                      {program_ast = $1;}
;

func_def:
    "def" header indented_local_def_and_block     {$$ = new FuncDef($2, $3->first, new Block(new Stmt($3->second)));}
;

indented_local_def_and_block:
    T_INDENT local_def_and_stmts T_DEDENT         {$$ = $2;}
;

local_def_and_stmts:
    local_def stmt_list                           {$$ = new std::pair<LocalDef*, StmtList*>($1, $2);}
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
    data_type                                     {$$ = new Type_($1);}
|   type '[' T_intconst ']'                       {$1->append(new Int($3)); $$ = $1;}
;

fpar_type:
    data_type                                     {$$ = new FparType(new Type_($1));}
|   fpar_sized_array                              {$$ = $1;}
|   fpar_unsized_array                            {$$ = $1;}
|   "ref" data_type                               {$$ = new FparType(nullptr, $2);}
;

fpar_sized_array:
    fpar_sized_array_type                         {$$ = new FparType($1);}
;

fpar_sized_array_type:
    data_type '[' T_intconst ']'                  {Type_ *t = new Type_($1); t->append(new Int($3)); $$ = t;}
|   fpar_sized_array_type '[' T_intconst ']'      {$1->append(new Int($3)); $$ = $1;}
;

fpar_unsized_array:
    fpar_unsized_array_type                       {$$ = new FparType(nullptr, UNDEFINED, $1);}
;

fpar_unsized_array_type:
    data_type '[' ']'                             {$$ = new ArrayType_($1);}
|   fpar_unsized_array_type '[' T_intconst ']'    {$1->append(new Int($3)); $$ = $1;}
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

stmt_list:
    /* empty */                                  {$$ = new StmtList();}
|   stmt_list simple_stmt                        {$1->append($2); $$ = $1;}
;

simple_stmt_no_colon:
    "skip"                                       {$$ = new Skip();}
|   l_value ":=" expr                            {$$ = new Assign($1, $3);}
|   proc_call                                    {$$ = $1;}
|   "exit"                                       {$$ = new Exit();}
|   "return" ':' expr                            {$$ = new Return($3);}
|   "loop" ':' block                             {$$ = new Loop($3);}
|   "loop" T_id ':' block                        {$$ = new Loop($4, new Id($2));}
|   "break"                                      {$$ = new Break();}
|   "break" ':' T_id                             {$$ = new Break(new Id($3));}
|   "continue"                                   {$$ = new Continue();}
|   "continue" ':' T_id                          {$$ = new Continue(new Id($3));}
;

simple_stmt:
    "skip"                                       {$$ = new Skip();}
|   l_value ":=" expr                            {$$ = new Assign($1, $3);}
|   proc_call                                    {$$ = $1;}
|   "exit"                                       {$$ = new Exit();}
|   "return" ':' expr                            {$$ = new Return($3);}
|   "if" cond ':' block if_aux else_part         {
        if ($6 != nullptr) {
            $$ = new If($2, $4, $6, $5);
        } else {
            $$ = new If($2, $4, nullptr, $5);
        }
    }
|   "loop" ':' block                             {$$ = new Loop($3);}
|   "loop" T_id ':' block                        {$$ = new Loop($4, new Id($2));}
|   "break"                                      {$$ = new Break();}
|   "break" ':' T_id                             {$$ = new Break(new Id($3));}
|   "continue"                                   {$$ = new Continue();}
|   "continue" ':' T_id                          {$$ = new Continue(new Id($3));}
;

else_part:
    /* empty */                                  {$$ = nullptr;}
|   "else" ':' block                             {$$ = $3;}
;

if_aux:
    /* empty */                                  {$$ = new IfAux();}
|   if_aux "elif" cond ':' block                 {$1->append(new Elif($3, $5)); $$ = $1;} 
;

block:
    indented_stmt_list                           {$$ = new Block(new Stmt($1));}
|   simple_stmt_no_colon                         {StmtList *sl = new StmtList(); sl->append($1); $$ = new Block(new Stmt(sl));}
;

indented_stmt_list:
    T_INDENT stmt_list T_DEDENT                  {$$ = $2;}
|   "begin" stmt_list "end"                      {$$ = $2;}
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
|   l_value                     {$1->fromExpr(true); $$ = $1;}
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
    exit(1);
}

void print_usage(const char* program_name) {
    std::cerr << "Usage: " << program_name << " [options] [input_file]\n";
    std::cerr << "Options:\n";
    std::cerr << "  -i              Output intermediate code (LLVM IR) to stdout\n";
    std::cerr << "  -f              Output final code (Assembly) to stdout\n";
    std::cerr << "  -O              Enable optimizations\n";
    std::cerr << "  -o <file>       Output executable to <file> (default: a.out)\n";
    std::cerr << "\n";
    std::cerr << "If no -i or -f flags are given:\n";
    std::cerr << "  - Intermediate code (.imm) and assembly (.asm) files are created\n";
    std::cerr << "  - Files are placed in the same directory as the input file\n";
    std::cerr << "\n";
    std::cerr << "Examples:\n";
    std::cerr << "  " << program_name << " program.dana           # Creates program.imm, program.asm, a.out\n";
    std::cerr << "  " << program_name << " -O program.dana       # Same with optimizations\n";
    std::cerr << "  " << program_name << " -o myapp program.dana # Creates myapp executable\n";
    std::cerr << "  " << program_name << " -i < program.dana     # Output LLVM IR to stdout\n";
    std::cerr << "  " << program_name << " -f < program.dana     # Output assembly to stdout\n";
}

int main(int argc, char** argv) {
    // Parse command-line arguments
    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        
        if (arg == "-i") {
            flag_i = true;
        } else if (arg == "-f") {
            flag_f = true;
        } else if (arg == "-O") {
            flag_O = true;
        } else if (arg == "-o") {
            if (i + 1 < argc) {
                output_file = argv[++i];
            } else {
                std::cerr << "Error: -o requires an argument\n";
                print_usage(argv[0]);
                return 1;
            }
        } else if (arg == "-h" || arg == "--help") {
            print_usage(argv[0]);
            return 0;
        } else if (arg[0] == '-') {
            std::cerr << "Error: Unknown option '" << arg << "'\n";
            print_usage(argv[0]);
            return 1;
        } else {
            // This is the input file
            input_file = arg;
        }
    }
    
    // Validate options
    if (flag_i && flag_f) {
        std::cerr << "Error: Cannot specify both -i and -f\n";
        return 1;
    }
    
    // If reading from stdin (-i or -f), input_file should be empty
    // If reading from file, input_file should be set
    if (!flag_i && !flag_f && input_file.empty()) {
        std::cerr << "Error: No input file specified\n";
        print_usage(argv[0]);
        return 1;
    }
    
    // Open input file if specified
    FILE* input = stdin;
    if (!input_file.empty()) {
        input = fopen(input_file.c_str(), "r");
        if (!input) {
            std::cerr << "Error: Cannot open input file '" << input_file << "'\n";
            return 1;
        }
        yyin = input;
    }
    
    // Redirect logging
    std::ofstream logfile("mylog.txt");
    auto* oldbuf = std::clog.rdbuf(logfile.rdbuf());
    
    // Parse the input
    int parse_result = yyparse();
    
    if (parse_result != 0) {
        std::clog.rdbuf(oldbuf);
        if (input != stdin) fclose(input);
        return 1;
    }
    
    // Now compile the program
    if (program_ast) {
        try {
            program_ast->compile(input_file, flag_i, flag_f, flag_O, output_file);
        } catch (const std::exception& e) {
            std::cerr << "Compilation error: " << e.what() << "\n";
            std::clog.rdbuf(oldbuf);
            if (input != stdin) fclose(input);
            return 1;
        }
    } else {
        std::cerr << "Error: No program to compile\n";
        std::clog.rdbuf(oldbuf);
        if (input != stdin) fclose(input);
        return 1;
    }
    
    std::clog.rdbuf(oldbuf);
    if (input != stdin) fclose(input);
    
    return 0;
}
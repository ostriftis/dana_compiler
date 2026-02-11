%{
#include <cstring>
#include "ast.hpp"
#include "lexer.hpp"
#include "parser.hpp"



#define T_eof       0

int linenum = 1;

%}

L [a-zA-Z]
D [0-9]
W [ \t\r]
HEX [0-9a-fA-F]
ESCAPE \\(n|t|r|0|\\|\'|\"|x{HEX}{HEX})

Identifier  {L}({L}|{D}|_)*
Number      {D}+
Character   [']([^\'\"\\]|{ESCAPE})[']
String      ["]([^\n\"])*["]    

%option noyywrap

%%

"and"       { yylval.op = strdup(yytext); return T_and;}
"as"        { return T_as;                    }
"begin"     { return T_begin;                 }
"break"     { return T_break;                 }
"byte"      { return T_byte;                  }
"continue"  { return T_continue;              }
"decl"      { return T_decl;                  }
"def"       { return T_def;                   }
"elif"      { return T_elif;                  }
"else"      { return T_else;                  }
"end"       { return T_end;                   }
"exit"      { return T_exit;                  }
"false"     { return T_false;                 }
"if"        { return T_if;                    }
"is"        { return T_is;                    }
"int"       { return T_int;                   }
"loop"      { return T_loop;                  }
"not"       { yylval.op = strdup(yytext); return T_not;}
"or"        { yylval.op = strdup(yytext); return T_or; }
"ref"       { return T_ref;                   }         
"return"    { return T_return;                }         
"skip"      { return T_skip;                  }         
"true"      { return T_true;                  }         
"var"       { return T_var;                   }

":="        { yylval.op = strdup(yytext); return T_assign;}
"="         { yylval.op = strdup(yytext); return T_eq;    }
"<>"        { yylval.op = strdup(yytext); return T_neq;   }
"<"         { yylval.op = strdup(yytext); return T_lt;    }
">"         { yylval.op = strdup(yytext); return T_gt;    }
"<="        { yylval.op = strdup(yytext); return T_le;    }
">="        { yylval.op = strdup(yytext); return T_ge;    }
"+"         { yylval.op = strdup(yytext); return T_plus;  }
"-"         { yylval.op = strdup(yytext); return T_minus; }
"*"         { yylval.op = strdup(yytext); return T_times; }
"/"         { yylval.op = strdup(yytext); return T_div;   }
"%"         { yylval.op = strdup(yytext); return T_mod;   }

[\(\)\[\]\,\:,\!\&\|] { yylval.op = yytext; return yytext[0]; }  /* return single character tokens */


{Number}             { yylval.num = atoi(yytext); return T_intconst;    }
{Identifier}         { yylval.var = strdup(yytext); return T_id;        }
{Character}          { yylval.character = yytext[0]; return T_charconst;}
{String}             { yylval.string = strdup(yytext); return T_string; }
{W}+                 { /* ignore whitespace */                          }

#.*                            { /* ignore comments */   }
"(*"([^*]+|\*+[^*)])*\*+")"    {/*for every new line inside the comment we increase linenum*/ 
                                for (int i = 0; yytext[i]; i++)
                                    if (yytext[i] == '\n') linenum++;
                                }

[\n\r]          { linenum++; }

.           { fprintf(stderr, "Unrecognized character %c at line %d\n", yytext[0], linenum); }

%%
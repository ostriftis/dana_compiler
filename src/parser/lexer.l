%{
#include "lexer.h"
#include "parser.h"

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

"and"       { return T_and;       }
"as"        { return T_as;        }
"begin"     { return T_begin;     }
"break"     { return T_break;     }
"byte"      { return T_byte;      }
"continue"  { return T_continue;  }
"decl"      { return T_decl;      }
"def"       { return T_def;       }
"elif"      { return T_elif;      }
"else"      { return T_else;      }
"end"       { return T_end;       }
"exit"      { return T_exit;      }
"false"     { return T_false;     }
"if"        { return T_if;        }
"is"        { return T_is;        }
"int"       { return T_int;       }
"loop"      { return T_loop;      }
"not"       { return T_not;       }
"or"        { return T_or;        }
"ref"       { return T_ref;       }
"return"    { return T_return;    }
"skip"      { return T_skip;      }
"true"      { return T_true;      }
"var"       { return T_var;       }

":="        { return T_assign;    }
"="         { return T_eq;        }
"<>"        { return T_neq;       }
"<"         { return T_lt;        }
">"         { return T_gt;        }
"<="        { return T_le;        }
">="        { return T_ge;        }
"+"         { return T_plus;      }
"-"         { return T_minus;     }
"*"         { return T_times;     }
"/"         { return T_div;       }
"%"         { return T_mod;       }

[\(\)\[\]\,\:,\!\&\|] { return yytext[0]; }  /* return single character tokens */


{Number}             { return T_intconst;      }
{Identifier}         { return T_id;            }
{Character}          { return T_charconst;     }
{String}             { return T_string;        }
{W}+                 { /* ignore whitespace */ }

#.*                   { /* ignore comments */   }
"(*"([^*]+|\*+[^*)])*\*+")"    {/*for every new line inside the comment we increase linenum*/ 
                                for (int i = 0; yytext[i]; i++)
                                    if (yytext[i] == '\n') linenum++;
                                }

[\n\r]          { linenum++; }

.           { fprintf(stderr, "Unrecognized character %c at line %d\n", yytext[0], linenum); }

%%
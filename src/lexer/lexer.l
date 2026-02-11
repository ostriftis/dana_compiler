%{

#include <stdio.h>

/* to be added to parser.y later. currently here for testing */
#define T_eof       0
#define T_and       5000
#define T_as        5001
#define T_begin     5002
#define T_break     5003
#define T_byte      5004
#define T_continue  5005
#define T_decl      5006
#define T_def       5007
#define T_elif      5008
#define T_else      5009
#define T_end       5010
#define T_exit      5011
#define T_false     5012
#define T_if        5013
#define T_is        5014
#define T_int       5015
#define T_loop      5016
#define T_not       5017
#define T_or        5018
#define T_ref       5019
#define T_return    5020
#define T_skip      5021
#define T_true      5022
#define T_var       5023

#define T_assign    5050
#define T_eq        5051
#define T_neq       5052
#define T_lt        5053
#define T_gt        5054
#define T_le        5055
#define T_ge        5056
#define T_plus      5057
#define T_minus     5058
#define T_times     5059
#define T_div       5060
#define T_mod       5061

#define T_const     5100
#define T_id        5101
#define T_string    5102

int linenum = 1;

%}

L [a-zA-Z]
D [0-9]
W [ \t\r]
HEX [0-9a-fA-F]
ESCAPE \\(n|t|r|0|\\|\'|\"|x{HEX}{HEX})

Identifier  {L}({L}|{D}|_)*
Number      {D}+
Character   ['][^\'\"\\][']
String      ["]([^\n\"])*["]    
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

[\(\)\[\]\,\:] { return yytext[0]; }  /* return single character tokens */

{Number}             { return T_const;         }
{Identifier}         { return T_id;            }
{Character}          { return T_char;          }
{String}             { return T_string;        }
{W}+                 { /* ignore whitespace */ }

#.[\n\r]*                   { /* ignore comments */   }
"(*"([^*]+|\*+[^*)])*\*+")" { /* ignore multiline comments*/ }

\n          { linenum++; }

.           { fprintf(stderr, "Unrecognized character %c at line %d\n", yytext[0], linenum); }

%%

int main(void) {
    int token;
    do {
        token = yylex();
        printf("Token: %d  Word: %s\n", token, yytext);
    } while (token != T_eof);
    return 0;
}
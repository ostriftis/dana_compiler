%{
#include <cstring>
#include <cstdlib>
#include <stack>
#include <iostream>
#include "ast.hpp"
#include "lexer.hpp"
#include "parser.hpp"

#define T_eof       0

int linenum = 1;

int indent_caller;  /* to remember which state called the indent state */

/* globals to track current indentation */
int g_current_line_indent = 0;   /* indentation of the current line */
std::stack<size_t> g_indent_levels;
int g_is_fake_outdent_symbol = 0;
        
static const unsigned int TAB_WIDTH = 4;

#define YY_USER_INIT { \
        g_indent_levels.push(0); \
        BEGIN(initial); \
    }

int yycolumn = 1;
void set_yycolumn(int val) {
    yycolumn = val;
}

#define YY_USER_ACTION { \
    set_yycolumn(yycolumn); \
    yycolumn += yyleng; \
}

char* process_escapes(char* str) {
    char* result = (char*)malloc(strlen(str) + 1);
    char* dst = result;
    char* src = str;

    while (*src) {
        if (*src == '\\' && *(src + 1)) {
            switch (*(src + 1)) {
                case 'n': *dst++ = '\n'; src += 2; break;
                case 't': *dst++ = '\t'; src += 2; break;
                case 'r': *dst++ = '\r'; src += 2; break;
                case '0': *dst++ = '\0'; src += 2; break;
                case '\\': *dst++ = '\\'; src += 2; break;
                case '\'': *dst++ = '\''; src += 2; break;
                case '"': *dst++ = '"'; src += 2; break;
                default: *dst++ = *src++; break;
            }
        } else {
            *dst++ = *src++;
        }
    }
    *dst = '\0';
    return result;
}

%}

%x initial
%x indent
%s normal

L [a-zA-Z]
D [0-9]
W [ \t\r]
HEX [0-9a-fA-F]
ESCAPE \\(n|t|r|0|\\|\'|\"|x{HEX}{HEX})

Identifier  {L}({L}|{D}|_)*
Number      {D}+
Character   \'([^\\']|\\.)\'
String      \"([^\\\"]|\\.)*\"

%option noyywrap
%option yylineno

%%

<initial>.  { indent_caller = normal; yyless(0); BEGIN(indent); }
<initial>\n { indent_caller = normal; yyless(0); BEGIN(indent); }

<indent>" "     { g_current_line_indent++; }
<indent>\t      { g_current_line_indent = (g_current_line_indent + TAB_WIDTH) & ~(TAB_WIDTH-1); }
<indent>\n      { 
                    if (!g_is_fake_outdent_symbol) {
                        linenum++;
                    }
                    g_current_line_indent = 0; /* ignoring blank line */
                }
<indent><<EOF>> {
                    // When encountering the end of file, we want to emit an
                    // outdent for all indents currently left.
                    if(g_indent_levels.top() != 0) {
                        g_indent_levels.pop();

                        // See the same code below (<indent>.) for a rationale.
                        if((size_t)g_current_line_indent != g_indent_levels.top()) {
                            g_is_fake_outdent_symbol = 1;  // Mark as fake to avoid line counting
                            unput('\n');
                            for(size_t i = 0 ; i < g_indent_levels.top() ; ++i) {
                                unput(' ');
                            }
                        } else {
                            BEGIN(indent_caller);
                        }

                        return T_DEDENT;
                    } else {
                        yyterminate();
                    }
                }

<indent>.       {
                    if(!g_is_fake_outdent_symbol) {
                        unput(*yytext);
                    }
                    set_yycolumn(yycolumn-1);
                    g_is_fake_outdent_symbol = 0;

                    // Indentation level has increased. It can only ever
                    // increase by one level at a time. Remember how many
                    // spaces this level has and emit an indentation token.
                    if((size_t)g_current_line_indent > g_indent_levels.top()) {
                        g_indent_levels.push(g_current_line_indent);
                        BEGIN(indent_caller);
                        return T_INDENT;
                    } else if((size_t)g_current_line_indent < g_indent_levels.top()) {
                        // Outdenting is the most difficult, as we might need to
                        // outdent multiple times at once, but flex doesn't allow
                        // emitting multiple tokens at once! So we fake this by
                        // 'unput'ting fake lines which will give us the next
                        // outdent.
                        g_indent_levels.pop();

                        if((size_t)g_current_line_indent != g_indent_levels.top()) {
                            // Mark that we're creating fake symbols to avoid counting newlines
                            g_is_fake_outdent_symbol = 1;
                            // Unput the rest of the current line, including the newline.
                            // We want to keep it untouched.
                            for(size_t i = 0 ; i < (size_t)g_current_line_indent ; ++i) {
                                unput(' ');
                            }
                            unput('\n');
                            // Now, insert a fake character indented just so
                            // that we get a correct outdent the next time.
                            unput('.');
                            // Though we need to remember that it's a fake one
                            // so we can ignore the symbol.
                            for(size_t i = 0 ; i < g_indent_levels.top() ; ++i) {
                                unput(' ');
                            }
                            unput('\n');
                        } else {
                            BEGIN(indent_caller);
                        }

                        return T_DEDENT;
                    } else {
                        // No change in indentation, not much to do here...
                        BEGIN(indent_caller);
                    }
                }

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

[\(\)\[\]\,\:,\!\&\|] { yylval.op = strdup(yytext); return yytext[0]; }

{Number}             { yylval.num = atoll(yytext); return T_intconst;    }
{Identifier}         { yylval.var = strdup(yytext); return T_id;        }
{Character} {
    // Handle escape sequences
    char c;
    if (yytext[1] == '\\') {
        switch (yytext[2]) {
            case 'n': c = '\n'; break;
            case 't': c = '\t'; break;
            case 'r': c = '\r'; break;
            case '0': c = '\0'; break;
            case '\\': c = '\\'; break;
            case '\'': c = '\''; break;
            case '\"': c = '\"'; break;
            default: c = yytext[2]; break;
        }
    } else {
        c = yytext[1];
    }
    yylval.character = c;
    return T_charconst;
}
{String}             {
                        char *str = yytext + 1;
                        str[strlen(str) - 1] = '\0';
                        char *processed = process_escapes(str);
                        yylval.string = processed;
                        return T_string;
                     }

{W}+                 { /* ignore whitespace */ }

#.*                            { /* ignore comments */ }
"(*"([^*]+|\*+[^*)])*\*+")"    {
                                for (int i = 0; yytext[i]; i++)
                                    if (yytext[i] == '\n') linenum++;
                                }

[\n\r]          { linenum++; g_current_line_indent = 0; BEGIN(indent); }


<<EOF>>             {
                        if(g_indent_levels.size() > 1) {  
                            g_indent_levels.pop();
                            if(g_indent_levels.size() > 1) {
                                g_is_fake_outdent_symbol = 1;  // Mark as fake to avoid line counting
                                unput(' '); 
                                for(size_t i = 0; i < g_indent_levels.top(); ++i) {
                                    unput(' ');
                                }
                                unput('\n');
                                g_current_line_indent = g_indent_levels.top();
                                BEGIN(indent);
                            }
                            return T_DEDENT;
                        } else {
                            yyterminate();
                        }
                    }

.           { fprintf(stderr, "Unrecognized character %c at line %d\n", yytext[0], linenum); }

%%
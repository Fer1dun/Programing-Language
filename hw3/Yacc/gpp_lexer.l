%{
    #include "gpp.h"
    #include "parser.tab.h"

    int line_no = 1;
%}

%%
"def"    { return KW_DEF; }
"exit"      { return KW_EXIT; }
"+"         { return OP_PLUS; }
"-"         { return OP_MINUS; }
"/"         { return OP_DIV; }
"*"         { return OP_MULT; }
"("         { return OP_OP; }
")"         { return OP_CP; }
";;".*      { ; /* Ignore */ }  

[a-zA-Z_][a-zA-Z0-9_]*              { strcpy(yylval.str, yytext); return ID; }
[-]?([1-9][0-9]*|0)b([1-9][0-9]*|0)     { yylval.valuef = valuef_convert(yytext); return VALUEF; }

^\n                         { if (yyin == stdin) return 0; }
[ \t\r  ]                   { ; }
\n                          { ++line_no; }
<<EOF>>                     { return 0; }

[0]+[a-zA-Z][_a-zA-Z0-9]*   { printf("%s: LEXICAL ERROR (LEADING DIGIT)\n", yytext); return 0; } 
[0-9][0-9]*                 { printf("%s: LEXICAL ERROR (LEADING ZERO)\n", yytext); return 0; }
[a-zA-z0-9+\-/*_,]*[^a-zA-z0-9+\-/*()_,\t\n ]+[a-zA-z0-9+\-/*_,]*   { printf("%s: LEXICAL ERROR (APPEARANCE OF ILLEGAL CHRARACTERS)\n", yytext); } 
.                           { printf("%s(%d): LEXICAL ERROR (SPELLING ERROR)\n", yytext, *yytext); return 0; } 
%%

int yywrap() {
    return 1;
}
%{
#include <stdio.h>

enum Token {
    KW_AND=1, KW_OR, KW_NOT, KW_EQUAL, KW_LESS, KW_NIL, KW_LIST,
    KW_APPEND, KW_CONCAT, KW_SET, KW_DEF, KW_FOR, KW_IF,
    KW_EXIT, KW_LOAD, KW_DISPLAY, KW_TRUE, KW_FALSE,
    OP_PLUS, OP_MINUS, OP_DIV, OP_MULT, OP_OP, OP_CP, OP_COMMA,
    COMMENT,VALUEF,IDENTIFIER,ERROR,quit
};


%}

%%

[" \t\n"]   ; /* Ignore spaces, tab and line break characters */

and     { return KW_AND; }          
or      { return KW_OR; }
not     { return KW_NOT; }
equal   { return KW_EQUAL; }
less    { return KW_LESS; }
nil     { return KW_NIL; }
list    { return KW_LIST; }
append  { return KW_APPEND; }
concat  { return KW_CONCAT; }
set     { return KW_SET; }
def     { return KW_DEF; }
for     { return KW_FOR; }
if      { return KW_IF; }
exit    { return KW_EXIT; }
load    { return KW_LOAD; }
disp    { return KW_DISPLAY; }
true    { return KW_TRUE; }
false   { return KW_FALSE; }
quit    {return quit;}

"+"       { return OP_PLUS; }
"-"       { return OP_MINUS; }
"*"       { return OP_MULT; }
"/"       { return OP_DIV; }
","       { return OP_COMMA; }

"("     { return OP_OP; }
")"     { return OP_CP; }

;;.*    { return COMMENT; }

[1-9][0-9]*b[1-9][0-9]*   { return VALUEF; } 
[a-zA-Z][a-zA-Z_0-9]*    { return IDENTIFIER; }

. {return ERROR;}


%%
int yywrap(void){   //Returns what happens when it reaches the end of the file
    return 1;
}

/* Code Section */
int main(int argc, char *argv[]) {
    yywrap();
    int token;
    while ((token = yylex())) { //reads input data and chunks it into strings
        switch (token) {
            case KW_AND: printf("KW_AND\n"); break;
            case KW_OR: printf("KW_OR\n"); break;
            case KW_NOT: printf("KW_NOT\n"); break;
            case KW_EQUAL: printf("KW_EQUAL\n"); break;
            case KW_LESS: printf("KW_LESS\n"); break;
            case KW_NIL: printf("KW_NIL\n"); break;
            case KW_LIST: printf("KW_LIST\n"); break;
            case KW_APPEND: printf("KW_APPEND\n"); break;
            case KW_CONCAT: printf("KW_CONCAT\n"); break;
            case KW_SET: printf("KW_SET\n"); break;
            case KW_DEF: printf("KW_DEF\n"); break;
            case KW_FOR: printf("KW_FOR\n"); break;
            case KW_IF: printf("KW_IF\n"); break;
            case KW_EXIT: printf("KW_EXIT\n"); break;
            case KW_LOAD: printf("KW_LOAD\n"); break;
            case KW_DISPLAY: printf("KW_DISPLAY\n"); break;
            case KW_TRUE: printf("KW_TRUE\n"); break;
            case KW_FALSE: printf("KW_FALSE\n"); break;
            case OP_PLUS: printf("OP_PLUS\n"); break;
            case OP_MINUS: printf("OP_MINUS\n"); break;
            case OP_MULT: printf("OP_MULT\n"); break;
            case OP_DIV: printf("OP_DIV\n"); break;
            case OP_COMMA: printf("OP_COMMA\n"); break;
            case OP_OP: printf("OP_OP\n"); break;
            case OP_CP: printf("OP_CP\n"); break;
            case COMMENT: printf("COMMENT\n"); break;
            case VALUEF: printf("VALUEF\n"); break;
            case IDENTIFIER: printf("IDENTIFIER\n"); break;
            case ERROR: printf("ERROR\n"); break;
            case quit: return 0;
            default: printf("UNKNOWN\n"); break;
        }
    }

    return 0;
}


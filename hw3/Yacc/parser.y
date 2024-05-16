%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gpp.h"


#define OUT_FILE "output.txt"

extern FILE * yyin;     /* defined by lex, reads from this file */
extern FILE * yyout;    /* defined by lex, writes to this file */
extern int yyparse();   /* defined by yacc, parser function */
extern char * yytext;   /* defined by lex current token */
extern int line_no;     /* current line */

int yylex();

int yyerror(char* str) { 
    fprintf(yyout, "SYNTAX_ERROR %s Expression at line %d is not recognized\n", yytext, line_no);
    return 0; 
}

int exit_prog = 0;
typedef struct FunctionNode {
    char name[16];
    struct FunctionNode* next;
} FunctionNode;

FunctionNode* functionList = NULL;

void insertFunction(char* name) {
    FunctionNode* newNode = (FunctionNode*)malloc(sizeof(FunctionNode));
    strncpy(newNode->name, name, sizeof(newNode->name));
    newNode->next = functionList;
    functionList = newNode;
}

int isFunctionDefined(char* name) {
    FunctionNode* current = functionList;
    while (current != NULL) {
        if (strcmp(current->name, name) == 0) {
            return 1; // Function is defined
        }
        current = current->next;
    }
    return 0; // Function is not defined
}

%}

%union {
    Valuef valuef;
    int valueb;
    char str[16];
}

%start START

%token KW_DEF  KW_EXIT 

%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP 

%token COMMENT

%token <valuef> VALUEF

%token <str> ID

%type <valuef> EXP
%type <valuef> FUNCTION

%%

START   : START EXP             { fprintf(yyout, "Syntax OK.\nResult: %db%d\n", $2.num, $2.denom); }
        | START OP_OP KW_EXIT OP_CP                     { return 1; exit_prog = 1; }
        | START FUNCTION        { fprintf(yyout, "Syntax OK.\nResult: %db%d\n", $2.num, $2.denom); }
        | EXP                   { fprintf(yyout, "Syntax OK.\nResult: %db%d\n", $1.num, $1.denom); }
        | FUNCTION              { fprintf(yyout, "Syntax OK.\nResult: %db%d\n", $1.num, $1.denom); }
    ;

EXP     : OP_OP OP_PLUS EXP EXP OP_CP                   { $$ = valuef_add($3, $4); }
        | OP_OP OP_MINUS EXP EXP OP_CP                  { $$ = valuef_sub($3, $4); }
        | OP_OP OP_MULT EXP EXP OP_CP                   { $$ = valuef_mult($3, $4); }
        | OP_OP OP_DIV EXP EXP OP_CP                    { $$ = valuef_div($3, $4); }    
        | OP_OP ID EXP EXP  OP_CP                       { printf("reqw"); $$ = valuef_create(0, 1); }
        | OP_OP OP_PLUS ID ID  OP_CP                    { printf("rerewrewqw"); $$ = valuef_create(0, 1); }
        | ID            { $$ = valuef_create(0,1); fprintf(yyout, "Result: %s\n", $1); }                                      
        | VALUEF                                        {  $$ = $1; }
        ;


FUNCTION    : OP_OP KW_DEF ID EXP OP_CP       { insertFunction($3); fprintf(yyout, "#Function\n"); }
            | OP_OP KW_DEF ID ID EXP OP_CP { insertFunction($3); fprintf(yyout, "#Function\n"); }
            | OP_OP KW_DEF ID ID ID EXP OP_CP { insertFunction($3); fprintf(yyout, "#Function\n"); }
            ; 

%%

int main(int argc, char* argv[]) {
    FILE* in_stream = NULL;
    FILE* out_stream = NULL;

    if (argc == 1) {
        /* shell mode */
        in_stream = stdin;
    } else {
        in_stream = fopen(argv[1], "r");
        if (!in_stream) {
            printf("File \"%s\" cannot find or open\n", argv[1]);
            return 1;
        }
    }

    out_stream = fopen(OUT_FILE, "w");
    yyin = in_stream;
    yyout = out_stream;
    yyparse();
    fclose(out_stream);
}
/****************************************************/
/* File: cminus.y                                   */
/* The TINY Yacc/Bison specification file           */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/
%{
#define YYPARSER /* distinguishes Yacc output from other code files */

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

#define YYSTYPE TreeNode *
static char * savedName; /* for use in assignments */
static int savedLineNo;  /* ditto */
static TreeNode * savedTree; /* stores syntax tree for later return */
static int yylex(void); // added 11/2/11 to ensure no conflict with lex
char *nameStack[10];
int nameStackIndex = 0;
char* nameStackPop();
int lineStack[10];
int lineStackIndex = 0;
int lineStackPop();
%}

%token IF ELSE WHILE RETURN INT VOID
%token ID NUM
%token EQ NE LT LE GT GE LPAREN RPAREN LBRACE RBRACE LCURLY RCURLY SEMI
%token ERROR 
%left PLUS MINUS TIMES OVER COMMA
%right ASSIGN

%% /* Grammar for C- */

program             : declaration_list
                         { savedTree = $1; } 
                    ;
declaration_list    : declaration_list declaration
                         { YYSTYPE t = $1;
                           if (t != NULL)
                             { while (t->sibling != NULL)
                                  t = t->sibling;
                               t->sibling = $2;
                               $$ = $1; }
                           else $$ = $2;
                         }
                    | declaration { $$ =$1; }
                    ;
declaration         : var_declaration { $$ = $1; }
                    | fun_declaration { $$ = $1; }
                    ;
var_declaration     : type_specifier ID SEMI
                         { $$ = newExpNode(VarK);
                           $$->type = (ExpType)$1;
                           $$->attr.name = nameStackPop();
                           $$->lineno = lineStackPop();
                         }
                    | type_specifier ID LBRACE size RBRACE SEMI
                         { $$ = newExpNode(VarArrayK);
                           $$->type = (ExpType)$1;
                           $$->attr.name = nameStackPop();
                           $$->lineno = lineStackPop();
                           $$->child[0] = $4;
                         }
                    ;
size                : NUM
                         { $$ = newExpNode(ConstK);
                           $$->type = Integer;
                           $$->attr.val = atoi(tokenString);
                           $$->lineno = lineno;
                         }
                    ;
type_specifier      : INT { $$ = Integer; }
                    | VOID { $$ = Void; }
                    ;
fun_declaration     : type_specifier ID LPAREN params RPAREN compound_stmt
                         { $$ = newStmtNode(FunctionK);
                           $$->type = (ExpType)$1;
                           $$->attr.name = nameStackPop();
                           $$->lineno = lineStackPop();
                           $$->child[0] = $4;
                           $$->child[1] = $6;
                         }
                    ;
params              : param_list { $$ = $1; }
                    | VOID
                         { $$ = newExpNode(SingleParamK);
                           $$->type = Void;
                           $$->lineno = lineno;
                         }
                    ;
param_list          : param_list COMMA param
                         { YYSTYPE t = $1;
                           if (t != NULL)
                           { while (t->sibling != NULL)
                                t = t->sibling;
                             t->sibling = $3;
                             $$ = $1; }
                             else $$ = $3;
                         }
                    | param { $$ = $1; }
                    ;
param               : type_specifier ID
                         { $$ = newExpNode(SingleParamK);
                           $$->type = (ExpType)$1;
                           $$->attr.name = nameStackPop();
                           $$->lineno = lineStackPop();
                         }
                    | type_specifier ID LBRACE RBRACE
                         { $$ = newExpNode(ArrayParamK);
                           $$->type = (ExpType)$1;
                           $$->attr.name = nameStackPop();
                           $$->lineno = lineStackPop();
                         }
                    ;
compound_stmt       : LCURLY local_declarations statement_list RCURLY
                         { $$ = newStmtNode(CompoundK);
                           $$->child[0] = $2;
                           $$->child[1] = $3;
                           //$$->lineno = $2->lineno;
                         }
                    ;
local_declarations  : local_declarations var_declaration
                         { YYSTYPE t = $1;
                           if (t != NULL)
                           { while (t->sibling != NULL)
                                t = t->sibling;
                             t->sibling = $2;
                             $$ = $1; }
                             else $$ = $2;
                         }
                    | empty { $$ = $1; }
                    ;
statement_list      : statement_list statement
                         { YYSTYPE t = $1;
                           if (t != NULL)
                           { while (t->sibling != NULL)
                                t = t->sibling;
                             t->sibling = $2;
                             $$ = $1; }
                             else $$ = $2;
                         }
                    | empty { $$ = $1; }
                    ;
statement           : expression_stmt { $$ = $1; }
                    | compound_stmt { $$ = $1; }
                    | selection_stmt { $$ = $1; }
                    | iteration_stmt { $$ = $1; }
                    | return_stmt { $$ = $1; }
                    ;
expression_stmt     : expression SEMI { $$ = $1;}
                    | SEMI { $$ = NULL; }
                    ;
selection_stmt      : IF LPAREN expression RPAREN statement
                         { $$ = newStmtNode(IfK);
                           $$->child[0] = $3;
                           $$->child[1] = $5;
                           $$->attr.withElse = FALSE;
                           $$->lineno = $3->lineno;
                         }
                    | IF LPAREN expression RPAREN statement ELSE statement
                         { $$ = newStmtNode(IfK);
                           $$->child[0] = $3;
                           $$->child[1] = $5;
                           $$->child[2] = $7;
                           $$->attr.withElse = TRUE;
                           $$->lineno = $3->lineno;
                         }
                    ;
iteration_stmt      : WHILE LPAREN expression RPAREN statement
                         { $$ = newStmtNode(WhileK);
                           $$->child[0] = $3;
                           $$->child[1] = $5;
                           $$->lineno = $3->lineno;
                         }
                    ;
return_stmt         : RETURN SEMI { $$ = newStmtNode(ReturnK);}
                    | RETURN expression SEMI
                         { $$ = newStmtNode(ReturnK);
                           $$->child[0] = $2;
                           $$->lineno = $2->lineno;
                         }
                    ;
expression          : var ASSIGN expression
                         { $$ = newExpNode(AssignK);
                           $$->type = Integer;
                           $$->child[0] = $1;
                           $$->child[1] = $3;
                           $$->lineno = $1->lineno;
                         }
                    | simple_expression { $$ = $1; }
                    ;
var                 : ID
                         { $$ = newExpNode(IdK);
                           $$->type = Integer;
                           $$->attr.name = nameStackPop();
                           $$->lineno = lineStackPop();
                         }
                    | ID LBRACE expression RBRACE
                         { $$ = newExpNode(IdArrayK);
                           $$->attr.name = nameStackPop();
                           $$->child[0] = $3;
                           $$->lineno = lineStackPop();
                         }
                    ;
simple_expression   : additive_expression relop additive_expression
                         { $$ = newExpNode(OpK); 
                           $$->type = Integer;
                           $$->child[0] = $1;
                           $$->child[1] = $3;
                           $$->attr.op = $2;
                           $$->lineno = $1->lineno;
                         }
                    | additive_expression
                         { $$ = $1; }
                    ;
relop               : LE { $$ = LE; }
                    | LT { $$ = LT; }
                    | GT { $$ = GT; }
                    | GE { $$ = GE; }
                    | EQ { $$ = EQ; }
                    | NE { $$ = NE; }
                    ;
additive_expression : additive_expression addop term
                         { $$ = newExpNode(OpK);
                           $$->type = Integer;
                           $$->child[0] = $1;
                           $$->child[1] = $3;
                           $$->attr.op = $2;
                           $$->lineno = $1->lineno;
                         }
                    | term { $$ = $1; }
                    ;
addop               : PLUS { $$ = PLUS; }
                    | MINUS { $$ = MINUS; }
                    ;
term                : term mulop factor
                         { $$ = newExpNode(OpK);
                           $$->type = Integer;
                           $$->child[0] = $1;
                           $$->child[1] = $3;
                           $$->attr.op = $2;
                           $$->lineno = $1->lineno;
                         }
                    | factor { $$ = $1; }
                    ;
mulop               : TIMES { $$ = TIMES; }
                    | OVER { $$ = OVER; }
                    ;
factor              : LPAREN expression RPAREN
                         { $$ = $2; }
                    | var { $$ = $1; }
                    | call { $$ = $1; }
                    | NUM
                         { $$ = newExpNode(ConstK);
                           $$->type = Integer;
                           $$->attr.val = atoi(tokenString);
                           $$->lineno = lineno;
                         }
                    ;
call                : ID LPAREN args RPAREN
                         { $$ = newExpNode(CallK);
                           $$->attr.name = nameStackPop();
                           $$->child[0] = $3;
                           $$->lineno = lineStackPop();
                         }
                    ;
args                : arg_list
                         { $$ = $1; }
                    | empty
                         { $$ = $1; }
                    ;
arg_list            : arg_list COMMA expression
                         { YYSTYPE t = $1; 
                           if (t != NULL)
                           { while (t->sibling != NULL)
                                t = t->sibling;
                             t->sibling = $3;
                             $$ = $1; }
                             else $$ = $3;
                         }
                    | expression
                         { $$ = $1; }
                    ;
empty               : { $$ = NULL;}
                    ;

%%

int yyerror(char * message)
{ fprintf(listing,"Syntax error at line %d: %s\n",lineno,message);
  fprintf(listing,"Current token: ");
  printToken(yychar,tokenString);
  Error = TRUE;
  return 0;
}

/* yylex calls getToken to make Yacc/Bison output
 * compatible with ealier versions of the TINY scanner
 */
static int yylex(void)
{ TokenType token = getToken();
  if (token == ID)
  { nameStackPush(copyString(tokenString));
    lineStackPush(lineno); }
  return token;
}

TreeNode * parse(void)
{ yyparse();
  return savedTree;
}

void nameStackPush(char *name)
{ nameStack[nameStackIndex++] = name; }

char* nameStackPop()
{ return nameStack[--nameStackIndex]; }

void lineStackPush(int line)
{ lineStack[lineStackIndex++] = line; }

int lineStackPop()
{ return lineStack[--lineStackIndex]; }
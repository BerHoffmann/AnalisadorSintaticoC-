/****************************************************/
/* File: cminus.y                                   */
/* The CMINUS Yacc/Bison specification file         */
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
static int yylex(void);
int yyerror(char *);

%}

%token IF ELSE VOID INT WHILE RETURN
%token ID NUM 
%token ASSIGN EQ LT LTE GT GTE DIF PLUS MINUS TIMES OVER LPAREN RPAREN SEMI COMMA LCOL RCOL LBRACK LBRACK
%token ERROR 

%% /* Grammar for TINY */

program     : decl-list
                { savedTree = $1;} 
            ;
decl-list   : decl-list decl 
                { YYSTYPE t = $1;
                  if (t != NULL) {
                    while (t->sibling != NULL)
                      t = t->sibling;
                    t->sibling = $2;
                    $$ = $1; 
                  }
                  else $$ = $2;
                }
            | decl { $$ = $1; }
            ;
decl        : var-decl { $$ = $1; }
            | fun-decl { $$ = $1; }
            ;
var-decl    : type-specifier ID SEMI 
                { $$ = newDeclNode(VarK);
                  $$->attr.type = $1;
                  $$->lineno = lineno;
                  $$->child[0] = newExpNode(IdK);
                  $$->child[0]->lineno = lineno;
                  $$->child[0]->attr.name = savedName;
                }
            | type-specifier ID LCOL NUM RCOL SEMI
                { $$ = newDeclNode(ArrVarK);
                  $$->attr.type = $1;
                  $$->lineno = lineno;
                  $$->child[0] = newExpNode(IdK);
                  $$->child[0]->attr.arr = newArrAttr(savedName, atoi($4));
                  $$->child[0]->lineno = lineno;
                }
            ;
type-specifier  : INT  { $$ = $1; }
                | VOID { $$ = $1; }
                ;
fun-decl    : type-specifier ID LPAREN params RPAREN composed-decl
                { $$ = newDeclNode(FunK);
                  $$->attr.type = $1;
                  $$->lineno = lineno;
                  $$->child[0] = newExpNode(IdK);
                  $$->child[0]->attr.name = savedName;
                  $$->child[0]->lineno = lineno;
                  if (params == NULL)
                    $$->child[0]->child[0] = $6;
                  else {
                    $$->child[0]->child[0] = $4;
                    $$->child[0]->child[1] = $6;
                  }
                }
            ;
params      : param-list  { $$ = $1; }
            | VOID  { $$ = NULL; }
            ;
param-list  : param-list COMMA param 
                { YYSTYPE t = $1;
                  if (t != NULL) {
                    while (t->sibling != NULL)
                      t = t->sibling;
                    t->sibling = $3;
                    $$ = $1; 
                  }
                  else $$ = $3;
                }
            | param { $$ = $1; }
            ;
param       : type-specifier ID 
                { $$ = newDeclNode(ParamK);
                  $$->attr.type = $1;
                  $$->lineno = lineno;
                  $$->child[0] = newExpNode(IdK);
                  $$->child[0]->attr.name = savedName;
                  $$->child[0]->lineno = lineno;
                }
            | type-specifier ID LCOL RCOL
                { $$ = newDeclNode(ArrParamK);
                  $$->attr.type = $1;
                  $$->lineno = lineno;
                  $$->child[0] = newExpNode(IdK);
                  $$->child[0]->attr.name = savedName;
                  $$->child[0]->lineno = lineno;
                }
            ;
composed-decl : LBRACK local-decls stmt-list LBRACK
                  { YYSTYPE t = $2;
                    if (t != NULL) {
                      while (t->sibling != NULL)
                        t = t->sibling;
                      t->sibling = $3;
                      $$ = $2; 
                    }
                    else $$ = $3;
                  }
              ;
local-decls : local-decls var-decl
                { YYSTYPE t = $1;
                  if (t != NULL) {
                    while (t->sibling != NULL)
                      t = t->sibling;
                    t->sibling = $2;
                    $$ = $1; 
                  }
                  else $$ = $2;
                }
            | { $$ = NULL; }
stmt-list   : stmt-list stmt 
                { YYSTYPE t = $1;
                  if (t != NULL) {
                    while (t->sibling != NULL)
                      t = t->sibling;
                    t->sibling = $2;
                    $$ = $1; 
                  }
                  else $$ = $2;
                }
            | { $$ = NULL; }
            ;
stmt        : exp-decl      { $$ = $1; }
            | composed-decl { $$ = $1; }
            | select-decl   { $$ = $1; }
            | iter-decl     { $$ = $1; }
            | rtn-decl      { $$ = $1; }
            ;
exp-decl    : exp SEMI { $$ = $1; }
            | SEMI     { $$ = NULL; }
            ;
select-decl : IF LPAREN exp RPAREN stmt 
                { $$ = newStmtNode(IfK);
                  $$->child[0] = $3;
                  $$->child[1] = $5;
                }
            | IF LPAREN exp RPAREN stmt ELSE stmt
                { $$ = newStmtNode(IfK);
                  $$->child[0] = $3;
                  $$->child[1] = $5;
                  $$->child[2] = $7;
                }
            ;
iter-decl   : WHILE LPAREN exp RPAREN stmt
                { $$ = newStmtNode(WhileK);
                  $$->child[0] = $3;
                  $$->child[1] = $5;
                }
            ;
rtn-decl    : RETURN SEMI 
                { $$ = newStmtNode(ReturnK); 
                  $$->lineno = lineno;
                }
            | RETURN exp SEMI
                { $$ = newStmtNode(ReturnK); 
                  $$->child[0] = $2;
                  $$->lineno = lineno;
                }
            ;
exp         : var EQ exp 
              { $$ = newStmtNode(AssignK);
                $$->attr.name = savedName;
                $$->lineno = savedLineNo;
                $$->child[0] = $3;
              }
            | simple_exp { $$ = $1; }
            ;
var         : ID 
              { savedName = copyString(tokenString);
                savedLineNo = lineno;
              }
            | ID LCOL exp RCOL
              { savedName = copyString(tokenString);
                savedLineNo = lineno;
              }
            ;
simple_exp  : sum-exp relational sum-exp 
              { $$ = newExpNode(OpK);
                $$->child[0] = $1;
                $$->child[1] = $3;
                $$->attr.op = $2;
              }
            | sum-exp { $$ = $1; }
            ;
relational  : LTE { $$ = $1; }
            | LT  { $$ = $1; }
            | GT  { $$ = $1; }
            | GTE { $$ = $1; }
            | EQ  { $$ = $1; }
            | DIF { $$ = $1; }
            ;
sum-exp     : sum-exp sum term 
              { $$ = newExpNode(OpK);
                $$->child[0] = $1;
                $$->child[1] = $3;
                $$->attr.op = $2;
              }
            | term { $$ = $1; }
            ;
sum         : PLUS  { $$ = $1; }
            | MINUS { $$ = $1; }
            ;
term        : term mult factor
                { $$ = newExpNode(OpK);
                  $$->child[0] = $1;
                  $$->child[1] = $3;
                  $$->attr.op = $2;
                }
            | factor { $$ = $1; }
            ;
mult        : TIMES { $$ = $1; }
            | OVER  { $$ = $1; }
            ;
factor      : LPAREN exp RPAREN  { $$ = $2; }
            | var
                { $$ = newExpNode(IdK);
                  $$->attr.val = atoi(tokenString);
                }
            | act 
            | NUM
                { $$ = newExpNode(ConstK);
                  $$->attr.val = atoi(tokenString);
                }
            ;
act         : ID 
                { savedName = copyString(tokenString);
                  savedLineNo = lineno; 
                }
              LPAREN args RPAREN
                { $$ = $2;
                  $$ = newExpNode(CallK);
                  $$->child[0] = $4;
                }
            ;
args        : arg-list { $$ = $1; }
            | { $$ = NULL; }
            ;
arg-list    : arg-list COMMA exp 
            | exp
              { $$ = $2; }
            ;
%%

int yyerror(char * message)
{ pce("Syntax error at line %d: %s\n",lineno,message);
  pce("Current token: ");
  printToken(yychar,tokenString);
  Error = TRUE;
  return 0;
}

/* yylex calls getToken to make Yacc/Bison output
 * compatible with ealier versions of the TINY scanner
 */
static int yylex(void)
{ return getToken(); }

TreeNode * parse(void)
{ yyparse();
  return savedTree;
}


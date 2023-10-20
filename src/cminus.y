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
static int savedNumber;
static TreeNode * savedTree; /* stores syntax tree for later return */
static int yylex(void);
int yyerror(char *);

%}

%token IF ELSE VOID INT WHILE RETURN
%token ID NUM 
%token ASSIGN EQ LT LTE GT GTE DIF PLUS MINUS TIMES OVER LPAREN RPAREN SEMI COMMA LCOL RCOL LBRACK RBRACK
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
id          : ID 
                { savedName = copyString(tokenString);
                  savedLineNo = lineno;
                }
            ;
num         : NUM 
                { savedNumber = atoi(tokenString);
                  savedLineNo = lineno;
                }
            ;
var-decl    : type-specifier id SEMI 
                { $$ = newDeclNode(VarK);
                  $$->attr.type = $1;
                  $$->lineno = savedLineNo;
                  $$->child[0] = newExpNode(IdK);
                  $$->child[0]->attr.name = savedName;
                }
            | type-specifier id LCOL num RCOL SEMI
                { $$ = newDeclNode(ArrVarK);
                  $$->attr.type = $1;
                  $$->lineno = savedLineNo;
                  $$->child[0] = newExpNode(IdK);
                  $$->child[0]->attr.name = savedName;
                  $$->child[0]->child[0] = newExpNode(ConstK);
                  $$->child[0]->child[0]->attr.val = savedNumber;
                }
            ;
type-specifier  : INT  { $$ = INT; }
                | VOID { $$ = VOID; }
                ;
fun-decl    : type-specifier id 
                { $$ = newDeclNode(FunK);
                  $$->attr.type = $1;
                  $$->lineno = savedLineNo;
                  $$->child[0] = newExpNode(IdK);
                  $$->child[0]->attr.name = savedName;
                }
              LPAREN params RPAREN composed-decl
                { $$ = $3;
                  $$->child[0]->child[0] = $5;
                  $$->child[0]->child[1] = $7;
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
param       : type-specifier id 
                { $$ = newDeclNode(ParamK);
                  $$->attr.type = $1;
                  $$->lineno = savedLineNo;
                  $$->child[0] = newExpNode(IdK);
                  $$->child[0]->attr.name = savedName;
                }
            | type-specifier id LCOL RCOL
                { $$ = newDeclNode(ArrParamK);
                  $$->attr.type = $1;
                  $$->lineno = savedLineNo;
                  $$->child[0] = newExpNode(IdK);
                  $$->child[0]->attr.name = savedName;
                }
            ;
composed-decl : LBRACK local-decls stmt-list RBRACK
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
exp         : var ASSIGN exp 
              { $$ = newStmtNode(AssignK);
                $$->child[0] = $1;
                $$->child[1] = $3;
              }
            | simple_exp { $$ = $1; }
            ;
var         : id 
              { $$ = newExpNode(IdK);
                $$->attr.name = savedName;
              }
            | id LCOL exp RCOL
              { $$ = newExpNode(IdK);
                $$->attr.name = savedName;
                $$->child[0] = $3;
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
relational  : LTE { $$ = LTE; }
            | LT  { $$ = LT; }
            | GT  { $$ = GT; }
            | GTE { $$ = GTE; }
            | EQ  { $$ = EQ; }
            | DIF { $$ = DIF; }
            ;
sum-exp     : sum-exp sum term 
              { $$ = newExpNode(OpK);
                $$->child[0] = $1;
                $$->child[1] = $3;
                $$->attr.op = $2;
              }
            | term { $$ = $1; }
            ;
sum         : PLUS  { $$ = PLUS; }
            | MINUS { $$ = MINUS; }
            ;
term        : term mult factor
                { $$ = newExpNode(OpK);
                  $$->child[0] = $1;
                  $$->child[1] = $3;
                  $$->attr.op = $2;
                }
            | factor { $$ = $1; }
            ;
mult        : TIMES { $$ = TIMES; }
            | OVER  { $$ = OVER; }
            ;
factor      : LPAREN exp RPAREN  { $$ = $2; }
            | var { $$ = $1; }
            | act { $$ = $1; }
            | num
                { $$ = newExpNode(ConstK);
                  $$->attr.val = savedNumber;
                }
            ;
act         : id 
                { $$ = newExpNode(CallK);
                  $$->attr.name = savedName;
                }
              LPAREN args RPAREN
                { $$ = $2;
                  $$->child[0] = $4;
                }
            ;
args        : arg-list { $$ = $1; }
            | { $$ = NULL; }
            ;
arg-list    : arg-list COMMA exp 
                { YYSTYPE t = $1;
                    if (t != NULL) {
                      while (t->sibling != NULL)
                        t = t->sibling;
                      t->sibling = $3;
                      $$ = $1; 
                    }
                    else $$ = $3;
                  }
            | exp { $$ = $1; }
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


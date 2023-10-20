/****************************************************/
/* File: cminus.l                                   */
/* Lex specification for C-                         */
/* Project for CES41: Compiladores                  */
/****************************************************/

%{
#include "globals.h"
#include "util.h"
#include "scan.h"
/* lexeme of identifier or reserved word */
char tokenString[MAXTOKENLEN+1];
%}
digit       [0-9]
number      {digit}+
letter      [a-zA-Z]
identifier  {letter}+
newline     \n
whitespace  [ \t\r]+
%%
"else"          {return ELSE;}
"if"            {return IF;}
"int"           {return INT;}
"return"        {return RETURN;}
"void"          {return VOID;}
"while"         {return WHILE;}
"+"             {return PLUS;}
"-"             {return MINUS;}
"*"             {return TIMES;}
"/"             {return OVER;}
"<"             {return LT;}
"<="            {return LTE;}
">"             {return GT;}
">="            {return GTE;}
"=="            {return EQ;}
"!="            {return DIF;}
"="             {return ASSIGN;}
"("             {return LPAREN;}
")"             {return RPAREN;}
";"             {return SEMI;}
","             {return COMMA;}
"["             {return LCOL;}
"]"             {return RCOL;}
"{"             {return LBRACK;}
"}"             {return RBRACK;}
{number}        {return NUM;}
{identifier}    {return ID;}
{newline}       {lineno++;}
{whitespace}    {/* skip whitespace */}
"/*"            { char c1; char c2;
                  do
                  { c1 = input();
                    if (c1 == EOF) break;
                    if (c1 == '\n') lineno++;
                  } while (c1 != '*' || (c2 = input()) != '/');
                }
.               {return ERROR;}
%%

int lineit = 1;

TokenType getToken(FILE * source2)
{ static int firstTime = TRUE;
  TokenType currentToken;

  char * buffer = NULL;
  size_t buffer_size = 0;

  if (firstTime)
  { firstTime = FALSE;
    lineit = 1;
    lineno++;
    yyin = source;
    yyout = listing;
  }
  
  currentToken = yylex();
  strncpy(tokenString,yytext,MAXTOKENLEN);
  if (TraceScan) {

    while(lineit <= lineno){
      fprintf(listing,"%d: ",lineit);
      getline(&buffer, &buffer_size, source2);
      fprintf(listing, "%s", buffer);
      lineit++;
    }

    fprintf(listing,"\t%d: ",lineno);
    printToken(currentToken,tokenString);
  }
  
  return currentToken;
}
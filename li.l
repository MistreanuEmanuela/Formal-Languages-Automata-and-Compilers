%{
#include<stdbool.h>
#include <stdio.h>
#include "y.tab.h"
#include<stdlib.h>
#include"structuri.h"

%}
%option noyywrap

%%
"int" {yylval.sir= strdup(yytext); return TIP;}
"string" {yylval.sir=strdup(yytext); return TIP;} 
"float" {yylval.sir= strdup(yytext); return TIP;}
"char" {yylval.sir= strdup(yytext); return TIP;}
"clasa" {yylval.sir= strdup(yytext); return Clasa;}
"bool" {yylval.sir= strdup(yytext); return TIP;}
"begin_progr" {return BGIN;}
"end_progr" {return END;}
"const" {return CONST;}
"return" {return Return;}
"TypeOf" {return TYPEOF;}
"eval" {yylval.sir= strdup(yytext); return Eval;}
"->" {return assign;}
"+" {return PLUS;}
"-" {return MINUS;}
"*" {return ORI;}
"/" {return DIV;}
"==" {return egal;}
"<" {return maimic;}
">" {return maimare;}
"<=" {return micegal;}
">=" {return mareegal;}
"if" {return IF;}
"for" {return FOR;}
"else" {return ELSE;}
"while" {return WHILE;}
"true"|"false" {yylval.sir= strdup(yytext); return Bool;}
[_a-zA-Z][_a-zA-Z0-9]* {yylval.sir=strdup(yytext); return ID;} 
\'[a-zA-Z]\' {yylval.sir=strdup(yytext); return litere;}
["][a-zA-Z]*["] {yylval.sir=strdup(yytext); return String;}
0|[1-9][0-9]* {yylval.numar=atoi(yytext); return Numar;}

[0-9]+[,][0-9]+  {yylval.real_nr=atof(yytext); return Real;}
[ \t] ;
\n {yylineno++;}
. {return yytext[0];}

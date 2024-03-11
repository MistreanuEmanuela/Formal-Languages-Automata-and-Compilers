%{
#include<stdbool.h>
#include "structuri.h"
#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<stdarg.h>
extern FILE* yyin;
extern char* yytext;
extern int yylineno;
int yylex();
int tip_functie=0;
char scop[100]="global";
int nr_var=0;
int nr_functii=0;
int nr_parametrii=0;
char nume_parametru[10][10];
variabile var[100];
functie f[100];
int nr_p;
int nr_par;
char nume_cl[100];
char nume_f[10];
int a[10];
void creare_tabele();
int Decl(char *nume);
 int Decl_functie(char *nume);
  int nr_varb(char *nume);
 void declarare_variabila(char *nume, char *tip, char *scope, bool Constanta, bool Declarat, bool Init, bool Vector);
void declarare_functie(char *nume, char *tip, int nr, bool Declarat, int b[10], char d[10][10]);
int return_tip( char *nume);
expresii* expresie_int(int valoare);
void initializare(char *nume, expresii* expr);
expresii* expresie_float(float valoare);
expresii* expresie_bool(bool valoare);
expresii* expresie_char(char valoare);
 int nr_functie(char *nume);
expresii* expresie_string(char* valoare, char* valoare2);
expresii* expresie(char* nume);
%}

%union {
 int numar;
 float real_nr;
 char* sir;
 char caract;
 struct expresii* Expr;
 };
 
%token <sir> ID TIP Clasa Eval Bool String
%token <caract> litere
%token <numar> Numar
%token< real_nr> Real
%token IF FOR ELSE WHILE CONST assign
%token PLUS MINUS ORI DIV
%token maimic maimare egal mareegal micegal
%token BGIN END Return TYPEOF
%type <Expr> valoare val
%left PLUS ORI DIV MINUS maimic maimare egal mareegal micegal
%start program
%%
program: program_global BGIN '{'program_local sf END '}' {printf("PROGRAM CORECT SINTACTIC\n");}
       ;
program_global : declaratii_variabile_globale ';'
               | declaratii_vector ';'
               | program_global declaratii_variabile_globale ';'
               | program_global declaratii_vector ';'
               | clasa ';'
               | program_global clasa ';'
               | constante ';'
               | program_global constante ';'
               | decl_functii 
               |program_global decl_functii 
               | lista_instructiuni ';'
               | program_global lista_instructiuni ';'
               ;
               
 declaratii_variabile_globale :  TIP ID  { if( Decl($2)==0)
                           { declarare_variabila ($2, $1, scop, false, true, false, false); free($2); free($1);}
                           else
                            {printf("[%d]... Variabila a fost declarata anterior\n", yylineno); exit(1);}
                            }
               ; 
 declaratii_vector  : TIP ID '[' Numar ']'  {if( Decl($2)==0)
                                            { declarare_variabila( $2, "vector" , scop, false, true, false, true);
                                             char nume_sir[100], index[100];
                                             int i;
                                             bzero(nume_sir, sizeof(nume_sir));
                                             for (i=0; i< $4; i++)
                                             {
                                              bzero(index,sizeof(index));
                                              strcat(nume_sir,$2);
                                              strcat(nume_sir,"[");
                                              sprintf(index,"%d", i);
                                              strcat(nume_sir, index);
                                              strcat(nume_sir, "]");
                                              declarare_variabila(nume_sir, $1, scop, false, true, false, false); 
                                              bzero(nume_sir, sizeof(nume_sir));
                                              }
                                              }
                                              else
                                              {printf("[%d]... Variabila a fost declarata anterior\n", yylineno); exit(2);}
                                                free($2); free($1);   }     
                     ;  
clasa: declaratii_clase '{' declaratii_campuri '}'
     ;                  
declaratii_clase: Clasa ID {                    bzero(nume_cl, sizeof(nume_cl)) ; strcat(nume_cl, $2);
                                              if( Decl($2)==0)
                                               { declarare_variabila( $2, "clasa" , scop, false, true, false, false); }                                                             
                                                else
                                              {printf("[%d]... Variabila a fost declarata anterior\n", yylineno); exit(3);}
                                                free($2); free($1);   } 
                ;
 declaratii_campuri:   declaratii_variabile_clasa ';'
                  | declaratii_vector_clasa ';'
                  | declaratii_campuri declaratii_variabile_clasa ';'
                  | declaratii_campuri declaratii_vector_clasa ';'
                ;
               
 declaratii_variabile_clasa :  TIP ID  { if( Decl($2)==0)
                            {
                             char nume1[100]; bzero(nume1, sizeof(nume1)); strcat(nume1, nume_cl); strcat(nume1,"."); strcat(nume1, $2);
                            declarare_variabila (nume1, $1, scop, false, true, false, false); free($2); free($1); }
                           else
                           {printf("[%d]... Variabila a fost declarata anterior\n", yylineno); exit(4);}
                            }
               ; 
 declaratii_vector_clasa  : TIP ID '[' Numar ']' { if( Decl($2)==0)
                                            {
                                            declarare_variabila( $2, "vector" , scop, false, true, false, true);
                                             char nume_sir[100], index[100];
                                             int i;
                                             bzero(nume_sir, sizeof(nume_sir));
                                             for (i=0; i< $4; i++)
                                             {
                                              bzero(index,sizeof(index));
                                              strcat(nume_sir,nume_cl);
                                              strcat(nume_sir,"."); 
                                              strcat(nume_sir,$2);
                                              strcat(nume_sir,"[");
                                              sprintf(index,"%d", i);
                                              strcat(nume_sir, index);
                                              strcat(nume_sir, "]");
                                              declarare_variabila(nume_sir, $1, scop, false, true, false, false); 
                                              bzero(nume_sir, sizeof(nume_sir));
                                              }
                                              }
                                              else
                                             {printf("[%d]... Variabila a fost declarata anterior\n", yylineno); exit(5);}
                                                free($2); free($1);   }     
                     ;     
constante: CONST TIP ID {   if( Decl($3)==0)
                           { declarare_variabila ($3, $2, "const global", true, true, false, false); free($3); free($2);}
                           else
                            {printf("[%d]... Variabila a fost declarata anterior\n", yylineno); exit(6);}
                            }    
         ;
decl_functii : TIP ID '(' parametri ')' '{' instructiuni_functie  Return ID ';' '}' 
{ strcpy(nume_f, $2);    if(Decl_functie($2)==0) { declarare_functie ($2, $1, nr_parametrii, true, a, nume_parametru); }
else { printf("[%d]... Functia a fost declarata anterior\n", yylineno); exit(7);}
 free($2); free($1); nr_parametrii=0;}
 ;
parametri: parametri ',' parametru 
          | parametru 
          ;
parametru: TIP ID {nr_parametrii++; if(strcmp($1,"int")==0){a[nr_parametrii]=1;} if(strcmp($1,"float")==0){a[nr_parametrii]=2;} if(strcmp($1,"bool")==0){a[nr_parametrii]=3;} if(strcmp($1,"char")==0){a[nr_parametrii]=4;} if(strcmp($1,"string")==0){a[nr_parametrii]=5;}
 bzero(nume_parametru[nr_parametrii],10); strcpy(nume_parametru[nr_parametrii],$2); free($2); free ($1);}
         ;
instructiuni_functie: declaratii_variabile_functie ';'   
                    | declaratii_vector_functie ';' 
                    | instructiuni_functie declaratii_vector_functie ';'       
                    | instructiuni_functie declaratii_variabile_functie ';'  
                    | instructiuni_functie lista_instructiuni ';'
                    ;

declaratii_variabile_functie: TIP ID { if(Decl($2)==0)
                                    {
                                        char nume1[100];
                                        bzero(nume1, sizeof(nume1));
                                        strcat(nume1, nume_cl);
                                        strcat(nume1, $2);
                                        declarare_variabila($2, $1, "scop intr-o functie", false, true, false, false);
                                    }
                                    else
                                    {
                                       {printf("[%d]... Variabila a fost declarata anterior\n", yylineno); exit(8);};
                                    }
                                    free($2);
                                    free($1);
                                }
               ;

declaratii_vector_functie : TIP ID '[' Numar ']' { if(Decl($2)==0)
                                                {
                                                    declarare_variabila( $2, "vector" , "scop_functie", false, true, false, true);
                                                    char nume_sir[100], index[100];
                                                    int i;
                                                    bzero(nume_sir, sizeof(nume_sir));
                                                    for(i=0; i<$4; i++);
                                                    {
                                                        bzero(index,sizeof(index));
                                                        strcat(nume_sir, $2);
                                                        strcat(nume_sir,"[");
                                                        sprintf(index,"%d", i);
                                                        strcat(nume_sir, index);
                                                        strcat(nume_sir, "]");
                                                        declarare_variabila(nume_sir, $1, "scop intr-o functie", false, true, false, false); 
                                                        bzero(nume_sir, sizeof(nume_sir));
                                                    }
                                                }
                                                else
                                                {
                                                    printf("[%d]... Variabila a fost declarata anterior\n", yylineno); exit(9);
                                                } 
                                                free($2);
                                                free($1);
                                            }     
                     ;
 lista_instructiuni: ID assign valoare { 
                                        if (Decl($1)==1) { 
                                                       if(return_tip($1)==$3->tip){ initializare($1, $3);} 
                                                           else{printf("[%d]... Incompatibilitate tipuri asignare\n", yylineno); exit(10);}}
                                      else{ printf("[%d]... Variabila nu a fost declarata anterior\n", yylineno);exit(11);}  free($1); free($3);}
                                      
                   | ID '[' Numar ']' assign valoare { char nume[20];  bzero(nume ,20); char i[10]; bzero(i,10); strcat(nume, $1); strcat(nume, "["); sprintf(i, "%d", $3); strcat(nume, i);
                   strcat(nume ,"]");   if (Decl(nume)==1) { 
                                                       if(return_tip(nume)==$6->tip){ initializare(nume, $6);} 
                                                           else{printf("[%d]... Incompatibilitate tipuri asignare\n", yylineno); exit(12);}}
                                      else{ printf("[%d]... Variabila nu a fost declarata anterior\n", yylineno);exit(13);}  free($1); free($6);} 
                   | ID '.' ID assign valoare { char nume[20];  bzero(nume ,20); strcat(nume, $1); strcat(nume, ".") ; strcat(nume, $3);  
                                                        if (Decl(nume)==1) { 
                                                       if(return_tip(nume)==$5->tip){ initializare(nume, $5);} 
                                                          else{printf("[%d]... Incompatibilitate tipuri asignare\n", yylineno); exit(14);}}
                                      else{ printf("[%d]... Variabila nu a fost declarata anterior\n", yylineno);exit(15);}  free($1); free($5);}     
                 | ID '.' ID '[' Numar ']' assign valoare { char i[10]; bzero(i,10);   char nume[20];  bzero(nume ,20); strcat(nume, $1); strcat(nume, ".") ; strcat(nume, $3);  strcat(nume, "["); sprintf(i, "%d", $5); strcat(nume, i);  strcat(nume ,"]"); 
                                                     if (Decl(nume)==1) { 
                                                       if(return_tip(nume)==$8->tip){ initializare(nume, $8);} 
                                                           else{printf("[%d]... Incompatibilitate tipuri asignare\n", yylineno); exit(16);}}
                                      else{ printf("[%d]... Variabila nu a fost declarata anterior\n", yylineno);exit(17);}  free($1); free($8);}    
                 | IF '(' val ')' '{' instructiuni  '}' { if($3->tip!=3){printf("[%d]... If-ul nu a fost apelat cu o valoare bool\n", yylineno); exit(18);} free($3);}   
                 | WHILE '(' val ')' '{' instructiuni  '}' { if($3->tip!=3){ printf("[%d]... While-ul nu a fost apelat cu o valoare bool\n", yylineno);exit(19); } free($3);}  
                 | FOR '(' init ';' val ';' init ')' '{' instructiuni '}' { if($5->tip!=3){  printf("[%d]... Conditia for-ului nu este de tip bool\n",yylineno); exit(20); } free($5);}   
                 | call_f
                 | TYPEOF '(' val ')' {if($3->tip==1) {printf("[%d] Tipul variabilei este int\n", yylineno);}
                                       if($3->tip==2) {printf("[%d] Tipul variabilei este float\n", yylineno);}
                                       if($3->tip==3) {printf("[%d] Tipul variabilei este bool\n", yylineno);}
                                       if($3->tip==4) {printf("[%d] Tipul variabilei este char\n", yylineno);}
                                       if($3->tip==5) {printf("[%d] Tipul variabilei este string\n", yylineno);}
                                       }
                 |Eval '(' val ')' { if($3->tip==1) {printf("[%d] Valoarea variabilei este %d\n", yylineno, $3->intreg);} else {printf("[%d] Expresia nu este de tip int\n",yylineno ); exit(21);}}                        
                 ;
 call_f   : ID '('apel')' { bzero(nume_f, 10); strcpy(nume_f,$1); if(Decl_functie($1)==1){ bzero(nume_f, 10); strcpy(nume_f,$1); nr_p=f[nr_functie($1)].nr_parametri; if(f[nr_functie($1)].nr_parametri!=nr_par){printf("[%d] Numarul de parametri cu care ati apelat functia nu corespunde cu numarul declarat de parametri\n",yylineno);exit(22);} nr_par=0;} else {printf("[%d] Functia %s nu a fost declarata anterior\n", yylineno, nume_f); exit(23);}}
          ;
 apel: valoare  {if(Decl_functie(nume_f)==1) { nr_par++; if(f[nr_functie(nume_f)].p[nr_par]!= $1->tip){ printf("[%d]...Functia a fost apelata cu parametrii de tip gresit\n",yylineno);exit(24);}}}
     | apel ';' valoare {if(Decl_functie(nume_f)==1) { nr_par++; if(f[nr_functie(nume_f)].p[nr_par]!= $3->tip){ printf("[%d]...Functia a fost apelata cu parametrii de tip gresit\n",yylineno);exit(25);}}}
     ;                         
 init: ID assign valoare { 
                                        if (Decl($1)==1) { 
                                                       if(return_tip($1)==$3->tip){ initializare($1, $3);} 
                                                          else{printf("[%d]... Incompatibilitate tipuri asignare\n", yylineno);exit(26);}}
                                      else{ printf("[%d]... Variabila nu a fost declarata anterior\n", yylineno);exit(27);} free($1); free($3);} 
                  ;                                         
 instructiuni: instructiuni lista_instructiuni ';'
            | lista_instructiuni ';' 
            ;          
 valoare: val {    $$=$1; }
        | litere  { $$=expresie_char($1);}
        | Bool { $$= expresie_bool($1);}
        ; 
 val: Numar {        $$= expresie_int($1);}
    | String {$$=expresie_string($1, NULL); free($1);} 
    | Real {   $$=expresie_float($1);}  
    | val PLUS val { if($1->tip==$3->tip) { if($1->tip==1){ $$=expresie_int($1->intreg + $3->intreg);} if($1->tip==2){ $$=expresie_float($1->real + $3->real);} if($1->tip==5){ $$=expresie_string($1->string, $3->string);}} else{printf("[%d]...Expresiile sunt de tipuri diferite)\n", yylineno);exit(28);}free($1); free($3); }
    | val MINUS val { if($1->tip==$3->tip) { if($1->tip==1){ $$=expresie_int($1->intreg - $3->intreg);} if($1->tip==2) { $$=expresie_float($1->real - $3->real);}} else{printf("[%d]...Expresiile sunt de tipuri diferite)\n", yylineno);exit(29);}free($1); free($3);}
    | val ORI val { if($1->tip==$3->tip) { if($1->tip==1){ $$=expresie_int($1->intreg * $3->intreg);} if($1->tip==2) { $$=expresie_float($1->real * $3->real);}} else{printf("[%d]...Expresiile sunt de tipuri diferite)\n", yylineno);exit(30);}free($1); free($3);}
    | val DIV val { if($1->tip==$3->tip) { if($1->tip==1){ $$=expresie_int($1->intreg / $3->intreg);}  if($1->tip==2) { $$=expresie_float($1->real / $3->real);}}else{printf("[%d]...Expresiile sunt de tipuri diferite)\n", yylineno);exit(31);}free($1); free($3);}
    | ID { if(Decl($1)==1) {if(var[nr_varb($1)].init==true){$$=expresie($1);} else{printf("[%d] Asignare esuata... incercati sa asignati o variabila fara valoare\n",yylineno);exit(55);}} else {printf("[%d]... Variabila nu a fost declarata anterior\n", yylineno);exit(32);}  free($1);} 
    | ID '[' Numar ']' { char nume[20];  bzero(nume ,20); char i[10]; bzero(i,10); strcat(nume, $1); strcat(nume, "["); sprintf(i, "%d", $3); strcat(nume, i);
                   strcat(nume ,"]"); if(Decl(nume)==1) {if(var[nr_varb(nume)].init==true){$$=expresie(nume);} else{printf("[%d] Asignare esuata... incercati sa asignati o variabila fara valoare\n",yylineno);exit(56);}}  else {printf("[%d]... Variabila nu a fost declarata anterior\n", yylineno);exit(33);}  free($1);} 
    | ID '.' ID '[' Numar ']' { char i[10]; bzero(i,10);   char nume[20];  bzero(nume ,20); strcat(nume, $1); strcat(nume, ".") ; strcat(nume, $3);  strcat(nume, "["); sprintf(i, "%d", $5); strcat(nume, i);  strcat(nume ,"]");  if(Decl(nume)==1) {if(var[nr_varb(nume)].init==true){ $$=expresie(nume);}else{printf("[%d] Asignare esuata... incercati sa asignati o variabila fara valoare\n",yylineno);exit(57);}}   else {printf("[%d]... Variabila nu a fost declarata anterior\n", yylineno);exit(34);}  free($1);} 
    | ID '.' ID  { char nume[20];  bzero(nume ,20); strcat(nume, $1); strcat(nume, ".") ; strcat(nume, $3);    if(Decl(nume)==1) {if(var[nr_varb(nume)].init==true){ $$=expresie(nume);} else{printf("[%d] Asignare esuata... incercati sa asignati o variabila fara valoare\n",yylineno);exit(58);}} else {printf("[%d]... Variabila nu a fost declarata anterior\n", yylineno);exit(35);}  free($1);}  
    | val maimic val { if($1->tip==$3->tip) { if($1->tip==1){ $$=expresie_bool($1->intreg < $3->intreg);} if($1->tip==2){ $$=expresie_bool($1->real < $3->real);}} else{printf("[%d]...Expresiile sunt de tipuri diferite)\n", yylineno);exit(36);} free($1); free($3);}   
    | val maimare val { if($1->tip==$3->tip) { if($1->tip==1){ $$=expresie_bool($1->intreg > $3->intreg);} if($1->tip==2){ $$=expresie_bool($1->real > $3->real);}} else{printf("[%d]...Expresiile sunt de tipuri diferite)\n", yylineno);exit(37);} free($1); free($3);} 
    | val egal val { if($1->tip==$3->tip) { if($1->tip==1){ $$=expresie_bool($1->intreg == $3->intreg);} if($1->tip==2){ $$=expresie_bool($1->real == $3->real);}} else{printf("[%d]...Expresiile sunt de tipuri diferite)\n", yylineno);exit(38);}free($1); free($3);}   
    | val micegal val { if($1->tip==$3->tip) { if($1->tip==1){ $$=expresie_bool($1->intreg <= $3->intreg);} if($1->tip==2){ $$=expresie_bool($1->real <= $3->real);}} else{printf("[%d]...Expresiile sunt de tipuri diferite)\n", yylineno);exit(39);}free($1); free($3);}
    | val mareegal val { if($1->tip==$3->tip) { if($1->tip==1){ $$=expresie_bool($1->intreg >= $3->intreg);} if($1->tip==2){ $$=expresie_bool($1->real >= $3->real);}} else{printf("[%d]...Expresiile sunt de tipuri diferite)\n", yylineno);exit(40);}free($1); free($3);} 
    | ID '('apel')' { bzero(nume_f, 10); strcpy(nume_f,$1); if(Decl_functie($1)==1) {bzero(nume_f, 10); strcpy(nume_f,$1); nr_p=f[nr_functie($1)].nr_parametri; if(f[nr_functie($1)].nr_parametri!=nr_par){printf("[%d]...Numarul de parametri cu care ati apelat functia nu corespunde cu declaratia functiei\n",yylineno);exit(42);} nr_par=0; if(strcmp(f[nr_functie($1)].tip_return, "int")==0){$$=expresie_int(0);} if(strcmp(f[nr_functie($1)].tip_return, "float")==0){$$=expresie_float(0);} if(strcmp(f[nr_functie($1)].tip_return, "bool")==0){$$=expresie_bool(false);} } else {printf("[%d]...Functia %s nu a fost declarata anterior\n", yylineno, nume_f);exit(46);}}
                    
    ;
    
 program_local   : declaratii_variabile_program ';'
               | declaratii_vector_program ';'
               | program_local  declaratii_variabile_program ';'
               | program_local   declaratii_vector_program ';'
               | constante_program ';'
               | program_local  BGIN  constante_program ';'
               | lista_instructiuni ';'
               | program_local  lista_instructiuni ';'
               ;
            
declaratii_variabile_program:  TIP ID  { if( Decl($2)==0)
                           { declarare_variabila ($2, $1, "program", false, true, false, false); free($2); free($1);}
                           else
                           {printf("[%d]... Variabila a fost declarata anterior\n", yylineno);exit(43);}
                            }
               ; 
 declaratii_vector_program  : TIP ID '[' Numar ']'  {if( Decl($2)==0)
                                            { declarare_variabila( $2, "vector" , "program", false, true, false, true);
                                             char nume_sir[100], index[100];
                                             int i;
                                             bzero(nume_sir, sizeof(nume_sir));
                                             for (i=0; i< $4; i++)
                                             {
                                              bzero(index,sizeof(index));
                                              strcat(nume_sir,$2);
                                              strcat(nume_sir,"[");
                                              sprintf(index,"%d", i);
                                              strcat(nume_sir, index);
                                              strcat(nume_sir, "]");
                                              declarare_variabila(nume_sir, $1,"program", false, true, false, false); 
                                              bzero(nume_sir, sizeof(nume_sir));
                                              }
                                              }
                                              else
                                              {printf("[%d]... Variabila a fost declarata anterior\n", yylineno);exit(44);}
                                                free($2); free($1);   }     
                     ;
constante_program: CONST TIP ID {   if( Decl($3)==0)
                           { declarare_variabila ($3, $2, "const program", true, true, false, false); free($3); free($2);}
                           else
                            {printf("[%d]... Variabila a fost declarata anterior\n", yylineno);exit(45);}
                            } 
                 ;                
  sf :{creare_tabele();}
    ;                                                                                                         
%% 
int Decl(char *nume)
{
 for (int i=0; i<=nr_var; i++)
   { 
     if(strcmp(var[i].nume, nume)==0)
        return 1;
        }
        return 0;
        };
 int Decl_functie(char *nume)
{
 for (int i=0; i<=nr_functii; i++)
   { 
     if(strcmp(f[i].nume, nume)==0)
        return 1;
        }
        return 0;
        };
void declarare_variabila(char *nume, char *tip, char *scope, bool Constanta, bool Declarat, bool Init, bool Vector)
{
  if(Decl(nume)==0)
  {
   nr_var++;
   strcat(var[nr_var].nume, nume);
   strcat(var[nr_var].tip, tip);
   strcat(var[nr_var].scop, scope);
   var[nr_var].constanta=Constanta;
   var[nr_var].declarat=Declarat;
   var[nr_var].init=Init;
   var[nr_var].vector=Vector;
   }
   };
   void declarare_functie(char *nume, char *tip, int nr, bool Declarat, int b[10], char numep[10][10])
{
  if(Decl_functie(nume)==0)
  {
   nr_functii++;
   strcat(f[nr_functii].nume, nume);
   strcat(f[nr_functii].tip_return, tip);
   f[nr_functii].declarat=Declarat;
   f[nr_functii].init=false;
   f[nr_functii].nr_parametri=nr;
   for(int i=1; i<=nr; i++)
   {
    f[nr_functii].p[i]=b[i];
    bzero(f[nr_functii].nume_p[i],10);
    strcat(f[nr_functii].nume_p[i], numep[i]);
   }
   }
   };
 
int return_tip( char *nume)
{
  for (int i=0; i<=nr_var; i++)
   { 
     if(strcmp(var[i].nume, nume)==0)
     { 
       if(strcmp(var[i].tip, "int")==0){ return 1;}
       if(strcmp(var[i].tip, "float")==0){ return 2;}
       if(strcmp(var[i].tip, "bool")==0){ return 3;}
       if(strcmp(var[i].tip, "char")==0){ return 4;}
       if(strcmp(var[i].tip, "string")==0){ return 5;}
     }
     }
     return 0;
     };
 expresii* expresie_int(int valoare)
 {
   expresii* expr=(expresii*)malloc(sizeof(expresii));
   expr->intreg=valoare;
   expr->tip= 1;
   return expr;
   } ; 
  expresii* expresie_float(float valoare)
 {
   expresii* expr1=(expresii*)malloc(sizeof(expresii));
   expr1->real=valoare;
   expr1->tip= 2;
   return expr1;
   } ;  
    expresii* expresie_bool(bool valoare)
 {
   expresii* expr=(expresii*)malloc(sizeof(expresii));
   expr->bool_val=valoare;
   expr->tip= 3;
   return expr;
   } ;  
    expresii* expresie_char(char valoare)
 {
   expresii* expr=(expresii*)malloc(sizeof(expresii));
   expr->caracter=valoare;
   expr->tip= 4;
   return expr;
   } ; 
   
     expresii* expresie_string(char* valoare1, char* valoare2)
 {
   expresii* expr1=(expresii*)malloc(sizeof(expresii));
   int lungime=valoare2 ? strlen (valoare2):0;
   expr1->string=(char*)malloc (sizeof(char)*(strlen(valoare1)+lungime+1));
   strcpy(expr1->string, valoare1);
   //pentru + la string
   if(lungime!=0)
   { strcat(expr1->string, valoare2);}
   expr1->tip= 5;
   return expr1;
   } ;
    expresii* expresie(char* nume)
    { 
    for (int i=0; i<=nr_var; i++)
   { 
     if(strcmp(var[i].nume, nume)==0)
     {
      if(var[i].init==true)
      { 
       if( return_tip(var[i].nume)==1)
       { return expresie_int(var[i].intreg);}
       if(return_tip(var[i].nume)==2)
       { return expresie_float(var[i].real);}
       if( return_tip(var[i].nume)==3)
       { return expresie_bool(var[i].bool_val);}
          if( return_tip(var[i].nume)==4)
       { return expresie_char(var[i].caracter);}
       if( return_tip(var[i].nume)==5)
       { return  expresie_string(var[i].sir, NULL);}
     }
     }
     }
     };
 void initializare(char *nume, expresii* expr)
 {
     for (int i=0; i<=nr_var; i++)
   { 
     if(strcmp(var[i].nume, nume)==0){
     if(var[i].init==true && var[i].constanta==true)
      {   
      printf("[%d] Variabla constanta initializata anterior\n",yylineno); exit(47);
      }
      else
     { if(strcmp(var[i].tip, "int")==0)
      {
        if(var[i].init==false || var[i].init==true && var[i].constanta==false){
         var[i].init=true;
         var[i].intreg=expr->intreg;
         }
         }
         if(strcmp(var[i].tip, "float")==0)
      {
        if(var[i].init==false || var[i].init==true && var[i].constanta==false){
         var[i].init=true;
         var[i].real=expr->real;
         }
         }
         if(strcmp(var[i].tip, "bool")==0)
      {
        if(var[i].init==false || var[i].init==true && var[i].constanta==false){
         var[i].init=true;
         var[i].bool_val=expr->bool_val;
         }
         }
         if(strcmp(var[i].tip, "char")==0)
      {
        if(var[i].init==false || var[i].init==true && var[i].constanta==false){
         var[i].init=true;
        var[i].caracter=expr->caracter;
         }
         }
           if(strcmp(var[i].tip, "string")==0)
      {
        if(var[i].init==false || var[i].init==true && var[i].constanta==false){
         var[i].init=true;
        var[i].caracter=expr->caracter;
         }
         }
         }
         }
         }
         };
 int nr_functie(char *nume)
 {
  for(int i=0; i<=nr_functii; i++)
  {
    if(strcmp(nume, f[i].nume)==0)
    {
     return i;
     }
     
     }
     }; 
      int nr_varb(char *nume)
 {
  for(int i=0; i<=nr_var; i++)
  {
    if(strcmp(nume, var[i].nume)==0)
    {
     return i;
     }
     
     }
     }; 
 void creare_tabele()
 {
        remove("symbol_table.txt");
        FILE* fd=fopen("symbol_table.txt", "a");
        if (fd==NULL)
        {
         perror("vezi ca n-ai fisier");
         }
         fprintf(fd, "          VARIABILE      \n");  
         for(int i=1; i<=nr_var; i++)
          {
            fprintf(fd, "NUMELE VARIABILEI %s\n", var[i].nume); 
            fprintf(fd, "TIPUL VARIABILEI %s\n" , var[i].tip);
            fprintf(fd, "SCOPUL VARIABILEI %s\n", var[i].scop);
            if(return_tip(var[i].nume)==1)
             {fprintf(fd, "VALOAREA VARIABILEI %d\n", var[i].intreg);}
                        if(return_tip(var[i].nume)==2)
                        
             {fprintf(fd, "VALOAREA VARIABILEI %f\n", var[i].real);}
                        if(return_tip(var[i].nume)==3)
             {fprintf(fd, "VALOAREA VARIABILEI %d\n", var[i].bool_val);}
                        if(return_tip(var[i].nume)==4)
             {fprintf(fd, "VALOAREA VARIABILEI NULL\n");}
                        if(return_tip(var[i].nume)==5)
             {fprintf(fd, "VALOAREA VARIABILEI  %s\n",var[i].sir);}
             
            fprintf(fd, "\n \n");
            
            } 
            fclose(fd); 
            
            remove("symbol_table_functions.txt");
        FILE* fdd=fopen("symbol_table_functions.txt", "a");
        if (fdd==NULL)
        {
         perror("vezi ca n-ai fisier");
         }
         fprintf(fdd,"             FUNCTII        \n");
         fprintf(fdd, "\n");
          for(int i=1; i<=nr_functii; i++)
          {
           fprintf(fdd, "NUMELE FUNCTIEI %s\n", f[i].nume);
           fprintf(fdd, "TIP RETURN %s\n", f[i].tip_return);
           fprintf(fdd, "NUMAR PARAMETRI %d\n", f[i].nr_parametri);
           for(int j=1; j<=f[i].nr_parametri; j++)
           {
            if(f[i].p[j]==1){
            fprintf(fdd ,"TIPUL PARAMETRULUI %s este int\n",f[i].nume_p[j]);}
             if(f[i].p[j]==2){
            fprintf(fdd ,"TIPUL PARAMETRULUI %s este float\n",f[i].nume_p[j]);}
             if(f[i].p[j]==3){
            fprintf(fdd ,"TIPUL PARAMETRULUI %s este bool\n",f[i].nume_p[j]);}
             if(f[i].p[j]==4){
            fprintf(fdd ,"TIPUL PARAMETRULUI %s este char\n",f[i].nume_p[j]);}
             if(f[i].p[j]==5){
            fprintf(fdd ,"TIPUL PARAMETRULUI %s este string\n",f[i].nume_p[j]);}
           }
           fprintf(fd, "\n \n");
           
           }fclose(fdd);
           }
int yyerror(char * s){
printf("eroare: %s la linia:%d\n",s,yylineno);
}

int main(){
yyin=fopen("gresit.txt","r");
yyparse();
} 

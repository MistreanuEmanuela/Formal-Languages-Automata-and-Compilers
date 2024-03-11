
typedef struct variabila
{
 char nume[100];
 char tip[100];
 char caracter;
 int intreg;
 char scop[100];
 bool constanta;
 bool declarat;
 bool init;
 bool vector;
 float real; 
 bool bool_val;
 char sir[100];
} variabile;

typedef struct functii
{
 char nume[100];
 char tip_return[100];
 int nr_parametri;
 bool declarat;
 bool init;
  float real; 
 bool bool_val;
 char sir[100];
 int intreg;
 int p[10];
 char nume_p[10][10];
 } functie;
 
 typedef struct expresii
 {
  int tip; //1-int, 2-char...
  int intreg;
  bool bool_val;
  char caracter;
  float real;
  char* string;
  } expresii;

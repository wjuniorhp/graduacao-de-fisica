#include "stdlib.h"
#include "stdio.h"
#include "math.h"
#include "time.h"

#define IND(A, x, y, d) A[(x)*(d)+(y)]
double ** alocaMatriz(int nlinhas, int ncolunas);

void liberaMatriz(double **matriz, int nlinhas, int ncolunas);
void testaMM(double **a, double **b, double **c, int x, int y, int z);
void imprime(double **V, int x, int y);


void MMT0(double **a, double **b, double **c, int x, int y, int z);

void MMbloco(double **a, double **b, double **c, int x, int y, int z);
void MMbloco0(double **a, double **b, double **c, int x, int y, int z);

double difMat(double **a, double **b, int x, int y);
int main() {
  clock_t inicio;

  int x,y,z;
  int i,j,k;
   double **a, **b, **c;
   double **a2;
   int n=4096;

  x = n;
  y=n;
  z=n;
//  x = 3;
//  z = 2;
//  y = 3;
   b = alocaMatriz(x,z);
   c = alocaMatriz(z,y);
   a = alocaMatriz(x,y);
   a2 = alocaMatriz(x,y);
   for(i=0;i<x;i++)
       for(k=0;k<z;k++) b[i][k]=i+k;
   for(j=0;j<y;j++)
       for(k=0;k<z;k++) c[k][j]=j+k;
/*
   b[0][0]=2;
   b[0][1]=3;
   b[1][0]=4;
   b[1][1]=1;
   b[2][0]=3;
   b[2][1]=2;

   c[0][0]=4;
   c[0][1]=1;
   c[0][2]=2;
   c[1][0]=2;
   c[1][1]=3;
   c[1][2]=2;
*/
  inicio = clock();
  testaMM(a,b,c, x, y, z);
  clock_t fim = clock();
  double t1 = (double)(fim-inicio)/CLOCKS_PER_SEC;

  printf("tempo %lf s\n", t1);

 // imprime(b, x, z);
 // imprime(c, z, y);
 // imprime(a, x, y);
  inicio = clock();
  MMT0(a2, b, c, x, y, z);
  fim = clock();
  double t2 = (double)(fim-inicio)/CLOCKS_PER_SEC;
  printf("tempo2 %lf s\n", t2);
  printf("diferenca %lf\n", difMat(a, a2, x, y));
  //imprime(a, x, y);
   for(i=0;i<x;i++)
       for(j=0;j<y;j++) a2[i][j]=0;
  inicio = clock();
  MMbloco(a2, b, c, x, y, z);
  fim = clock();
  double t3 = (double)(fim-inicio)/CLOCKS_PER_SEC;
  printf("tempo3 %lf s\n", t3);
  printf("diferenca %lf\n", difMat(a, a2, x, y));
   for(i=0;i<x;i++)
       for(j=0;j<y;j++) a2[i][j]=0;
  inicio = clock();
  MMbloco0(a2, b, c, x, y, z);
  fim = clock();
  double t4 = (double)(fim-inicio)/CLOCKS_PER_SEC;
  printf("tempo4 %lf s\n", t4);
  printf("diferenca %lf\n", difMat(a, a2, x, y));
  return 0;

}

// imprime a matriz V na tela
void imprime(double **V, int x, int y) {
    int i,j;
    for(i=0; i<x; i++) {
        for(j=0; j<y; j++) {
            printf("%5.2lf ",V[i][j]);
        }
        printf("\n");
    }
        printf("\n\n");
}
#define MIN(a,b) (((a)<(b))?(a):(b))

void MMbloco(double **a, double **b, double **c, int x, int y, int z) {
   int i,j,k;
  double **cx;
  int j2, k2;
  int block_x=64;
  int block_y=64;
 /*  cx = alocaMatriz(y, z);
  for(j =0; j < y; j++)
      for(k=0; k < z; k++)
          cx[j][k] = c[k][j];
*/
for(j2 = 0; j2 < y; j2 += block_x)
    for(k2 = 0; k2 < z; k2 += block_y)
      for(i = 0; i < x; i++)
           for(j = j2; j < MIN(j2 + block_x, y); j++) {
              for(k=k2; k < MIN(k2 + block_y, z); k++) {
                  //a[i][j] += b[i][k]*cx[j][k];
                  a[i][j] += b[i][k]*c[k][j];
              }
            }

  //liberaMatriz(cx, y, z);

}


void MMbloco0(double **a, double **b, double **c, int x, int y, int z) {
   int i,j,k;
  double **cx;
  int j2, k2;
  int block_x=128;
  int block_y=128;
   cx = alocaMatriz(y, z);

  for(j =0; j < y; j++)
      for(k=0; k < z; k++)
          cx[j][k] = c[k][j];

for(j2 = 0; j2 < y; j2 += block_x)
    for(k2 = 0; k2 < z; k2 += block_y)
      for(i = 0; i < x; i++)
           for(j = j2; j < MIN(j2 + block_x, y); j++) {
              for(k=k2; k < MIN(k2 + block_y, z); k++) {
                  a[i][j] += b[i][k]*cx[j][k];
                  //a[i][j] += b[i][k]*c[k][j];
              }
            }

  liberaMatriz(cx, y, z);

}


void MMT0(double **a, double **b, double **c, int x, int y, int z) {
   int i,j,k;
  double **cx;
   cx = alocaMatriz(y, z);
  for(j =0; j < y; j++)
      for(k=0; k < z; k++)
          cx[j][k] = c[k][j];
  for(i=0;i<x;i++)
      for(j=0;j<y;j++) {
          a[i][j] =0.0;
          for(k=0;k<z;k++) {
              a[i][j] += b[i][k]*cx[j][k];
          }
      }
  liberaMatriz(cx, y, z);

}




void testaMM(double **a, double **b, double **c, int x, int y, int z) {
   int i,j,k;
  for(i=0;i<x;i++)
       for(j=0;j<y;j++) {
           a[i][j]=0.0;
           for(k=0;k<z;k++) {
               a[i][j]+=b[i][k]*c[k][j];
           }
       }

}

double difMat(double **a, double **b, int x, int y) {
  int i,j;
  double df = 0.0;
  for(i=0;i<x;i++)
      for(j=0;j<y;j++) df += fabs(a[i][j]-b[i][j]);
  return df;
}

void liberaMatriz(double **matriz, int nlinhas, int ncolunas){
    int i;
    for(i=0; i<nlinhas;i++) {
        free(matriz[i]);
    }
    free(matriz);
}


double ** alocaMatriz(int nlinhas, int ncolunas) {
    double **matriz;
    int i;
    // aloca as linhas
    matriz = (double **) malloc(sizeof(double)*nlinhas);
    // sempre teste se a alocacao foi realizada com sucesso
    if( !matriz) {
        fprintf(stderr,"Erro ao alocar linhas da matriz\n");
        exit(1);
    }
    for(i=0; i<nlinhas;i++) {
        matriz[i] = (double *) malloc(sizeof(double)*ncolunas);
        // sempre teste se a alocacao foi realizada com sucesso
        if( !matriz[i]) {
            fprintf(stderr,"Erro ao alocar colunas da matriz\n");
            exit(1);

        }
    }
    return matriz;
}





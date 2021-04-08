#include "stdio.h"
#include "stdlib.h"
#include "math.h"


/* Cabecalho das funcoes */
void inicia(double **V, double **Vn1);
void imprime(double **V);
double atualiza(double **V, double **Vn1);
int calcula(double **V, double **Vn1, double eps, int itMax);
void imprimeGrafico(double **V, FILE *fp);
void imprimeCampo (double **Ex, double **Ey, FILE *fp);
void calculaE(double **V, double **Ex, double **Ey);
void imprimeNaTela(double **V);

double ** alocaMatriz(int nlinhas, int ncolunas);

// variavei globais
double Xmax=1;
double Xmin=-1;

double dx,dy;

// largura do quadrado interno
double larguraInterna;

int iinicial, jinicial;
double xinicial, yinicial;
int largura;
int N;
main() {

    double deltaV;
    double **V;
    double **Vn1;
    double **Ex;
    double **Ey;
    FILE *vfp, *efp;

    int convergiu;

    larguraInterna = 0.6;
    dx = 0.1;
    dy = 0.1;
    N = 1 + (Xmax - Xmin) / dx;
    dy = dx;
    xinicial = - larguraInterna/2.0;
    yinicial = larguraInterna/2.0;

    // o termo +0.5 serve para evitar erros de arredondamentos
    iinicial = (xinicial + Xmax)/dx +0.5;
    jinicial = (Xmax - yinicial)/dx +0.5;
    largura = larguraInterna / dx + 0.5;

    V = alocaMatriz(N, N);
    Vn1 = alocaMatriz(N, N);
    Ex = alocaMatriz(N, N);
    Ey = alocaMatriz(N, N);
    // inicializa as matrizes com as condicoes iniciais
    inicia(V, Vn1);

    convergiu = calcula(V, Vn1, 1.0e-4, 1000);
    if(convergiu ==1 ) {
        calculaE(V, Ex, Ey);
        vfp = fopen("Vxy.dat","w");
        imprimeGrafico(V, vfp);
        fclose(vfp);
        efp = fopen("Exy.dat","w");
        imprimeCampo(Ex, Ey, efp);
        fclose(efp);
        imprimeNaTela(V);
    } else {
        printf("Nao houve convergencia\n");
    }

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

/*
   Calcular o valor de V
   Parametros de entrada:
   V - matriz com a solucao inicial
   Vn1 - matriz calculada
   eps - criterio de convergencia
   itMax - numero maximo de iteracoes
   Parametros de retorno
   A funcao retornara 1 se houver convergencia e zero se nao houver convergencia
   Se houve convergencia as matrizes V e Vn1 serao a solucao
   */
int calcula(double **V, double **Vn1, double eps, int itMax) {
    int i;
    double deltaV;
    int convergiu = 0;
    for(i=0; i< itMax; i++) {
        deltaV = atualiza(V, Vn1);
        deltaV = atualiza(Vn1, V);
        if(deltaV < eps) {
            convergiu=1;
            break;
        }
        printf("i=%d deltaV = %lf\n",i, deltaV);
    }
    printf("i=%d deltaV = %lf\n",i, deltaV);
    return convergiu;
}

// calcula a matriz Vn1 e retorna deltaV
//
double atualiza(double **V, double **Vn1) {
    double deltaV = 0;
    int i,j;

    // calcula o retangulo superior
    for(i=1; i<iinicial; i++) {
        for(j=1; j<N-1; j++) {
            Vn1[i][j] = 0.25*( V[i+1][j] + V[i-1][j] +
                    V[i][j+1] + V[i][j-1] );
            deltaV += fabs(V[i][j] - Vn1[i][j]);
        }
    }

   // calcula o retangulo da esquerda
     for(i=iinicial; i<=iinicial+largura; i++) {
        for(j=1; j<jinicial; j++) {
            Vn1[i][j] = 0.25*( V[i+1][j] + V[i-1][j] +
                    V[i][j+1] + V[i][j-1] );
            deltaV += fabs(V[i][j] - Vn1[i][j]);
        }
    }

   // calcula o retangulo da direita
     for(i=iinicial; i<=iinicial+largura; i++) {
        for(j=jinicial+largura; j<N-1; j++) {
            Vn1[i][j] = 0.25*( V[i+1][j] + V[i-1][j] +
                    V[i][j+1] + V[i][j-1] );
            deltaV += fabs(V[i][j] - Vn1[i][j]);
        }
    }

    // calcula o retangulo inferior
    for(i=iinicial+largura+1; i<N-1; i++) {
        for(j=1; j<N-1; j++) {
            Vn1[i][j] = 0.25*( V[i+1][j] + V[i-1][j] +
                    V[i][j+1] + V[i][j-1] );
            deltaV += fabs(V[i][j] - Vn1[i][j]);
        }
    }

    return deltaV;
}

// recebe a matriz V e um ponteiro para um arquivo
// e imprime a matriz nesse arquivo

void imprimeGrafico(double **V, FILE *fp) {
    int i,j;
    double x, y;
    double dx,dy;

    x = Xmin;
    y = Xmax;

    dx = (Xmax - Xmin) / (double) (N-1);
    dy = dx;
    for(i=0; i<N; i++) {
        x = -1;
        for(j=0; j<N; j++) {
            fprintf(fp, "%5.2lf %5.2lf %5.2lf\n",x, y,V[i][j]);
            x += dx;
        }
        fprintf(fp,"\n");
        y -= dy;
    }
}

// imprime a matriz V na tela
void imprime(double **V) {
    int i,j;
    for(i=0; i<N; i++) {
        for(j=0; j<N; j++) {
            printf("%5.2lf ",V[i][j]);
        }
        printf("\n\n");
    }
}

// inicializa as matrizes V e Vn1 com as condicoes de contorno
void inicia(double **V, double **Vn1) {

    int i,j;


    // Vamos inicilizar todos os elementos de matriz com zero
    for(i=0; i<N; i++) {
        for(j=0; j<N; j++) {
            V[i][j]=Vn1[i][j]=0.0;
        }
    }
    for(i=iinicial; i<= iinicial + largura; i++) {
        for(j=jinicial; j<= jinicial + largura; j++) {
            V[i][j]=Vn1[i][j]=1.0;
        }
    }
}

void imprimeCampo (double **Ex, double **Ey, FILE *fp) {
    int i,j;
    double x,y;
    y=Xmax;
    for(i=0;i<N;i++) {
        x=Xmin;
        for(j=0;j<N;j++) {
            fprintf(fp,"%6.2lf %6.2lf %6.2lf %6.2lf\n",x, y, Ey[i][j], -Ex[i][j]);
            x+=dx;
        }
        y -=dx;
    }

}
void calculaE(double **V, double **Ex, double **Ey) {
    int i,j;
    double V1, V2;
    for(i=0;i<N;i++) {
        for(j=0;j<N;j++) {
            Ex[i][j]=Ey[i][j]=0.0;
        }
    }
    // nao vamos nos preocupar com as bordas por enquanto
    for(i=1;i<N-1;i++) {
        for(j=1;j<N-1;j++) {
            // calcula Ex
            V1 = V[i+1][j];
            V2=V[i-1][j];
            Ex[i][j] = - 0.5*(V1 - V2)/dx;
            // calcula Ey
            V1 = V[i][j+1];
            V2=V[i][j-1];
            Ey[i][j] = - 0.5*(V1 - V2)/dx;
        }
    }
}

void imprimeNaTela(double **V) {
    int i,j;
    printf("\n");
    printf("\n");
    for(i=0;i<N;i++) {
        for(j=0;j<N;j++) {
            printf("%5.2lf ",V[i][j]);
        }
        printf("\n");
        printf("\n");
    }
}

#include "stdio.h"
#include "stdlib.h"
#include "math.h"

#define N 301

/* Cabecalho das funcoes */
void inicia(double V[N][N], double Vn1[N][N]);
void imprime(double V[N][N]);
double atualiza(double V[N][N], double Vn1[N][N]);
int calcula(double V[N][N], double Vn1[N][N], double eps, int itMax);
void imprimeGrafico(double V[N][N], FILE *fp);
void imprimeCampo (double Ex[N][N], double Ey[N][N], FILE *fp);
void calculaE(double V[N][N], double Ex[N][N], double Ey[N][N]);
void imprimeNaTela(double V[N][N]);

// variavei globais
double Xmax=1;
double Xmin=-1;

double dx,dy;
main() {


double V[N][N];
double Vn1[N][N];
double Ex[N][N];
double Ey[N][N];
FILE *vfp, *efp;

int convergiu;

dx = (Xmax - Xmin) / (double) (N-1);
dy = dx;
// inicializa as matrizes com as condicoes iniciais
inicia(V, Vn1);

convergiu = calcula(V, Vn1, 1.0e-4, 10000);
if(convergiu ==1 ) {
  calculaE(V, Ex, Ey);
  vfp = fopen("Vxy.dat","w");
  imprimeGrafico(V, vfp);
  fclose(vfp);
  efp = fopen("Exy.dat","w");
  imprimeCampo(Ex, Ey, efp);
  fclose(efp);
//  imprimeNaTela(V);
//  printf("Ex : \n");
//  imprimeNaTela(Ex);
//  printf("Ey : \n");
//  imprimeNaTela(Ey);
printf("N %d\n", N);

} else {
	printf("Nao houve convergencia\n");
}

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
int calcula(double V[N][N], double Vn1[N][N], double eps, int itMax) {
	int i;
	double deltaV;
	int convergiu = 0;
	for(i=0; i< itMax; i++) {
		deltaV = atualiza(V, Vn1);
		deltaV = atualiza(V, Vn1);
		if(deltaV < eps) {
			convergiu=1;
			break;
		}
	//	printf("i=%d deltaV = %lf\n",i, deltaV); 
	}
	printf("i=%d deltaV = %lf\n",i, deltaV); 
	return convergiu; 
}

// calcula a matriz Vn1 e retorna deltaV
//
double atualiza(double V[N][N], double Vn1[N][N]) {
	double deltaV = 0;
	int i,j;
double temp;

	// calcula inicialmente a parte interior da matriz
	for(i=1; i<N-1; i++) {
		for(j=1; j<N-1; j++) {
                        temp = V[i][j];
			V[i][j] = 0.25*( V[i+1][j] + V[i-1][j] +
					V[i][j+1] + V[i][j-1] );
			deltaV = fabs(V[i][j] - temp); 
		}
	}

	// calcula os elementos da borda superior
	for(j=1; j<N-1; j++) {
                temp = V[0][j];
		V[0][j] = 1.0/3.0*( V[1][j] +
				V[0][j+1] + V[0][j-1] );
		deltaV += fabs(V[0][j] - temp); 
	}

	// calcula os elementos da borda inferior
	for(j=1; j<N-1; j++) {
                temp = V[N-1][j];
		V[N-1][j] = 1.0/3.0*( V[N-2][j] +
				V[N-1][j+1] + V[N-1][j-1] );
		deltaV += fabs(V[N-1][j] - temp); 
	}
	return deltaV;
}

// recebe a matriz V e um ponteiro para um arquivo
// e imprime a matriz nesse arquivo

void imprimeGrafico(double V[N][N], FILE *fp) {
	int i,j;
	double x, y;
	double dx,dy;

	x = Xmin;
	y = Xmax;

	dx = (Xmax - Xmin) / (double) (N-1);
	dy = dx;
	for(i=0; i<N; i++) {
		x = Xmin;
		for(j=0; j<N; j++) {
			fprintf(fp, "%5.2lf %5.2lf %5.2lf\n",x, y,V[i][j]);      
			x += dx; 
		}
		fprintf(fp,"\n");
		y -= dy; 
	}
}

// imprime a matriz V na tela
void imprime(double V[N][N]) {
	int i,j;
	for(i=0; i<N; i++) {
		for(j=0; j<N; j++) {
			printf("%5.2lf ",V[i][j]);      
		}
		printf("\n\n");
	}
}

// inicializa as matrizes V e Vn1 com as condicoes de contorno
void inicia(double V[N][N], double Vn1[N][N]) {

	int i,j;
	for(i=0; i<N; i++) {
		for(j=0; j<N; j++) {
			V[i][j]=Vn1[i][j]=0.0;      
		}
	}

	// atribuir o valor 1 na parede esquerda
	for(i=0; i<N; i++) {
		V[i][0]=Vn1[i][0]=1.0;      
	}
	// atribuir o valor -1 na parede direita
	for(i=0; i<N; i++) {
		V[i][N-1]=Vn1[i][N-1]=-1.0;      
	}
}

void imprimeCampo (double Ex[N][N], double Ey[N][N], FILE *fp) {
	int i,j;
	double x,y;
	y=Xmax;
	for(i=0;i<N;i++) {
		x=Xmin;
		for(j=0;j<N;j++) {
			fprintf(fp,"%6.2lf %6.2lf %6.2lf %6.2lf\n",x, y, Ey[i][j], Ex[i][j]);
			x+=dx;
		}
		y -=dx;
	}

}
void calculaE(double V[N][N], double Ex[N][N], double Ey[N][N]) {
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

void imprimeNaTela(double V[N][N]) {
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

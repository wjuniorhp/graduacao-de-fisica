#include "stdlib.h"
#include "stdio.h"
#include "math.h"
double *alocaVetor(int n);
double phi(double r);
double rho(double r);
double numerov(int i, double Vn1, double Vn2);

void calculaPrimeiro(double primeiro, double *segundo);
void fdt(double Y[2], double tempo, double F[2]);
void rk4(double Y[2], double tempo, double dt);

int npassos=100000;
double h = 0.0001;
void itera(int npassos, double *V);
main() {
    int i,j,erro;
    double inicial, primeiro;
    double *V, *V1, *V2;
    double Y[2];
    V = alocaVetor(npassos);
    V1 = alocaVetor(npassos);
    V2 = alocaVetor(npassos);
    inicial = -1.0;
    for(i=0;i<npassos;i++) {
        V[i]=V1[i]=V2[i]=0.0;
    }

   // V[1] = primeiro;

    // primeiro com V[1] analitico
    V[0]=inicial;
    V[1] = phi(h);
    itera(npassos, V);

    // agora com V[1] calculado
    V1[0]=inicial;
    calculaPrimeiro(inicial, &primeiro);
    V1[1] = primeiro;
    itera(npassos, V1);

    // agora V[1] com % do valor analitico
    V2[0]=inicial;
    V2[1] = phi(h)*0.95;
    itera(npassos, V2);

    double ana;
    for(i=0;i<npassos;i++) {
        ana = phi(i*h);
        printf("%lf %lf %lf %lf %f\n", i*h, V[i], ana-V[i], ana-V1[i], ana-V2[i]  );
    }
        system ("pause");
}

void itera(int npassos, double *V) {
   int i;
    for(i=2;i<npassos;i++) {
        V[i] = numerov(i, V[i-1], V[i-2]);
    }

}

void calculaPrimeiro(double primeiro, double *segundo) {
    double tempo, Y[2], dt;
    int i;
    tempo=0.0;
    dt = h/1000.0;
    Y[0]=primeiro;
    for(i=0;i<1000;i++) {
        tempo = i*dt;
        rk4(Y, tempo, dt);

    }
    *segundo = Y[0];
}

void fdt(double Y[2], double tempo, double F[2]) {
    F[0]=Y[1];
    F[1]=rho(Y[0]);

}

void rk4(double Y[2], double tempo, double dt) {

    double YH[2], Y1DT[2], Y2DT[2], Y3DT[2], Y4DT[2];
    int i;

    // calcula k1
    fdt(Y, tempo, Y1DT);

    // calcula k2
    for(i=0;i<2;i++) {
        YH[i] = Y[i] + 0.5*dt*Y1DT[i];
    }
    fdt(YH, tempo + dt / 2., Y2DT);

    //calcula k3
    for(i=0;i<2;i++) {
        YH[i] = Y[i] + 0.5*dt*Y2DT[i];
    }
    fdt(YH, tempo + dt / 2., Y3DT);

    //calcula k4
    for(i=0;i<2;i++) {
        YH[i] = Y[i] + 0.5*dt*Y3DT[i];
    }
    fdt(YH, tempo + dt / 2., Y4DT);

    for(i=0;i<2;i++) {
        Y[i] = Y[i] + (Y1DT[i] + 2.0*Y2DT[i] + 2.0*Y3DT[i] + Y4DT[i])*dt/6.0;
    }
}

double phi(double x) {
    return -exp(-x)-x;
}

double rho(double r) {
    return  -exp(-r);

}

double numerov(int i, double Vn1, double Vn2) {
    double V;
    double h2;
    double r;
    double t1;
    double n, nm1, np1;
    n = h*i;
    nm1 = h*(i-1);
    np1 = h*(i+1);
    h2 = h*h;
    V = (2.0*Vn1 - Vn2 + (h2/12.0)*(rho(np1) + 10.0*rho(n) + rho(nm1)));
    return V;

}

double *alocaVetor(int n) {
	double *vetor;

	vetor = malloc(sizeof(double)*n);
	if(!vetor) {
		fprintf(stderr,"Erro ao alocar vetor com %d elementos\n",n);
		exit(1);
	}
	return vetor;
}

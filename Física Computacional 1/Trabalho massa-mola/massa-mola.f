*Esse programa tem como objetivo obter os valores de x para cada t em um
*sistema massa-mola, onde:
*        n --> n£mero de subintervalos adotados
*        a --> tempo inicial adotado
*        b --> tempo final adotado
*        x(n) --> vetor das posi‡äes
*        v(n) --> vetor das velocidades
*        k --> constante el stica da mola em questÆo
*        m --> massa do objeto que esta semovendo com a mola

      program sistema_massa_mola
         integer n, i
         real dt, k, m, c, a, b, t
         parameter(n=10000, a=0., b=10., k=1., m=1.)
         real x(n+1), v(n+1)
         
         dt = (b-a)/n
         c = k/m

*Condi‡Æo Inicial:
         x(1)=2
         v(1)=0
*Condi‡Æo de contorno:
         x(2) = x(1) + v(1)*dt
         v(2) = v(1) - c*x(1)*dt

*Calculando...
         do i=2, n
            x(i+1) = x(i-1) + 2*v(i)*dt
            v(i+1) = v(i-1) - 2*c*x(i)*dt
         enddo
*Abrindo o arquivo e impprmindo os resultados:
         open(10, file='tempo e posicao MHS.dat')
100   format(f10.6, 2x, f10.6)
         do i=1, n+1
            t = a + (i-1)*dt
            write(10,100) t, x(i)
         enddo
      end

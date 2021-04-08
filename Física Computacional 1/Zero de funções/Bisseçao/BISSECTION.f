* Programa para determinar ra¡zes da equa‡Æo
* atrav‚s do m‚todo de bisse‡Æo


      PROGRAM BISECTION
      IMPLICIT NONE
      INTEGER i
      REAL A,B,DL,DX,X0,X1,F,FX
21    FORMAT(I4,2F16.8)

      DL = 1.0E-06
      A  = 1.0
      B  = 2.0
      DX = B - A
      i = 0

      DO WHILE (ABS(DX).GT.DL)
         X0 = (A+B)/2.0
           IF ((F(A)*F(X0)).LT.0) THEN
              B  = X0
              DX = B-A
           ELSE
              A  = X0
              DX = B-A
           END IF
      i = i+1
      END DO

      WRITE (6,21) I,X0,DX ! Escrevendo na tela

      
      END PROGRAM BISECTION
      FUNCTION F(X)
      IMPLICIT NONE
      REAL F
      REAL X
      F = EXP(X)*ALOG(X)-X*X
      END FUNCTION F


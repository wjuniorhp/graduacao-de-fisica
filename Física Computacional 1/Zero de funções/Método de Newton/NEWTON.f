* Programa para determinar ra¡zes da equa‡Æo
* atrav‚s do m‚todo de Newton


      PROGRAM NEWTON
      IMPLICIT NONE
      INTEGER i
      REAL A,B,DL,DX,X0,X1,F,FX,DF,DFX
21    FORMAT  (I4,2F16.8)
      DL = 1.0E-06
      A  = 1.0
      B  = 2.0
      DX = B-A
      X0 = (A+B)/2.0
      i = 0

      DO WHILE (ABS(DX).GT.DL)
         X1 = X0-F(X0)/DF(X0)
         DX = X1-X0
         X0 = X1
        i = i+1
      END DO

      WRITE (6,21) i,X0,DX

      END PROGRAM NEWTON


      FUNCTION F(X)
      IMPLICIT NONE
      REAL F
      REAL X
      F = EXP(X)*ALOG(X)-X*X
      END FUNCTION F

      FUNCTION DF(X)
      IMPLICIT NONE
      REAL DF
      REAL X
      DF = EXP(X)*(ALOG(X)+1.0/X)-2.0*X
      END FUNCTION DF


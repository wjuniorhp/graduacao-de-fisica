* programa para resolver um sistema de equa‡äes lineares
* Atrav‚s do m‚todo de elimina‡Æo gaussiana
* Jos‚ Wellington mar‡o - 2010
      program kirchoff
      implicit none
      integer i, j, n
      parameter (n = 3) !nk ‚ a dimensÆo da matriz colocando-se as duas primeiras linhas no final
      real A(n,n), A1(n,n), A2(n,n), A3(n,n)
      real B, det
11    Format (1x,5(2x,f7.3))
12    Format (1x,f7.3)

      ! o Comando data vai ordenar os dados da seguinte forma

      ! data A/a(1,1),a(2,1), a(n, 1) ... a(1,2),a(2,2), a(n,2) ....,a(1,3),a(2,3),a(n,3) ....

      !data  AA/-1,-4,0,-1,1,1,1,0,4,0,8,16/        ! Essa ‚ a matriz aumentada, que ‚ usada no c lculo atrav‚s de elimina‡Æo gaussiana
      data  A/-1,-4,0,-1,1,1,1,0,4/                 ! essa   a matriz principal, usada no c lculo de determinantes
      data  A1/0,8,16,-1,1,1,1,0,4/
      data  A2/-1,-4,0,0,8,16,1,0,4/
      data  A3/-1,-4,0,-1,1,1,0,8,16/

      ! Matriz principal
      Print *, "Matriz principal = "
      do i = 1, n
         write(6,11) (A(i,j),j=1,n)
      enddo

      call linha(1)

      ! c lculo do determinante de A
      call calc_det(A,B,n)
      print *, "O determinante da matriz A = "
      write (6,12) B
      call linha(1)
      det=b

      !! Matriz aumentada
      !Print *, " Matriz aumentada = "
      !do i = 1, n
       !  write(6,11) (AA(i,j),j=1,n+1)
      !enddo
      !call linha(1)
      
      call calc_det(A1,B,n)
      print*, "i1 =", B/det
      
      call calc_det(A2,B,n)
      print*, "i2 =", B/det
      
      call calc_det(A3,B,n)
      print*, "i3 =", B/det

      end     ! Fim do programa principal


************************************************************************
************************************************************************
*********************Subrotina para calcular o determinante*************
************************************************************************
************************************************************************
      subroutine calc_det(A,det,n)
      IMPLICIT NONE
      INTEGER i, j, k, l
      integer n
      REAL A(n,n)
      REAL m, B, det
      LOGICAL existe

      l = 1
! Convertendo a matriz para uma triangular superior
      DO k = 1, n-1
         IF (A(k,k).eq.0.) then     ! Se algum termo da diagonal principal for zero, entÆo nÆo se define o determinante
            existe = .FALSE.
               DO i = k+1, n
                  IF (A(i,k).ne.0.) then
                     DO j = 1, n
                        B      = A(i,j)
                        A(i,j) = A(k,j)
                        A(k,j) = B
                     END DO
                        existe = .TRUE.
                        l= -1
                        EXIT
                  ENDIF
               END DO

               IF (existe.EQV..FALSE.) THEN
                   det = 0.0
                 return
               END IF
         ENDIF

       ! usando os piv“s

        DO j = k+1, n
           m = A(j,k)/A(k,k)
             DO i = k+1, n
                A(j,i) = A(j,i) - m*A(k,i)
             ENDDO
        END DO

      END DO

      ! uma vez que a matriz ‚ reduzida … triangular superior, entÆo o determinante ‚ o
      !produto dos elementos da diagonal principal

      det = 1
      DO i = 1, n
         det = det*A(i,i)
      ENDDO

      return
      END subroutine

************************************************************************
************************************************************************
*********************Subrotina para deixar uma linha em branco**********
************************************************************************
************************************************************************
      subroutine linha(li)
      implicit none
      integer li

      if(li.eq.1) then
        print *, " "
      else if(li.eq.2) then
        print *, " "
        print *, " "
      endif
      return
      end subroutine linha
************************************************************************
************************************************************************

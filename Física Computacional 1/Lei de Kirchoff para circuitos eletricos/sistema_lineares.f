* programa para resolver um sistema de equaá‰es lineares
* AtravÇs do mÇtodo de eliminaá∆o gaussiana
* Cl†udio Silva maráo - 2010
      program sistema
      implicit none
      integer i, j, n
      parameter (n = 3) !nk Ç a dimens∆o da matriz colocando-se as duas primeiras linhas no final
      real AA(n,n+1), A(n,n)
      real sol(n), B
      integer erro
11    Format (1x,5(2x,f7.3))
12    Format (1x,f7.3)
      
      ! o Comando data vai ordenar os dados da seguinte forma
      
      ! data A/a(1,1),a(2,1), a(n, 1) ... a(1,2),a(2,2), a(n,2) ....,a(1,3),a(2,3),a(n,3) ....

      data  AA/1,4,-1,10,-2,1,-12,-20,5,120,60,10/        ! Essa Ç a matriz aumentada, que Ç usada no c†lculo atravÇs de eliminaá∆o gaussiana
      data  A/1,4,-1,10,-2,1,-12,-20,5/                 ! essa † a matriz principal, usada no c†lculo de determinantes

      erro = 0
      
      
      ! Matriz principal
      Print *, "Matriz principal = "
      do i = 1, n
         write(6,11) (A(i,j),j=1,n)
      enddo
      
      call linha(1)
      
      ! c†lculo do determinante de A
      call calc_det(A,B,n)
      print *, "O determinante da matriz A = "
      write (6,12) B
      call linha(1)

      ! Matriz aumentada
      Print *, " Matriz aumentada = "
      do i = 1, n
         write(6,11) (AA(i,j),j=1,n+1)
      enddo
      call linha(1)

      call gaussiana(AA,sol,n,erro)
      print *, "A solucao do sistema linear"
      do i = 1, n
         write(6,11) sol(i)
      enddo

      end     ! Fim do programa principal


************************************************************************
************************************************************************
*********************Subrotina com o mÇtodo de eliminaá∆o gaussiana*****
************************************************************************
      subroutine gaussiana(a, x, n, erro)
      implicit none
      integer n   !dimens∆o para armazenar o n£mero de vari†veis desconhecidas
      integer erro
      real a(n,n+1) ! Matriz "aumentada"
      real x(n)
      real xi     ! matriz que armazena as soluá‰es
      integer i, j, k ! contadores para os loops
      integer l
      real temp, m, soma
      logical existe
      
      m = 0
        existe = .FALSE.
             xi = 0
	       DO k = 1, n-1
	          DO j = 1, n
		     IF (a(j, 1).ne.0.0 ) existe = .TRUE.
		  END DO
		     IF (existe.EQV..FALSE.) THEN
		     	PRINT *,"Sem solucao unica"
		           erro = -1
		     	   xi = 0
		     	EXIT
		     ELSE
		     l = k

		     DO j = k, n
		       IF (ABS(a(j, k)).lt.ABS(a(l,k))) l = j
                     END DO

                     DO j = 1, n + 1
		        temp   = a(k, j)
			a(k,j) = a(l,j)
			a(l,j)=temp
		     END DO
		     ENDIF

		DO j = k+1, n
		     m = a(j,k)/a(k,k)
		     DO i = k+1, n+1
		        a(j,i) = a(j,i) - m*a(k,i)
		     END DO
		END DO

		IF (a(n,n).eq.0.) THEN
		
		    PRINT*,"Sem solucao unica"
		       erro = -1
		       xi = 0
		EXIT
		ELSE
		x(n) = a(n,n+1)/a(n,n)
        	DO i = n-1, 1, -1
		   soma = 0
	 	      DO j = i+1, n
		 	 soma = soma + a(i,j)*x(j)
		      ENDDO
			 x(i) = (a(i,n+1) - soma)/a(i,i)
		      END DO
		ENDIF
	END DO

      END SUBROUTINE
      
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
         IF (A(k,k).eq.0.) then     ! Se algum termo da diagonal principal for zero, ent∆o n∆o se define o determinante
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

       ! usando os pivìs

        DO j = k+1, n
           m = A(j,k)/A(k,k)
             DO i = k+1, n
                A(j,i) = A(j,i) - m*A(k,i)
             ENDDO
        END DO

      END DO

      ! uma vez que a matriz Ç reduzida Ö triangular superior, ent∆o o determinante Ç o
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






      
      

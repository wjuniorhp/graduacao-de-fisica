*  Programa usado para fazer c lculo com matrizes
* Disciplina: F¡sica COmputcional I
*  Cl udio Mois‚s - Mar‡o de 2010

      program matrizes
      implicit none
      integer i, j, nx, ny, nk, ii
      parameter (nx = 3, ny = 3, nk = 3)                  ! essa ‚ a ordem da matriz, caso vocˆ queira visualizar com
      real A(nx,ny), B(nx,ny), C(nx,ny), D(nx,ny) ! ordens diferentes ‚ necess rio modificar o "format"
      real SO, SU                                 ! POR EXEMPLO NX=NY="8" IMPLICA EM   format(1x,"8"(2x,f6.1))
      character lixo*4
11    FORMAT(1X,3(2X,F6.1))

! Criando matrizes baseadas nos ¡ndices i e j.
     
      open(21,file ='matrizes-2.dat',status='unknown')
      open(22,file ='matriz A.dat',status='old')
      open(23,file ='matriz B.dat',status='old')

      do i=1, nx
         read(22,*)
         read(22,*)(lixo(ii), ii=0, 2),(A(i,j), j=1, ny)
         read(23,*)(B(i,j), j=1, ny)
      enddo
      
      do i = 1, nx
         do j = 1, ny
            ! Determinando a soma e a subtra‡Æo entre as matrizes (definida por fun‡äes, mas pode ser feito atrav‚s de subrotinas)
              C(i,j) = SO(A(i,j),B(i,j))
              D(i,j) = SU(A(i,j),B(i,j))

         enddo
      enddo
      


! Escrevendo as matrizes na tela (fique a vontade para escrever em arquivos textos)
      
      write(21,*) "Matriz A"
      do i = 1, nx
         WRITE(21,11) (A(i,j), j=1, ny)
      enddo
      

      write(21,*) "Matriz B"
      do i = 1, nx
         WRITE(21,11) (B(i,j), j=1, ny)
      enddo


      write(21,*) "Matriz C = A + B"
      do i = 1, nx
         WRITE(21,11) (C(i,j), j=1, ny)
      enddo

      write(21,*) "Matriz D = A - B"
      do i = 1, nx
         WRITE(21,11) (D(i,j), j=1, ny)
      enddo


      CALL TRANS(A,B,NX,NY) ! CHAMO A SUBROUTINA QUE CALCULA A MATRIZ TRANSPOSTA
      write(21,*) "Matriz TRANSPOSTA DE A"
      do i = 1, nx
         WRITE(21,11) (B(i,j), j=1, ny)
      enddo



      CALL produto(A,B,C,NX,NY, nk) ! CHAMO A SUBROUTINA QUE CALCULA O PRODUTO ENTRE A MATRIZ A E B
      write(21,*) "PRODUTO ENTRE A E B"
      do i = 1, nx
         WRITE(21,11) (C(i,j), j=1, ny)
      enddo


      STOP
      END PROGRAM MATRIZES     ! FIM DO PROGRAMA PRINCIPAL
      

************************************************************************
************* FUN€åES E SUBROTINAS USADAS NO PROGRAMA*******************
************************************************************************
! Fun‡Æo para somar

      FUNCTION SO(X,Y)
      IMPLICIT NONE
      REAL X, Y, SO
      SO = X + Y
      END FUNCTION SO
      
      
! Fun‡Æo para subtrair

      FUNCTION SU(X,Y)
      IMPLICIT NONE
      REAL X, Y, SU
      SU = X - Y
      END FUNCTION SU
      

! Subrotina pr  deixar linhas em branco

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


! SUBROTINA QUE CALCULA A MATRIZ TRANSPOSTA

      SUBROUTINE trans(A,B,ni,nj)
      IMPLICIT NONE
      INTEGER i, j, ni, nj
      REAL A(ni,nj), B(nj,ni)

      do i = 1, ni
         do j = 1, nj
            B(j,i) = A(i,j)
         enddo
      enddo
      return
      end SUBROUTINE trans
      
!  Subrotina que calcula o produto entre duas matrizes
! em FORTRAN 90 TEMOS A OP€ÇO "MATMUL"  C(ni,nj) = matmul(A(ni,nj),B(ni,nj))

      SUBROUTINE produto(A,B,C,ni,nj,nk)
      IMPLICIT NONE
      INTEGER I,J,k, NI,NJ,NK
      REAL A(NI,NJ), B(NI,NJ), C(NI,NJ)

      do i = 1, ni
         do j = 1, nj
            C(i,j) = 0.0
               do k = 1, nk
                  C(i,j) = C(i,j) + A(i,k)*B(k,j)
               enddo
         enddo
      enddo
      return
      end subroutine

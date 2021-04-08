! Programa usado para resolver a difus∆o bidimensional de calor atravez de diferenáa finita.
! Guilherme Monteiro Maio de 2012
!Difus∆o bidimensional(quadrado):
!n=tempo, i = eixo x, j = eixo y;
!T(n+1, i, j) = T(n,i,j) + ((k*dt/((2dx)**2))*(T(n,i+2,j)+T(n,i-2,j)+T(n,i,j+2,)+T(n,i,j-2)))



      program difusaodecalor
      implicit none
      integer n,i,j,nx,nt, ny
      real dx,dt,k, m
      real*8 T(10, 20, 20)
      
      !Parametros:
      nt = 10
      nx = 20
      ny = 20
      dx = 2E-2
      k = 1.1E-4
      !dx em centimetros.
      dt = 1.0
      !Fim dos Parametros.
      
      open(24, file='tempo1.dat')
      !Tarando a matriz:
      do n=1, nt
        do i=1, nx
          do j=1, ny

          T(1,i,j) = 28

          enddo
        enddo
      enddo
      !Condiá∆o inicial:
      do i=1, nx
        do j=1, ny
          if (((i==10) .OR. (i==11)) .and. ((j==10) .OR.(j==11))) then
          T(1,i,j) = 30
          endif
        enddo
      enddo
      !Fim da condiá∆o inicial.
      do i=1, nx
        write (24,'(50(f7.2))') (T(1,i,j), j=1,ny)
        enddo

      !Inicio do C†lculo:
      
      !C†lculando a constante:
      m = (k*dt/((2*dx)**2))
      PRINT *, M
      
      !Fim do c†lculo da constante.
      
      do n=1, nt
        do i=1, nx
          do j=1, ny
         T(n+1, i, j) = T(n,i,j) + (m*(T(n,i+2,j)
     &+T(n,i-2,j)+T(n,i,j+2)+T(n,i,j-2)))
         enddo
        enddo
      enddo
      !Fim do Calculo.
      
      !Abrindo arquivos:
      open(25, file='calor.dat')
      
      !Escrevendo:
      do n = 1, nt
        do i=1, nx
          write (25,'(50(f7.2))') (T(n,i,j), j=1, ny)
        enddo
        
        write (25,*)
      enddo
      
      
      end program
      
      

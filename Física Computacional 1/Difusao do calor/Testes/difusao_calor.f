! Programa para determinar a difusao termica em uma placa de metal
! cujo coeficiente de difusividade v = 1.1e-4
! Claudio Silva ---> Maio/2012

! RESUMO DO PROBLEMA

! t1 e a temperatura da placa
! colocamos uma fonte de calor, que pode ser de uma unica fase (temperatura t2), ou seja ci = 1
! ou colocamos uma fonte de calor de duas fases (t2 e t3), ou seja ci = 2
! Aplicamos a equacao de difusao -->> dT/dt = D (d2T/dx2 + d2T/dy2)
! Usamos valores apropriados para a espacamento de grade dx = dy = 2e-2
! Tambem usamos um passo de tempo (time step) apropriado, dt = 2.
! OBS: para t > 3 o modelo apresenta problemas de instabilidade computacional
! O modelo e integrado para nt = 1000 intervalos de tempo, considerado que cada passo sao 2 segundos
! entao o tempo de integracao total e de 2000 segundos




      program difusao
      integer nx, ny, nt, ci
      parameter (nx = 30, ny = 30, nt = 1001, ci = 2)
      real dx, v, dt, m, t1, t2, t3
      parameter (dx = 2e-2, v = 1.1e-4, dt = 3.5, t1 = 0., t2 = 15.)
      parameter (m = (v*dt/((2*dx)**2)), t3 = 2*t2)
      real T(nt,nx,ny)

      open(25, file = 'difusao.out')
      write(6,*) m, dx, dt

!Diferentes condicoes iniciais

! ci = 1  --> Fonte de calor no meio da placa
      if(ci==1) then
       do i = 1, nx
         do j = 1, ny
	   if(i.gt.10.and.i.le.20.and.j.gt.10.and.j.le.20) then
           T(1,i,j) = t3
	   else
	   T(1,i,j) = t1
	   endif
	enddo
      enddo

!ci = 2   ---> Fonte de calor em duas "fases", t2 e t3
      elseif(ci==2) then
       do i = 1, nx
         do j = 1, ny
	   if(i.gt.8.and.i.le.22.and.j.gt.8.and.j.le.22) then
	      T(1,i,j) = t2
	     if(i.gt.12.and.i.le.18.and.j.gt.12.and.j.le.18) then
	      T(1,i,j) = t3
	     endif
	   else
	   T(1,i,j) = t1
	   endif
	enddo
       enddo
      endif

! Fim do estabelecimento das condicoes iniciais


! Inicio da integracao da equacao de difusao

      do n = 1, nt-1               ! laco do tempo, uso nt-1, pois pela condicao de contorno o ultimo
	                           ! vai se repetir no primeiro

	do i = 2, nx-1            !laco da direcao i

	   do j = 2, ny-1         !laco da direcao j

	     T(n+1,i,j) = T(n,i,j) + m*(T(n,i+1,j)+T(n,i-1,j)+T(n,i,j+1)
     &        +T(n,i,j-1)-4*T(n,i,j))

	   enddo                  !fecha o laco da direcao i

	enddo                     !fecha o laco da direcao j


!*******************************
! Fronteiras laterais  rigidas   ! fronteiras sao aplicadas para cada tempo
!*******************************
	 do j = 1, ny
	   T(n+1,1,j)  = T(n,1,1)
	   T(n+1,nx,j) = T(n,1,1)
	 enddo

	 do i = 1, nx
	   T(n+1,i,1)  = T(n,1,1)
	   T(n+1,i,ny) = T(n,1,1)
	 enddo
!*******************************
!*******************************


      enddo                     !fecha o laco do tempo


! escrevendo a saida para um arquivo texto
      do n = 1, nt
         write(25, *)
	 write(25, *) "Tempo = ", n
	 write(25, *)
        do i = 1, nx
	  write (25,'(30(f5.1))') (T(n,i,j), j=1, ny)
	enddo
      enddo

      end program



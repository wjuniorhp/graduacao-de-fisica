! Programa usado para resolver a equa‡Æo de advec‡Æo unidimensional
! Cl udio Silva setembro de 2010

      program advec
      implicit none
      integer nx, nt, n, i, j, k
      parameter (nx=30, nt=100)
      real u(nt,nx)
      real dt, dx, c, t, cfl, undef
      parameter (dt=10.,dx=20000., c=200., undef = -9999.)


! Criando uma condi‡Æo inicial
      j = nx/2
      
      do i = 1, nx
         if(i.gt.10.and.i.le.j) then
           k = i-j
           u(1,i) = 5+(1.- 2*k**2)/(exp(1.*k**2))
         elseif(i.gt.j.and.i.le.20) then
           k = i-j
           u(1,i) = 5+(1.- 2*k**2)/(exp(1.*k**2))
         else
           u(1,i) = 5
         endif
      enddo

      ! Verificando o crit‚rio de instabilidade num‚rica
      call cflc(cfl,c,dt,dx,undef)
      ! Esquema centrado
      call centrado(u,cfl,nt, nx,undef)
      ! Escrevendo em um arquivo texto
      call escreve_dat(u,nx,nt)
      end program

! Fim do programa principal
! Subrotina que determina o crit‚rio de instabilidade computacional "CFL"

      subroutine cflc(cfl,c,dt,dx,undef)
      implicit none
      real cfl, c, dt, dx, undef
      cfl = c*dt/dx ! crit‚rio de instabilidade CFL
      if(cfl.le.1) then
       cfl = cfl
      else
       cfl = undef
      endif
      end subroutine cflc

! Subrotina que resolve a equa‡Æo de advec‡Æo centrada no espa‡o
      subroutine centrado(u,cfl,nt,nx,undef)
      implicit none
      integer i, n, nt, nx
      real u(nt,nx), cfl, undef

      if(cfl.lt.1.) then
      do n = 2, nt
        do i = 1, nx-1
           u(n,i) = u(n-1,i) - (cfl/2)*(u(n-1,i+1)-u(n-1,i-1))
           
      ! Aplicando uma forma simples de fronteira peri¢dica
           if(i.eq.1)    u(n,1)  = u(1,i)
           if(i.eq.nx-1) u(n,nx) = u(1,i)

        enddo
      enddo

      elseif(cfl.eq.undef) then
      do n = 2, nt
        do i = 1, nx-1
          u(n,i) = undef
        enddo
      enddo
      write (6,*) "CFL nao atendido"
      endif
      end subroutine centrado

!  Subrotina que escreve os dados em um arquivo de texto
      subroutine escreve_dat(u,nx,nt)
      integer nx, nt
      real u(nt,nx)
      open(11, file = 'centrado.dat', status = 'unknown')
21    format (1x,i4,100(2x,f10.3))

      
      do i = 1, nx
         write(11,21) i, (u(n,i),n=1,nt)
      enddo
      
      end subroutine escreve_dat
      




      
!

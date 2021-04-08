      program funcao
      real :: dx=10, dt=1, c=100
      integer :: i, j, ni, nj
      parameter (ni=60, nj=60)
      real :: x(nj), u(ni,nj)

      open(25, file='resultados.dat', status='unknown')
101   format(1x,i4,60(2x,f10.3))
100   format(1x,i4,60(2x,d10.3))

      do i=1, ni                                                                      !i:para tempo
         do j=1, nj                                                                !j:para espa‡o
            x(j)=-c+j*dx
            u(i,j)=f(x(j))
            !write(6,*) x(j), i, u(i,j)
         enddo
      enddo

      do i=1, ni
           do j=1, nj
                u(i+1,j)=u(i,j)-c*(dt/dx)*(u(i,j+1)-u(i,j))
           enddo
      enddo
      
      write(25,101) (x(j), j=1, nj)
      do i=1, ni
           write(25,*)''
           write(25,100) i, (u(i,j), j=1, nj)
      enddo
      end program funcao
      
      function f(x)
      real :: f, x
      integer :: a=-1, b=2100
      if(x.gt.-70 .and. x.lt.70) then
                 f=a*x**2+b
      else
                 f=0
      endif
      end function

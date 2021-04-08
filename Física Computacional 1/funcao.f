      program funcao
      real :: dx=10, c=100
      integer :: i, j, ni=60, nj=100
      real :: x(100), u(60, 100)

      do i=1, ni                                                                      !i:para tempo
         do j=1, nj                                                                !j:para espa‡o
            x(j)=-c+j*dx
            u(i,j)=f(x(j))
            write(6,*) x(j), i, u(i,j)
         enddo
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

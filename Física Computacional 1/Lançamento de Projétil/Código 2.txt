      program movimento_parabolico
      integer :: x, ni=100, q=0
      real :: y(50)
      real :: theta=45., vo=100., g=9.8, a, b
      b=tan(2*(theta/180.)*3.14)
      a=-1.*g/(2*vo**2*(cos(theta))**2)                                   !y=axý+bx
      open(13, file='finais.dat')
       do x=1, ni
         y(x)=a*x**2+b*x
         write(13,*) y(x), x
         if(y.eq.0.)q=q+1
         if(q.eq.2) exit
       end do
      end program movimento_parabolico
      program metodo_de_newton
      real :: xk=1., dl=1.E-6, a=1., b=10., dx, xo
      real :: df, f
      integer :: i=0
       dx=b-a
       xo=(a+b)/2.
       do while(abs(dx).gt.dl)
        xk=xo-((f(xo))/(df(xo)))
        dx=xk-xo
        xo=xk
       enddo
       print *, xk

      end program metodo_de_newton

      function f(x)
       real x
       f=x**3-2*x**2
      end function f

      function df(x)
       real x
       df=3*x**2-4*x
      end function df

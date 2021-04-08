      program metodo_de_bissecao
       real :: a=1., b=2., delta=1e-6, dx, xo, f
100    format(a15, f16.3)
       if(f(a)*f(b).lt.0)then
        i=0
        dx=b-a
        do while (abs(dx).ge.delta)
         xo=(a+b)/2
         if(f(a)*f(xo).lt.0)then
          b=xo
          dx=b-a
         elseif(f(xo)*f(b).lt.0)then
          a=xo
          dx=b-a
         endif
         i=i+1
        enddo
        print 100, 'zero da fun‡Æo:', xo
       endif
      end program metodo_de_bissecao

      real function f(x)
       real x
       f=exp(x)*log(x)-x**2
      end function f


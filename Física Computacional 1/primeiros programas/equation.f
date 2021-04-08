       program equation
       real:: a=1, b=2, c=-3
       real x1, x2, delta
       
       delta = b**2-4*a*c
       if (delta.ge.0) then
       x1=(-1*b+sqrt(delta))/(2*a)
       x2=(-1*b-sqrt(delta))/(2*a)
       print*, "As raizes sao ",x1, " e",x2
100    format(a13,f5.2)
       print 100, "As raizes sao ",x1, " e ",x2
       else
       print*, "Nao possui solu‡oes reais"
       endif
       end program

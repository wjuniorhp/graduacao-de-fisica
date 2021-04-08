      program trapezio2
        real a, b, In, f, h, soma, cv, T, phi
        integer i, n
        parameter (a=1., b=3., n=4000, T=300.)                                         !n=4 subintervalos
        real x(n-1)

        h=(b-a)/n
        soma=0.
        do i=1, n-1
             x(i)=a+i*h
             soma = soma + f(x(i))
        enddo
        
        In=(h/2)*(f(a)+f(b)+2*soma)

        print*, In
        
        phi = b
        cv=9*((4*((T/phi)**3)*In)-((phi/T)/(exp(phi/T)-1)))
        print*, cv
      end

      function f(x)
        real x
        f= (x)/((300.**2)*(exp(x/300.)-1))
      end

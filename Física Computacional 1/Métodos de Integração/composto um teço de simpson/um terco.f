      program um_terco_simpson
        real a, b, In, f, h, soma
        integer i, n, nn
        parameter (a=1., b=3., nn=10000)
        real x, c

        do n=1, nn
          h=(b-a)/n                                                          !h ‚ o comprimento de intervalos
          soma=0.
          do i=1, n-1
               if (mod(i,2).eq.0) then
                c=2
               else if (mod(i,2).eq.1) then
                  c=4
               endif
               x=a+i*h
               soma = soma + c*f(x)
          enddo

          In=(h/3)*(f(a)+f(b)+soma)

        !Imprmindo:
          open(10, file='solucoes numericas.dat')
          write(10,*) n, In
        enddo
        end

      function f(x)
        real x
        f=1/x
      end

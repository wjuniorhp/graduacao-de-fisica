      program met_integr
         real h, a, b, trap, simpson
         integer n!, nmax
         parameter (a = -1e5, b = 1e5, n = 10**6 + 1)
         
            h = (b-a)/(n-1)
            
            print*, h
            write(6,*) 'Regra dp Trapezio:       ', trap (h, a, b, n)
            write(6,*) 'Regra do 1/3 de simpson: ', simpson (h, a, b, n)
            write(6,*) 'Somatorio:               ', somatorio (h, a, b,
     &       n)

            print*, "PRONTO!"
         
      end

************************************************************************

      real function somatorio (h, a, b, n)
         real a, b, h, soma, f, xi
         integer n, i
         
         soma = 0.0
         
         do i=0, n
              xi = a + i*h
              soma = soma + f(xi)*h
         enddo

         somatorio = soma

      end

************************************************************************
************************************************************************

      real function trap (h, a, b, n)
         real a, b, h, soma, f, xi
         integer n, i

         soma = 0.0

         do i=1, n-1
              xi = a + i*h
              soma = soma + f(xi)
         enddo

         trap = h/2 * (f(a) + f(b)) + h * soma

      end

************************************************************************
************************************************************************

      real function simpson (h, a, b, n)
         real a, b, h, soma, f, w, xi
         integer n, i

         soma = 0.0

         do i=1, n-1
              xi = a + i*h
              if (mod(i,2).eq.0) then
                  w = 2.
              else
                  w = 4.
              endif
              soma = soma + w*f(xi)
         enddo

         simpson = h/3 * (f(a) + f(b) + soma)

      end

************************************************************************
      
      real function f (z)
         real x, z, pi, mi0, i
         
         pi = 3.1415
         mi0 = 4*pi*1e-7
         i = 10.
         x = 2e-2
         
         f = (mi0*i)/(4.*pi) * x/((x**2+z**2)**(3./2))
      end

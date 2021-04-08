      program questao_3
         real h, a, b, trap, simpson
         integer n, i!, nmax
         parameter ( n = 10**6 + 1)
         
         do i=1, 4
            call selec (a, b, i)
         
            h = (b-a)/(n-1)

            write(6,*) 'Opcao ', i
            write(6,*) ' '
            print*, 'h = ',h
            write(6,*) 'Regra dp Trapezio:       ', trap (h,a,b,n,i)
            write(6,*) 'Regra do 1/3 de simpson: ', simpson (h,a,b,n,i)
            write(6,*) ' '
     
         enddo
            print*, "PRONTO!"

      end

************************************************************************
************************************************************************

      real function trap (h, a, b, n, opc)
         real a, b, h, soma, f, xi
         integer n, i, opc

         soma = 0.0

         do i=1, n-1
              xi = a + i*h
              soma = soma + f(xi,opc)
         enddo

         trap = h/2 * (f(a,opc) + f(b,opc)) + h * soma

      end

************************************************************************
************************************************************************

      real function simpson (h, a, b, n, opc)
         real a, b, h, soma, f, w, xi
         integer n, i, opc

         soma = 0.0

         do i=1, n-1
              xi = a + i*h
              if (mod(i,2).eq.0) then
                  w = 2.
              else
                  w = 4.
              endif
              soma = soma + w*f(xi,opc)
         enddo

         simpson = h/3 * (f(a,opc) + f(b,opc) + soma)

      end
      
************************************************************************

      subroutine selec(a, b, opc)
         real a, b
         integer opc

         if (opc.eq.1) then
              a = 0
              b = 1
         elseif (opc.eq.2) then
              a = 0
              b = 1
         elseif (opc.eq.3) then
              a = 1
              b = 5
         elseif (opc.eq.4) then
              a = -1e5
              b = 1e5
         endif

      end

************************************************************************

      real function f (x, opc)
         real x
         integer opc

         if (opc.eq.1) then
              f = 4/(1+x**2)
         elseif (opc.eq.2) then
              f = exp(-x**2)
         elseif (opc.eq.3) then
              f = alog(x)
         elseif (opc.eq.4) then
              f = exp(-x**2/2)
         endif

      end

      program numpi
         real h, a, b, trap, simpson, mc
         integer n!, nmax
         parameter (n = 10**6 + 1)
         
         !pi = 2*acos(0.0)
         a = 0.0
         b = 1.0

         h = (b-a)/(n-1)

         write(6,*) 'Valor de pi segundo:'
         write(6,*) 'Regra dp Trapezio:       ', 4*trap (h, a, b, n)
         write(6,*) 'Regra do 1/3 de simpson: ', 4*simpson (h, a, b, n)
         write(6,*) 'Monte Carlo:             ', 4* mc (a, b, n)

         print*, "PRONTO!"

      end

************************************************************************

      real function mc(a, b, n)
         integer i, seed, n
         real f, ranf, soma, x, a, b

         seed = 1
         call ranset(seed)

         soma = 0.0
         do i=1, n
              x = a + ranf()*(b-a)
              soma = soma + f(x)
         enddo

         mc = soma/n * (b-a)

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

      real function f (x)
         real r, x

         r = 1.0

         f = (r**2-x**2)**(1./2)
      end
      
************************************************************************
************************************************************************

      real function ranf()
           integer xor
           common irand(250),l
           l1=mod(l+146,250)+1
           iranf=xor(irand(l),irand(l1))
           ranf=float(iranf)*2.328306e-10+0.5
           irand(l)=iranf
           l=mod(l,250)+1
           return
      end

      subroutine ranset(isd)
           common irand(250),l
           isd=mod(isd,65536)
           !iseed=isd

           do 10 i=1,250
           do 20 j=1,5

   20      isd=mod(isd*8973+1,65536)-32768
           jrand=isd*65536
           j1=mod(i,2)
           do 30 k=1,5
   30      isd=mod(isd*8973+j1,65536)
           irand(i)=jrand+isd
   10      continue
           l=1

!           write(6,1) iseed
!    1      format(1x,'KSRG: Seed set to:',i12)
           return
      end

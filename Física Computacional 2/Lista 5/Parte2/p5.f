      program questao_5
          integer a, c, M, r, n, i, aleat
          real x1, x                          !onde x1 = r(n-1), o x anterior; e n ‚ o n£mero de numeros gerados
          parameter(a=9999, c=11, M=111223, n=10**5)
          r = 10
          x = real(r)/M

          open(15, file='dados3.dat')

          do i=1, n
               x1 = x
               r = aleat(a, c, M, r)
               x = real(r)/M

               write(15,*) real(i), x1, x
          enddo

      end

************************************************************************
      !essa subrotina tem como objetivo criar n numeros aleatorios pelo metodo de congruencia linear

      integer function aleat(a, c, M, r)
         integer a, c, M, r

         aleat = mod(a*r+c,M)

         return
      end

      program teste2
          integer nn, i, n
          parameter (nn=10**5)
          real  x(nn), dif, c, b

          open(15, file='dados3.dat')

          do i=1,nn
             read(15,*) c, b, x(i)
          enddo

          do i=2, 5
            n = 10**i
            if (i.ne.3) then
               print*, "n =", n,", k =", 1, " ==> dif =", dif(x,n, 1)
               print*, "n =", n,", k =", 3, " ==> dif =", dif(x,n, 3)
               print*, "n =", n,", k =", 7, " ==> dif =", dif(x,n, 7)
            endif
          enddo

      end

      real function dif(x,n,k)
          real soma, x(n)
          integer n, k, a
          
          a = 9999
          soma=0.
          do i=1,n
             soma = soma + x(i)**k
          enddo

          dif = abs(soma - real(a)/(k+1))/n

      end

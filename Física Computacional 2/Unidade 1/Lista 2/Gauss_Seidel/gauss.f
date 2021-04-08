      program gs_met
         integer n, meio, d
         real xmax, xmin, dx
         parameter(n=21, xmax=1, xmin=-1)
         real V(n,n)

         dx=(xmax-xmin)/(n-1)
         meio = n/2 + 1
         d = ifix(0.3 * (n-1)/2)

         print*, meio, d

         call iniciaV(V, n, meio, d)

         call calcula(V,n,meio,d)

         call imprime(V,n,dx,xmin)


      end



*************************************************************
      subroutine iniciaV(V, n, meio, d)
         integer i, j, d
         real V(n,n)

         do i=1, n
              do j=1, n
                   if ((i.ge.meio-d .and. i.le.meio+d) .and. (j.ge.
     &              meio-d .and. j.le.meio+d)) then
                          V(i,j) = 1

                   else
                           V(i,j) = 0

                   endif
              enddo
         enddo

      end
*********************************************************

*********************************************************
      subroutine atualiza(V,n,dV,meio,d)
         real V(n,n),temp
         real dV
         integer meio, d

         dV = 0.

         do i=2, n-1
              do j=2, n-1
                   if ((i.lt.meio-d .or. i.gt.meio+d) .or. (j.lt.
     &              meio-d .or. j.gt.meio+d)) then

                        temp = V(i,j)
                        V(i,j)=(V(i+1,j)+V(i-1,j)+V(i,j+1)+V(i,j-1))/4
                        dV = dV + abs(temp-V(i,j))

                   endif
              enddo
         enddo

      end
**************************************************************

**************************************************************
      subroutine calcula(V,n,meio,d)
         real V(n,n), tole, dV
         integer i,d

         tole=1e-15*(n*n)

         do i=1, 1000
              call atualiza(V,n,dV,meio,d)
              call atualiza(V,n,dV,meio,d)

              print *, i, dV

              if (dV.le.tole) then
                   go to 15
              endif
         enddo

15    continue

      end
**************************************************************

      subroutine imprime(V,n,dx,xmin)
         integer i, j
         real V(n,n),dx,xmin

         open(30, file='potenciais.dat')

100   format(21(f5.2,1x))

         write(30,100) ((i*dx)+xmin, i=0, n-1)
         do j=1, n
              write(30,100) (V(i,j), i=1, n)
         enddo
         write(6,100)

      end

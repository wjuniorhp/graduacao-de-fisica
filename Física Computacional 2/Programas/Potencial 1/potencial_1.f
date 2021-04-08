      program potencial_1
         integer n
         real xmax, xmin, dx
         parameter(n=7, xmax=10, xmin=-10)
         real V(n,n), Vn(n,n)
         
         dx=(xmax-xmin)/(n-1)

         call iniciaV(V,Vn,n)
         
         call lap_calc(V,Vn,n)
         
         call E_calc(V,n, dx)

      end

*************************************************************
      subroutine iniciaV(V,Vn,n)
         integer i, j
         real V(n,n), Vn(n,n)
         
         do i=2, n-1
              do j=1, n
                   V(i,j)=0.
              enddo
         enddo
         do j=1, n
              V(1,j) = -1
              V(n,j) = 1
         enddo
         
         !Passando os valores de  V para Vn (tal que Vn = V)
         do i=1, n
              do j=1, n
                   Vn(i,j)=V(i,j)
              enddo
         enddo
      end
*********************************************************
      
*********************************************************
      subroutine updateV(V,Vn,n,dV)
         real V(n,n), Vn(n,n)
         real dV
         dV = 0.
         
         do i=2, n-1
              do j=2, n-1
                   Vn(i,j)=(V(i+1,j)+V(i-1,j)+V(i,j+1)+V(i,j-1))/4
              enddo
         enddo
         
         do i=2, n-1
              Vn(i,1) = (V(i+1,1)+V(i-1,1)+V(i,2))/3
              Vn(i,n) = (V(i+1,n)+V(i-1,n)+V(i,n-1))/3
              dV = dV + abs(V(i,j)-Vn(i,j))
         enddo
      end
**************************************************************

**************************************************************
      subroutine lap_calc(V,Vn,n)
         real V(n,n), Vn(n,n), tole, dV
         integer i

         tole=1e-15*(n*n)
         
         do i=1, 1000
              call updateV(V,Vn,n,dV)
              call updateV(Vn,V,n,dV)

              if (dV.le.tole) then
                   go to 15
              endif
              
              print *, i, dV
         enddo
         
15    continue
      
      end
**************************************************************
      
      subroutine imprime(V,Ex,Ey,n)
         integer i, j
         real V(n,n), Ex(n,n), Ey(n,n)
         
         open(30, file='potenciais.dat')
         open(31, file='Ex.dat')
         open(32, file='Ey.dat')

100   format(7(f5.2,1x))

         do j=1, n
              write(30,100) (V(i,j), i=1, n)
              write(31,100) (Ex(i,j), i=1, n)
              write(32,100) (Ey(i,j), i=1, n)
         enddo
         write(6,100)

      end
      
**************************************************************
*                           Parte 2                          *
**************************************************************
      !Agora vamos calcular o campo eletrico
      subroutine E_calc (V,n, dx)
         real V(n,n)
         real Ex(n,n), Ey(n,n)
         integer i,j

         do i=1, n
             do j=1, n
                   Ex(i,j)=0
                   Ey(i,j)=0.
              enddo
         enddo

         
         do i=2, n-1
             do j=2, n-1
                   Ex(i,j)=-(V(i+1,j)-V(i-1,j))/(2*dx)
                   Ey(i,j)=-(V(i,j+1)-V(i,j-1))/(2*dx)
              enddo
         enddo

         call imprime(V,Ex,Ey,n)

      end

      program numerov_met
         integer i, n
         double precision h, inicial, primeiro
         parameter(n=200, inicial=0.0d0, h=1.0d-1)
         double precision V(n), V1(n), V2(n)
         double precision phi
         
         do i=1, n
              V(i) = phi(i*h)
              V1(i) = 0.0d0
              V2(i) = 0.0d0
         enddo
         
*Potencial V() analitico (da literatura)
         V(1) = inicial

*Ptecial V1() calculado numericamente a partir da literatura
         V1(1) = inicial
         V1(2) = phi(h)
         call itera(V1,n,h)
         
*Potencial V2() a partir de 95% do valor analitico
         V2(1) = inicial
         V2(2) = phi(h)*0.95d0
         call itera(V2,n,h)

100   format(d5.2, 2x, 3(d9.5, 2x))
         do i=1, n
              print 100, i*h, V(i), V(i)-V1(i), V(i)-V2(i)
         enddo
      end
      
************************************************************************
      
      subroutine itera(V,n,h)
         double precision V(n), h, numerov
         integer i, n
         
         do i=3, n
              V(i) = numerov(i,V(i-1),V(i-2),h)
         enddo
      end

************************************************************************
************************************************************************

      double precision function numerov (i,Vn1,Vn2,h)
         double precision h2, n, nm1, np1, h, Vn1, Vn2, S
         
         n = h*i
         nm1 = h*(i-1)
         np1 = h*(i+1)
         h2 = h*h
         numerov = (2.0d0*Vn1 - Vn2 + (h2/12.0d0)*(S(np1) +
     &             10.0d0*S(n) + S(nm1)))
         
      end
      
************************************************************************
************************************************************************

      double precision function phi(x)
         double precision x
         phi = 1.0d0 - 0.5d0 * (x+2)**(-x)
         return
      end
      
************************************************************************

      double precision function S(x)
         double precision x
         S = -0.5d0 * x * exp(-x)
         return
      end

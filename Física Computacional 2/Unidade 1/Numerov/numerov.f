      program numerov_met
         integer i, n
         double precision h, inicial, primeiro, ana
         parameter(n=100000, inicial=-1.0d0, h=1.0d-4)
         double precision V(n), V1(n), V2(n), y(2)
         double precision phi
         
         do i=1, n
              V(i) = 0.0d0
              V1(i) = 0.0d0
              V2(i) = 0.0d0
         enddo
         
*Potencial V() analitico (da literatura)
         V(1) = inicial
         V(2) = phi(h)
         call itera(V,n,h)

*Ptecial V1() calculado numericamente
         V1(1) = inicial
         call calculaPrimeiro(inicial,primeiro,h,y)
         V1(2) = primeiro
         call itera(V1,n,h)
         
*Potencial V2() com % do valor analitico
         V2(1) = inicial
         V2(2) = phi(h)*0.95d0
         call itera(V2,n,h)

         do i=1, n
              ana = phi(i*h)
              print *, i*h, V(i), ana-V(i), ana-V1(i), ana-V2(i)
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
         double precision h2, n, nm1, np1, h, Vn1, Vn2, rho
         
         n = h*i
         nm1 = h*(i-1)
         np1 = h*(i+1)
         h2 = h*h
         numerov = (2.0d0*Vn1 - Vn2 + (h2/12.0d0)*(rho(np1) +
     &             10.0d0*rho(n) + rho(nm1)))
         
      end
      
************************************************************************
************************************************************************

      subroutine calculaPrimeiro(primeiro,segundo,h,y)
         double precision tempo, y(2), dt, h, primeiro, segundo
         integer i
         
         dt = h/1000.0d0
         y(1) = primeiro
         
         do i=1, 1000
              tempo = i*dt
              call rk4(y,tempo, dt)
         enddo
         
         segundo = y(1)
      end

************************************************************************
************************************************************************
*Subrotina que calcula o resultado pelo metodo de runge-kutta de 4¦ ordem

      subroutine rk4(y,tempo,dt)
         double precision tempo, y(2), dt
         double precision YH(2), Y1DT(2), Y2DT(2), Y3DT(2), Y4DT(2)
         integer i

         !calculo de k1
         call fdt(y,tempo,Y1DT)
         
         !calculo do k2
         do i=1, 2
              YH(i) = Y(i) + 0.5d0*dt*Y1DT(i)
         enddo
         call fdt(YH, tempo + dt / 2.0d0, Y2DT)
         
         !calculo do k3
         do i=1, 2
              YH(i) = Y(i) + 0.5d0*dt*Y2DT(i)
         enddo
         call fdt(YH, tempo + dt / 2.0d0, Y3DT)
         
         !calculo do k4
         do i=1, 2
              YH(i) = Y(i) + 0.5d0*dt*Y3DT(i)
         enddo
         call fdt(YH, tempo + dt / 2.0d0, Y4DT)
         
         do i=1, 2
              Y(i) = Y(i) + (Y1DT(i) + 2.0d0*Y2DT(i) + 2.0d0*Y3DT(i) +
     &         Y4DT(i))*dt/6.0d0
         enddo
         
      end

************************************************************************
************************************************************************

      subroutine fdt (y, tempo, f)
         double precision y(2), f(2), tempo, rho
         
         f(1) = y(2)
         f(2) = rho(y(1))
      end

************************************************************************
      double precision function phi(x)
         double precision x
         phi = -exp(-x) - x
         return
      end
      
************************************************************************

      double precision function rho(x)
         double precision x
         rho = -exp(-x)
         return
      end

      program lista4
         real rho1, rho2, r1, r2, Ix, Iz
         integer n
         common /glob/ n, r1, r2
         common /rhos/ rho1, rho2

         rho1 = 8930.0
         rho2 = 19230.0
         r1 = 5e-2
         r2 = 1e-2
         n = 10**4

         print*, "Calculando.."
         call inercia(Ix,Iz)

         ! open(15, file='dados2.dat')

         print*, "Ix = ", Ix
         print*, "Iz = ", Iz

      end


************************************************************************

      subroutine inercia(Ix,Iz)
         integer i, seed, n
         real f, ranf, somax, somaz, x, y, z, a, b, Ix, Iz
         common /glob/ n, r1, r2

         a = -r1
         b = 2*r1           !b ‚ o intervalo de integra‡Æo, b = b-a = r1 - (-r1) = 2*r1

         seed = 1
         call ranset(seed)

         somax = 0.0
         somaz = 0.0
         do i=1, n
              x = a + ranf()*b
              y = a + ranf()*b
              z = a + ranf()*b
              somax = somax + f(x,y,z,1)
              somaz = somaz + f(x,y,z,2)
         enddo

         Ix = somax/n * b**3
         Iz = somaz/n * b**3

      end

************************************************************************
************************************************************************

      real function f(x,y,z,rot)
         integer rot
         real rho, r, x, y, z

         if (rot.eq.1) then
           f = rho(x,y,z)*(r(0.0,y,z))**2
         endif
         if (rot.eq.2) then
           f = rho(x,y,z)*(r(x,y,0.0))**2
         endif

      end

************************************************************************

************************************************************************

      real function rho(x,y,z)
         integer i
         real esf, cil, rho1, rho2
         common /glob/ n, r1, r2
         common /rhos/ rho1, rho2

         esf = x**2 + y**2 + z**2
         cil = x**2 + y**2

         do i=1, n
              if (cil.le.r2**2) then        !dentro do cilindro, dentro da esfera
                 rho = rho2
              elseif ((esf.le.r1**2).and.(cil.gt.r2**2)) then    !fora do cilindro, dentro da esfera
                 rho = rho1
              else
                 rho = 0.0
              endif
         enddo
         return

      end

************************************************************************
!esse r ‚ a distancia perpendicular ao eixo de rota‡ao

      real function r(x,y,z)
         real  x, y, z

         r = (x**2 + y**2 + z**2)**(0.5)

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

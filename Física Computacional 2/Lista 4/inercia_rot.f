      program inerciarot
         real rho1, rho2, r1, r2, m1, m2, pi, Ix, Iz
         integer n, n1, n2
         common /glob/ n, r1, r2
         common /ns/ n1, n2
         common /ms/ m1, m2

         rho1 = 8930.0
         rho2 = 19230.0
         r1 = 5e-2
         r2 = 1e-2
         n = 10**5
         pi = 2*acos(0.0)

         m1 = rho1*((4./3)*pi*r1**3 - pi*r2**2*2*r1)
         m2 = rho2*pi*r2**2*2*r1

         call defns(n1,n2)
         print*, n1, n2

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
         b = r1

         seed = 1
         call ranset(seed)

         somax = 0.0
         somaz = 0.0
         do i=1, n
              x = a + ranf()*(b-a)
              y = a + ranf()*(b-a)
              z = a + ranf()*(b-a)
              somax = somax + f(x,y,z,1)
              somaz = somaz + f(x,y,z,2)
         enddo

         Ix = somax
         Iz = somaz

      end

************************************************************************
************************************************************************

      real function f(x,y,z,rot)
         integer rot
         real dm, r, x, y, z, esf, cil, m1, m2
         common /glob/ n, r1, r2
         common /ms/ m1, m2
         common /ns/ n1, n2

         esf = x**2 + y**2 + z**2
         cil = x**2 + y**2

         if (cil.le.r2**2) then
             dm = m2/n2
         elseif ((esf.le.r1**2).and.(cil.gt.r2**2)) then
             dm = m1/n1
         else
             dm = 0.0
         endif

         if (rot.eq.1) then
           f = dm*(r(0.0,y,z))**2
         endif
         if (rot.eq.2) then
           f = dm*(r(x,y,0.0))**2
         endif

      end

************************************************************************

************************************************************************

      subroutine defns(n1, n2)
         common /glob/ n, r1, r2
         integer i, seed, n1, n2
         real ranf, a, b, esf, cil
         seed = 1
         call ranset(seed)

         a = -r1
         b = r1

         n1 = 0
         n2 = 0

         do i=1, n
              x = a + ranf()*(b-a)
              y = a + ranf()*(b-a)
              z = a + ranf()*(b-a)
              esf = x**2 + y**2 + z**2
              cil = x**2 + y**2
              if (cil.le.r2**2) then        !dentro do cilindro, dentro da esfera
                 n2 = n2 + 1
              endif
              if ((esf.le.r1**2).and.(cil.gt.r2**2)) then    !fora do cilindro, dentro da esfera
                 n1 = n1 + 1
              endif
         enddo
         return

      end

************************************************************************
!esse r ‚ a distancia perpendicular ao eixo de rota‡ao

      real function r(x,y,z)
         real  x, y, z

         !para a rota‡ao em torno do eixo z:
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

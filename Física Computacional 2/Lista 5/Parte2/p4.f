      program kirk_stoll
         real ranf, r1, r
         integer seed, n
         parameter (n = 10**5)
         seed = 1
         r = real(seed)

         open(15, file='dados2.dat')

         call ranset(seed)

         do i=1, n
               r1 = r
               r = ranf()

               write(15,*) i, r1, r
         enddo
      end

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
           iseed=isd

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

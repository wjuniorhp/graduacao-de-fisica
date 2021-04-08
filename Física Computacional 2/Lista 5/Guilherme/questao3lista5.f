      program questao3lista5
      
      integer a, r, m, c, i, y
      a=57
      c=1
      M=256
      r=10
      n=100000
      open(33, file='dadosquestao3lista5.dat')

        do i=1, n
           y = r
           r = aleat (a,c,m,r)
* Y será o valor antigo e R será o valor atual.          
           write (33,*) i, y, r
        enddo
      
      
      end program questao3lista5
      
      
      
      function aleat(a,c,m,r)
          integer a,c,m,r
          aleat = mod(a*r+c,m)
          return
      end function aleat

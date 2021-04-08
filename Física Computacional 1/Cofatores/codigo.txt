      program cofatores
      integer n, i, j, k, v
      parameter (n=3)
      real A(n,n), C(n-1,n-1)

      !data A/1,2,4,1,3,-2,1,1,-1,1,-1,3/
      open(15, file='matriz.dat', status='old')
      do i=1, n
           read(15,*)(A(i,j),j=1,n)
      enddo
      call escreva_matriz(A,n)

      do k=1, n
           do i=2,n
                v=1
                do j=1, n
                     if(j.ne.k)then
                          C(i-1,v)=A(i,j)
                          v=v+1
                     endif
                enddo
           enddo
           call escreva_matriz(C,n-1)
      enddo
      end

      subroutine escreva_matriz(M,n)
      integer n, i, j
      real M(n,n)

      do i=1, n
           write(6,*)(M(i,j), j=1, n)
      enddo
      print *,' '
      end subroutine
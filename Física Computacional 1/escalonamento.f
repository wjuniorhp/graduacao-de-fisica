      program escalonamento
      integer n, i, j, x
      parameter (n=3)
      real A(n,n+1)
      
      data A/1,2,4,1,3,-2,1,1,-1,1,-1,3/

      call escreva_matriz(A,n)
      
      k=1
      do while (A(n,n-1).ne.0)
           do i=k+1,n
                x=A(i,k)/A(k,k)
                do j=k, n+1
                   A(i,j)=A(i,j)-x*A(k,j)
                enddo
           enddo
           call escreva_matriz(A,n)
           k=k+1
           if (k.eq.n) exit
      enddo
      end
      
      subroutine escreva_matriz(A,n)
      integer n, i, j
      real A(n,n+1)

      do i=1, n
           write(6,*)(A(i,j), j=1, n+1)
      enddo
      print *,''
      end subroutine

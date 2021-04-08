      program irradiacao_do_corpo_negro_2
      real :: pi=3.14159265, h=6.626069E-34, c=29979458.
      double precision irradiacao(5,2000)
      real :: lambda(2000), kb=1.3806503E-23
      integer :: T, nt=5500, k=0,  ni=2000

      open(10, file='resulado2.dat')
        do T=3500, nt, 500
           j=0
           k=k+1
           do i=1, ni
               lambda(i)=real(i)*1E-3
               irradiacao(k,j)=c1/(lambda(i)**5*(exp(c2/(lambda(i)*T))
     &          -1))
               j=j+1
               write(10,*) T, lambda(i), irradiacao(k,j)
           enddo
        enddo
      end program irradiacao_do_corpo_negro

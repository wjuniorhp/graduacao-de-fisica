      program irradiacao_do_corpo_negro
      real :: c1=3.7427E8, c2=1.4388E4
      double precision irradiacao(5,2000)
      real :: ni=2000., lambda(2000)
      integer :: T, nt=5500, k=0
      
      open(10, file='resulado.dat')
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

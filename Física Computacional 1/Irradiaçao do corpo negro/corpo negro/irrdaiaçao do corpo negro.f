      program irradiacao_do_corpo_negro
      real :: c1=3.7427E8, c2=1.4388E4, pi=3.14159265, irradiacao(400)
      real :: ni=2000., nt=5500, lambda(2000)
      integer :: T
      
      open(10, file='resulado.dat')
        do T=3500, nt, 500
           j=0
           do i=1, ni
               lambda(i)=real(i)*1E-9
               irradiacao(j)=c1/(i**5*(exp(c2/(i*T)))-1)
               j=j+1
               write(10,*) lambda(i), irradiacao(j)
           enddo
        enddo
      end program irradiacao_do_corpo_negro

        program difusao
        implicit none

        integer, parameter :: nx=50, nt=50
        real, parameter :: alpha=0.0834,dt=0.023980,dx=0.2
        integer :: i,j,n
        real*8 :: T(nt,nx)=0.

        ! Condição inicial
        T(1,25)=1.

        do n=1,nt-1
           do i=2,nx-1
                 T(n+1,i)=T(n,i)+((alpha*dt)/dx/2.)*(T(n,i-1)-2.*T(n,i)+T(n,i+1))
           end do
      ! Condição de contorno
           T(n+1,2)=T(n+1,nx)
           T(n+1,1)=T(n+1,nx-1)
        end do

        open(30,file='teste2.out')
        do n=1,nt
        write(30,'(50F5.2)')(T(n,j),j=1,nx)
        end do

        stop
        end program difusao

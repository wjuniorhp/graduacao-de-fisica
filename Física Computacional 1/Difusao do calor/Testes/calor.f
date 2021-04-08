! Jos‚ Wellington Alves Rocha J£nior ->> maio de 2012

      program difusao_calor
      integer :: nx, nt
      real :: k, dt, dx
      integer :: i,j,n                        !n ‚ o tempo
      parameter(k = 1.1e-4, dt = 1. , dx = 2.0, nx=20, nt=10)
      real T(nt,nx,nx)
      real m
      !character*8 arquivo

! Condi‡Æo Inicial
      do n=1, nt
           do i=1, nx
                do j=1, nx
                     if((i.ge.(nx/2)-2).and.(i.le.(nx/2)+2).and.
     &                (j.ge.(nx/2)-2).and.(j.le.(nx/2)+2)) then
                          T(n,i,j)=45.e0
                     else
                         T(n,i,j)=28.e0
                     endif
                enddo
           enddo
      enddo



      m = (k*dt)/(2*dx)**2

      print *, m

      do n=1, nt
           do i=1, nx
                do j=1, nx
                     T(n+1,i,j)=T(n,i,j)+m*(T(n,i+2,j)-4*T(n,i,j)
     &                +T(n,i-2,j)+T(n,i,j+2)+T(n,i,j-2))
                enddo
                ! Boundary conditions

                T(n,i+1,2)=T(n,i+1,nx)
                T(n,i+1,1)=T(n,i+1,nx-1)
           enddo
      end do

      ! output files
      !do n=1, nt
       !       write(arquivo,*)'t_',n,'.dat'
        !      print *, arquivo
              open(11,file='t=1.dat')
              !write the output
              write(11,'(3x,20i6)') (j, j=1, nx)
              do i=1, nx
                   write(11,'(i2,1x,20(1xf5.2))') i, (T(1,i,j),j=1,nx)
              enddo
      !enddo


      end program difusao_calor

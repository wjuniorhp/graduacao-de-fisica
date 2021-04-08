! programa para resolver a equacao de adveccao atraves dos metodos de
! Euller e de Matsuno

! Jos‚ Wellington Alves Rocha J£nior ->> abril de 2012

      program advection
      integer :: nx=30, nt=50
      real :: c = 0.5, dt = 0.1,dx = 1.0
      integer :: i,j,n
      parameter(c = 0.5, dt = 0.1, dx = 1.0, nx=30, nt=50)
      real T1(nt,nx)
      real T2(nt,nx)
      real m1, m2

      do i=1, nt
           do j=1, nx
                T1(i,j)=0
                T2(i,j)=0
           enddo
      enddo

! Initial condition

      T1(1,15) = 2

      T2(1,15) = 2


      m1 = (c*dt)/(2*dx)

      m2 = ((c**2)*(dt**2))/((2*dx)**2)

      print *, m1, m2

      do n=1,nt-1

         do i=2,nx-1

         !Euller scheme

          T1(n+1,i)=T1(n,i)-m1*(T1(n,i+1)-T1(n,i-1))

         ! Matsuno scheme

          T2(n+1,i)=T2(n,i)-m1*(T2(n,i+1)-T2(n,i-1))+m2*(T2(n,i+2)-2*T2(n,i)+T2(n,i-2))

          end do

          ! Boundary conditions

          T1(n+1,2)=T1(n+1,nx)
          T1(n+1,1)=T1(n+1,nx-1)

          T2(n+1,2)=T2(n+1,nx)
          T2(n+1,1)=T2(n+1,nx-1)


      end do

      ! output files

      open(11,file='adv_euller.dat')
      open(22,file='adv_matsun.dat')

      !write the output
      do n = 1,nt

         write(11,'(i2,1x,30F5.2)') n, (T1(n,j),j=1,nx)

         write(22,'(i2,1x,30F5.2)') n, (T2(n,j),j=1,nx)

      enddo

      end program advection

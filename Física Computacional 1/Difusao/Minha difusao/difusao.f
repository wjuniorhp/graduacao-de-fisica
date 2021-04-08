      program difusao
      real :: dx=0.2, dt=0.023980, c=0.0834
      integer :: i, j, ni, nj
      parameter (ni=50, nj=50)
      real u(ni,nj)

      open(25, file='resultados.dat', status='unknown')
100   format(1x,i4,50(2x,f10.3))
101   format(5x,50(2x,i10))                                                   !i ‚ o tempo
                                                                              !j ‚ o espa‡o
      !Condi‡Æo Inicial:
      do j=1, nj
          if(j.eq.25) then
          u(1,j) = 1
          else
          u(1,j)=0
          endif
      enddo

      do i=1, ni-1
           do j=1, nj-1
                u(i+1,j)=u(i,j)+((c*dt)/(dx))*(u(i,j+1)-u(i,j))
                ! Condi‡Æo de contorno
                u(i,nj)=u(i,1)
           enddo
      enddo

      write(25,101) (j, j=1, nj)
      do i=1, ni
           write(25,*)' '
           write(25,100) i, (u(i,j), j=1, nj)
      enddo
      end program difusao
      

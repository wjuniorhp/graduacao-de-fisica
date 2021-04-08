      program alcance
      integer :: ni=7
      real :: R(7), theta(7)                                             !R ‚ o alcance
      real :: vo=100., g=9.8
      open(11, file='dados.dat', status='old')                           !abrindo o arquivo
      open(12, file='alcance.dat')
100   format(f3.0,2x,f5.1)
      do i=1, ni
        read(11,*) theta(i)
        R(i)=(vo**2/g)*sin(2*(theta(i)/180)*3.14)
        !print *, R(i), theta(i)
        write(12,100) theta(i), R(i)
      enddo
      end program alcance
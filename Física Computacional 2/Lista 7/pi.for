	program cpi

	double precision :: pi,w,sum,x 
	integer :: i,N=1000000000
	double precision inicio, fim
	pi = 0.d0 
	w = 1.d0/N 
	sum = 0.d0
        
	CALL CPU_TIME(inicio)
!$OMP PARALLEL PRIVATE(x) FIRSTPRIVATE(sum) 
!$OMP DO
	do i=1,n 
		x = w*(i-0.5d0) 
		sum = sum + 4.d0/(1.d0+x*x)
	enddo
!$OMP END DO 
!$OMP CRITICAL
	pi= pi + w*sum 
!$OMP END CRITICAL 
!$OMP END PARALLEL
	write(*,*) "pi = ", pi
	CALL CPU_TIME(fim)
	write(*,*) "Tempo ", fim-inicio
	stop
	end

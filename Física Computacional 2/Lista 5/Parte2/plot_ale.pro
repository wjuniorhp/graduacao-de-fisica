pro plot_ale
  openr, ird1, 'C:\Users\NetJunior\Dropbox\FC II\Lista 5\Parte2\dados1.dat', /get_lun
  openr, ird2, 'C:\Users\NetJunior\Dropbox\FC II\Lista 5\Parte2\dados2.dat', /get_lun
  openr, ird3, 'C:\Users\NetJunior\Dropbox\FC II\Lista 5\Parte2\dados3.dat', /get_lun
  
  n = 1e5
  A1 = findgen(3,n)
  A2 = A1
  A3 = A1
  
  readf, ird1, A1
  readf, ird2, A2
  readf, ird3, A3
  
  r1 = A1(1,*)
  r = A1(2,*) 
  
  u1 = A2(1,*)
  u = A2(2,*) 
  
  v1 = A3(1,*)
  v = A3(2,*) 
  
  plot, r1, r, psym=3
  
  window,1  
  plot, u1, u, psym=3
  
  window,2  
  plot, v1, v, psym=3
 
  free_lun, ird1, ird2, ird3
end
;kirk_stroll
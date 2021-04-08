pro plot_pot
  openr, itrap, 'C:\Users\NetJunior\Dropbox\FC II\Lista 3\Questao1\trap.dat', /get_lun
  openr, isimp, 'C:\Users\NetJunior\Dropbox\FC II\Lista 3\Questao1\simp.dat', /get_lun
  
  n = 1e4/2
  trape=findgen(n)
  simps=findgen(n)
  
  readf, itrap, trape 
  readf, isimp, simps
  
;  print, V
  
  plot, indgen(n), trape, yrange=[0,0.2], xrange=[n/2,n]
  oplot, indgen(n), simps, linestyle=1 
;  surface, V
 
  free_lun, itrap,isimp
end
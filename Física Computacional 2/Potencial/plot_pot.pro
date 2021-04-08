pro plot_pot
  openr, ipot, 'C:\Users\NetJunior\Dropbox\Fisica\FC II\Potencial\potenciais.dat', /get_lun
  openr, iex, 'C:\Users\NetJunior\Dropbox\Fisica\FC II\Potencial\Ex.dat', /get_lun
  openr, iey, 'C:\Users\NetJunior\Dropbox\Fisica\FC II\Potencial\Ey.dat', /get_lun
  
  readf, ipot, n, xmin, dx
;  readf, ipot, xmin
;  readf, ipot, dx
  
  V=findgen(n,n)
  Ex=findgen(n,n)
  Ey=findgen(n,n)
  
  readf, ipot, V
  readf, iex, Ex
  readf, iey, Ey
  
;  plot_field, Ex, Ey
  
;  window, 1
  x = xmin + findgen(n)*dx
  isurface, V,x,x 
 
  free_lun, ipot,iex,iey 
end
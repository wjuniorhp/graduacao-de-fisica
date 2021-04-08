pro boas_7_3_3
  tam = 1e2
  t = findgen(tam)
  pi = !PI
  
  f1 = sin(pi*t)
  f2 = sin(2*pi*t)
  f3 = sin(3*pi*t)/3.0
  
  f = f1 + f2 + f3
  
;  !p.multi = [0,2,2]
  plot, t, f1, $
    xtitle='t', ytitle='sin(pi*t)'
  plot, t, f2, $
    xtitle='t', ytitle='sin(2*pi*t)'
  plot, t, f3, $
    xtitle='t', ytitle='sin(3*pi*t)/3'
  plot, t, f, $
    xtitle='t', ytitle='sin(pi*t)+ sin(2*pi*t)!C+ sin(3*pi*t)/3'
end
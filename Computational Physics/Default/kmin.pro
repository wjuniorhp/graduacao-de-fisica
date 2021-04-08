pro kmin

c = 3d8
m = 9.109d-31
h_ = 1.055d-34
L = 1d-12
pi = double(!pi)

Eo = m*c^2
E = sqrt(((h_^2*pi*c)/(L))^2+Eo^2)
K = E - Eo

print, k

end
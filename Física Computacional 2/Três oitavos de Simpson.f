      program Tres_oitavos_de_Simpson
        integer j,c,nt
        real a,b,h,T,I,f,phi,soma
        parameter (phi=420.,T=300.)
        parameter (a=0.,b=phi,nt=1E4)
        !a e b s∆o os limites de integraá∆o.
        !420 Ç a temperatura de Debye para o alum°nio.
        h=(b-a)/nt
        soma=0.
        do j=1, nt-1
          if (mod(j,3).eq.0) then
            c=2
          else
            c=3
          endif
          x=a+j*h
          soma=soma+c*f(x)
        enddo
        I=(3*h/8)*(f(a)+f(b)+soma)
        resposta=9*((4*((T/phi)**3)*I)-((phi/T)/(exp(phi/T)-1)))
        write(6,*) resposta
        end

      function f(x)
        real x
        f= (x)/((T**2)*(exp(x/T)-1))
      end

      program trapezio1
        real :: a=1., b=3., I, f, h
      
        h=b-a
        
        I=(h/2)*(f(a)+f(b))
        
        print*, I
      end
      
      function f(x)
        real x
        f=1/x
      end

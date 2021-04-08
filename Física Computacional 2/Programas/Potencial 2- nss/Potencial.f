      program potential
         integer*4 x, y, i, j, n, nn, c
         parameter (x=100, y=100, nn=1e5)
         real dx, tole, dif, Vt
         parameter (dx=1e-6, tole=1e-2)
         real V(x,y)

         !"Chute" inicial, numeros aleatorios para come‡ar o problema
         do i=1, x
            do j=1, y
               V(i,j)=rand(0)*100
            enddo
         enddo

         !Calculando....
         do n=1, nn
            do i=2, x-1
               do j=2, y-1
                  Vt=(1./4)*(V(i+1,j)+V(i-1,j)+V(i,j+1)+V(i,j-1))
                  dif = abs(V(i,j)-Vt)
                  
                  if (dif.gt.tole) then
                       V(i,j)=Vt
              !    else if (dif.lt.tole) then
                 !      c = c + 1
                  endif

         !Condi‡Æo de contorno:
                  V(20,j)=10
                  V(60,j)=90

               enddo
            enddo
        !    if (c.eq.x*y) then
         !      go to 15
         !   endif
         enddo

!15     continue

         !Abrindo o arquivo de saida dos dados e criando o formato
         open(25, file='potencial.out')
100   format(100(f6.3, 2x))
         
         !Imprimindo
         do j=2, y-1
               write(25,100) (V(i,j), i=2, x-1)
         enddo
         
         print*, 'ok'
      end

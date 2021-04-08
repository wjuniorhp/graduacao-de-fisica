*utilizar "!" ou "*" para comentarios
*as linhas delimitam o espa‡o do programa. use "&" no final para quebra de linha" ou "+" no come‡o da pr¢xima
*Exemplos:
       program <nome2>               !programa prncipal
       end <nome2>
       
       subroutine <nome2>             !subroutina, programas secund rios
       end subroutine <nome2>
       
       function <nome3>               !‚ vista como uma variavel no pragrama principal
       end function <nome3>
       
*para declarar variaveis:
      integer
      real
      character
      
*Opera‡äes:
      + - * ** / sqrt exp log alog

*operadores l¢gicos:
      .gt. greater than
      .lt. less than
      .le. less or equal
      .eq. equal
      .ge. greater or equal
      .ne. not equal
      
*Comando de saida de dados:
      print fmt "mensagem"                      !fmt ‚ o formato. o comando PRINT ‚ exclusivo pra tela
      write(6, "mensagem")                      !o "6" do comando WRITE indica a impressao na tela
      write(id, file="nome.ext",...)Arquivo     !forma geral do WRITE
      
*Formatos:
      a(n) -> string               !n: caracteres
      (n)i(N) -> inteiro           !n: n§ de dados ; N: comprimento
      f -> real                    !
      x -> espa‡o
      
*Comando de entrada:
      !   file|formato
      !    |    |
      read(11, 100)t, R, N
100   format(i2,2x,f6.2,1x,a5)
!|
!rotulo/label

como compilar na mao, no prompt, neste mesmo diretorio:

$ flex tiny.l
$ gcc -o tiny *.c -lfl

(Em Mac pode ser que funcione com -ll no lugar de -lfl)

Executavel gerado -> tiny

cmake nao compila se o codigo gerado pelo flex estiver neste diretorio (o cmake
gera o codigo internamente no diretorio build)



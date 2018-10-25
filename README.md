[![Build Status](https://travis-ci.com/JessicaSousa/INF2471.P1.svg?branch=master)](https://travis-ci.com/JessicaSousa/INF2471.P1)
# INF2471.P1
 Implementação das questões da primeira prova da disciplina INF2471 
 
As função foram construídas em formato de pacote do R, o nome do pacote é "INF2471.P1", este foi desenvolvido utilizando o controle de versão do Github. Então a instalação dele pode ser realizado utilizando o seguinte comando no console do R:

``` r
# install.packages("devtools")
devtools::install_github("JessicaSousa/INF2471.P1")
```

Para ter acesso as questões implementas, basta executar a função `vignette("questoes")` no console do R, após a instalação do pacote `INF2471`. Esse *vignette*, está disponível no Github e pode ser acessado [aqui](https://jessicasousa.github.io/INF2471.P1/doc/questoes.html).

Dúvidas sobre as funções do pacote, experimente `help(nome_da_função)` ou `?nome_da_função`, exemplo:

``` r
#Carregar pacote
library(INF2471.P1)

#No console:
?apply_pseudo_inverse
help(apply_pseudo_inverse)
```

Aparecerá as instruções da seguinte forma: 

------------------------------------------------------
apply_pseudo_inverse {INF2471.P1}	R Documentation
## Function: Pseudo Inverse

#### Description

Given an m×n real matrix A, this function calculates the pseudo-inverse A+ of the matrix A.

#### Usage

apply_pseudo_inverse(A)
#### Arguments

A	
matrix of real numbers
#### Value

Returns a matrix A representing the pseudo-inverse of A.

#### Examples

A <- matrix(c(1, 0, 1,
              0, 1, 1), ncol = 2)

print(apply_pseudo_inverse(A))


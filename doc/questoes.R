## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
A <- matrix(c(8,2,0,0,
              2,8,2,0,
              0,2,8,2,
              0,0,2,8),nrow=4,ncol=4)

print(A)
cho <- INF2471.P1::cholesky(A)
print(cho)
print(cho%*%t(cho))

## ------------------------------------------------------------------------
print('R')

## ------------------------------------------------------------------------
v <- matrix(c(1, -1, 1), byrow = T, nrow = 1)
base <- INF2471.P1::find_orthogonal_complement(v)
print(base)
base_ortonormal <- INF2471.P1::apply_Gram_Schmidt(base)
print(base_ortonormal)

#Dois vetores são ortonormais entre si, se o produto interno deles 
#é igual a zero e se possuem norma 1.
#Produto Interno
print(INF2471.P1::dotprod(base_ortonormal[1, ], base_ortonormal[2, ]))
#Norma
print(INF2471.P1::magnitude(base_ortonormal[1, ]))
print(INF2471.P1::magnitude(base_ortonormal[2, ]))



## ----echo=TRUE-----------------------------------------------------------
library(rgl)
mfrow3d(nr = 1, nc = 1, sharedMouse = TRUE)
plot3d('')
arrow3d(c(0,0,0), c(1,-1,1), barblen=.2, lwd=3, col="green")
arrow3d(c(0,0,0), base_ortonormal[1, ], barblen=.2, lwd=3, col="blue")
arrow3d(c(0,0,0), base_ortonormal[2, ], barblen=.2, lwd=3, col="blue")
rglwidget()

## ------------------------------------------------------------------------
A <- matrix(c(1, 0, 1,
               0, 1, 1), ncol = 2)
print(A)
pseudoinverse <- INF2471.P1::apply_pseudo_inverse(A)
print(pseudoinverse)


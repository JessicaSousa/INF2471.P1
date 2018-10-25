#' Function: Row and column spaces
#'
#' Performs Gauss Elimination with Partial Pivoting of A and returns a list containing the row and column vectors of the given matrix
#'
#' @param A matrix of real numbers.
#'
#' @return Returns a list containing the matrix \code{A} reduced by rows and the rows and columns vectors.
#' @export
#' @examples
#' B <- matrix(sample.int(10, 12, replace = TRUE),  nrow = 3)
#' find_row_and_column_spaces(B)
#' A <- matrix(c(1,  1,
#'               1, -1,
#'               1,  0,
#'               1,  0), byrow = TRUE, ncol = 2)
#' find_row_and_column_spaces(A)
find_row_and_column_spaces <- function(A) {
  B <- A
  nvar <- ncol(A)
  nrows <- nrow(A)
  p <- 1
  js <- vector('integer', nvar)

  for(i in 1:nrows){
    for( j in 1:nvar){
      k <- which(A[ i:nrows, j] == max(A[i:nrows, j]))[1]
      k <- i + k - 1
      if(A[k, j] != 0 && k < nrows + 1){
        A[c(i, k), ] <- A[c(i, k), ]
        p <- j
        js[p] <- p
        break
      }
    }

    if(A[i, p] != 0 && i < nrows) {
      fator <- A[(i + 1), p] / A[i, p]
      fatores <- A[(i + 1):nrows, p] / A[i, p]
      A[(i + 1):nrows, ] <- A[(i + 1):nrows, ] - fatores %*% t(A[i, ])
    }
  }

  rowIds <- rowSums(A) != 0
  RowsB <- A[rowIds, ]
  ColsB <- A[ ,js != 0]

  list(A = A, RowsB = RowsB, ColsB = ColsB)
}

#' Function: Find Orthogonal Complement
#'
#' Compute the orthogonal complement of a set of vectors {v1, v2, ...}
#' @importFrom  matlib gaussianElimination
#' @importFrom  stats na.omit
#' @param A matrix of real numbers, where each line corresponds to each vector
#'
#' @return Returns a matrix \code{A} containing the orthogonal complement of the set of vectors.
#' @export
#' @examples
#' A <- matrix(c(1, 2, 3, 4,
#'               2, 5, 0, 1), byrow = TRUE, nrow = 2)
#' print(find_orthogonal_complement(A))
#' B <- matrix(c(3, 12, -1), byrow = TRUE, nrow = 1)
#' print(find_orthogonal_complement(B))
find_orthogonal_complement <- function(A){
  nEquations <- nrow(A)
  nVariables <- ncol(A)

  #proceder eliminação de Gauss
  if(nEquations > 1){
    A <- matlib::gaussianElimination(A)
    #Para cada linha, pegar a posição do primeiro elemento diferente de 0
    bound.indices <- apply(A, 1, function(row) {
      which(row != 0)[1]
    })
  }
  else{
    bound.indices <- which(A != 0)[1]
    A <- A * 1/A[ , bound.indices]
  }

  bound.indices <- stats::na.omit(bound.indices) #ignorar linhas nulas
  bound.length  <- length(bound.indices)
  free.length   <- nVariables - bound.length
  free.variables <- t(A[ ,-bound.indices])

  S <- matrix(0, nrow = free.length, ncol = nVariables)
  S[ ,bound.indices] <- -t(A[ ,-bound.indices]) #recebe variaveis livres

  if(free.length == 0)
    #Se o sistema não possuir variáveis livres, retorna vetor nulo
    return(A[1,] * 0)
  if(free.length > 1)
    diag(S[ , -bound.indices]) <- 1
  else
    S[ , -bound.indices] <- 1

  return(S)
}


#' Function: Gram-Schmidt
#'
#' This functions performs the orthonormalization the set of vectors using the Gram-Schmidt process.
#'
#' @param A matrix of real numbers, where the set of vectors is represented by each row of the matrix.
#'
#' @return Returns a matrix, where each row corresponds to the vector normalized by the Gram-Schimit process.
#' @export
#' @examples
#' A <- matrix(sample.int(4, 9, replace = TRUE),  nrow = 3)
#' print(A)
#' apply_Gram_Schmidt(A)
apply_Gram_Schmidt <- function(A){
  n <- nrow(A)
  A[1, ] <- unitvec(A[1, ])

  if(n >= 2){
    for(i in 2:n){
      e <- A[1:(i-1), ]
      if(i > 2) {
        A[i, ] <-  A[i, ] - colSums(dotprod(A[i, ], e) * e)
      }
      else{
        A[i, ] <-  A[i, ] - dotprod(A[i, ], e) * e
      }
      A[i, ] <- unitvec(A[i, ])
    }
  }
  return(A)
}


#' Function: Pseudo Inverse
#'
#' Given an m×n real matrix A, this function calculates the pseudo-inverse A+ of the matrix A.
#' @importFrom  matlib inv
#' @param A matrix of real numbers
#'
#' @return Returns a matrix \code{A} representing the pseudo-inverse of A.
#' @export
#' @examples
#' A <- matrix(c(1, 0, 1,
#'               0, 1, 1), ncol = 2)
#' print(apply_pseudo_inverse(A))
apply_pseudo_inverse <- function(A){
  matlib::inv((t(A) %*% A)) %*% t(A)
}

#' Function: Cholesky
#'
#' Given an m×n real matrix A, this function calculates the cholesky of matrix A.
#' @param A matrix of real numbers
#'
#' @return Returns a matrix \code{A} representing the cholesky decomposition
#' @export
#' @examples
#' A <- matrix(c(4.0,-1.0,0.0,0.0,-1.0,4.0,-1.0,0.0,0.0,-1.0,4.0,-1.0,0.0,0.0,-1.0,4.0),nrow=4,ncol=4)
#' print(cholesky(A))
cholesky <- function(A)
{
  L  = A
  L[upper.tri(L)] <- 0
  n = nrow(A)
  for(k in 1:(n-1)){

    L[k,k] = sqrt(L[k,k])
    for (i in (k+1):(n)){
      L[i,k] = L[i,k]/L[k,k]
    }
    for(j in (k+1):(n)){
      for (i in (j):(n)){
        L[i,j] = L[i,j]  - (L[i,k] * L[j,k])
      }
    }
  }
  L[n,n] = sqrt(L[n,n])
  return(L)
}

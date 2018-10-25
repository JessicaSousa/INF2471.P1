#' Vector Magnitude Function
#'
#' Find the vector magnitude (length)
#'
#' @param u vector of real numbers.
#'
#' @return Returns a magnitude (length) of \code{u}.
#' @export
#' @examples
#' u <- c(1,0,-3)
#' magnitude(u)
#' u <- sample(5, 4)
#' magnitude(u)
magnitude <- function(u){
  sqrt(sum(u ^ 2))
}

#' Vector Unit Function
#'
#' Find the unit vector.
#'
#' @param u vector of real numbers.
#'
#' @return Returns a unit vector of \code{u}.
#' @export
#' @examples
#' u <- c(1,0,-3)
#' unitvec(u)
#' u <- sample(5, 4)
#' unitvec(u)
unitvec <- function(u){
  mag <- magnitude(u)
  if(mag == 0){
    stop('The magnitude is equal to zero')
  }
  u / mag
}

#' Vector Dot Function
#'
#' Find vector dot product between two vectors.
#'
#' @param u vector of real numbers.
#' @param v vector of real numbers.
#'
#' @return Returns a dot product between \code{u} and \code{v}.
#' @export
#' @examples
#' u <- c(-1, -2, 3)
#' v <- c(4, 0, -8)
#' dotprod(u, v)
dotprod <- function(u, v){

  if(class(v) == 'matrix'){
    if(length(u) != ncol(v)){
      stop('The vectors must have the same size')
    }
    mult <- sweep(v, MARGIN=2, u,`*`)
    return(rowSums(mult))
  }
  else if(length(u) != length(v)){
    stop('The vectors must have the same size')
  }
  return (sum(u * v))
}


#' Vector Projection Function
#'
#' Find the vector projection.
#'
#' @param u vector of real numbers.
#' @param v vector of real numbers.
#'
#' @return Returns a projection of \code{u} onto \code{v}.
#' @export
#' @examples
#' v <- c(1, 2)
#' u <- c(3, -8)
#' projection(v, u)
#' v <- c(1, 0, 3)
#' u <- c(-1, 4, 2)
#' projection(v, u)
projection <- function(v, u){
  v.v <- dotprod(v, v)
  if(v.v == 0){
    stop('The magnitude of v is equal to zero')
  }
  v.u <- dotprod(v, u)

  (v.u/v.v) * v
}

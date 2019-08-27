#' Make L-th Norm Function
#' Function factory to create the L-th norm
#'
#' @param L numeric. The l-norm to create
#'
#' @return a function which calculates the L-th norm of a vector
#' @export
#'
#' @examples
#' l2_norm <- makeVecNorm(L = 2)
#' l3_norm <- makeVecNorm(L = 3)
makeVecNorm <- function(L) {
  function(vec){
    (sum(vec^L))^(1/L)
  }
}

#' Make n-th Root Function
#' Function factory to create a n-th root function
#' @param n numeric
#'
#' @return a function which complutes the n-th root of an input
#' @export
#'
#' @examples
#' square_root <- root(2)
#' cube_root <- root(3)
root <- function(n) {
  function(x) {
    (x)^(1/n)
  }
}

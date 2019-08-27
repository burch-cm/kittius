
# %notin% #####
#' Not IN
#'
#' @param x a thingy
#' @param y another thingy
#'
#' @return a logical
#' @export
#'
#' @examples
#' A <- c("Fido", "Leonidas", "Fuzzy", "Tralfaz")
#' A[A %notin% c("Fido")]
`%notin%` <- function(x,y) {
  !(x %in% y)
}

# no_space #####
#' Remove All Spaces From a String
#'
#' @param x a string
#' @param replacement a string to sub for spaces
#'
#' @return a string with spaces removed
#' @export
#'
#' @examples
#' no_space("abcd efgh")
#' no_space("abcd efgh", replacement = "_")
no_space <- function(x, replacement = "") {
  gsub(pattern = " *", replacement = replacement, x = x)
}
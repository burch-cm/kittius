
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

#' Create Fiscal Month Vector
#'
#' @param start numeric. The month in which the fiscal year begins.
#' @param abb logical. Return month abbreviations?
#'
#' @return a character vector of month names in fiscal year order
#' @export
#'
#' @examples
#' federal_year <- get_fiscal(start = 10)
get_fiscal <- function(start = 10, abb = FALSE) {
    if (start > 12 | start <= 0) stop("Starting month must be between 1 and 12")
    if (start == 1) {
        if (abb) return(month.abb)
        return(month.name)
    }
    if (abb) {
        c(month.abb[start:12], month.name[1:(start-1)])
    } else {
        c(month.name[start:12], month.name[1:(start-1)])
    }
}

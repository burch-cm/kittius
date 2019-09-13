#' Mutate at Condition
#'
#' @param .data a dataframe
#' @param condition A condition
#' @param ... all that jazz
#' @param envir the environment to scope
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @return the altered dataframe
#' @export
#'
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
    condition <- eval(substitute(condition), .data, envir)
    .data[condition, ] <- .data[condition, ] %>% mutate(...)
    .data
}

#' Smoosh Rows in a Data Frame
#'
#' @param .data data frame.
#' @param by character. The column to use as a key col.
#' @param to character vector. The columns to smoosh.
#' @param ... all that jazz
#'
#' @return a data frame.
#' @export
#'
smoosh <- function(.data, by, to = NULL, ...) {
    r <- eval(substitute(by), .data, parent.frame())
    .data[r, ]
}

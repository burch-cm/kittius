#' Mutate at Condition
#'
#' @param .data a dataframe
#' @param condition A condition
#' @param ... all that jazz
#' @param envir the environment to scope
#'
#' @return the altered dataframe
#' @export
#'
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
    condition <- eval(substitute(condition), .data, envir)
    .data[condition, ] <- .data[condition, ] %>% mutate(...)
    .data
}

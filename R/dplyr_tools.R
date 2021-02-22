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

#' Convert a List to a Data Frame
#'
#' @author Jason Bryer <<jason@@bryer.org>>
#' @references \url{http://stackoverflow.com/questions/4227223/r-list-to-data-frame}
#' @param x a list to convert to a data frame.
#' @param row.names a vector equal to \code{length(x)} corresponding to the row names.
#'        If \code{NULL}, the row names will be set to \code{names(x)}.
#' @param optional not used.
#' @param ... other parameters passed to \code{\link{data.frame}}.
#' @return a data frame.
#' @S3method as.data.frame list
#' @export
#' @examples
#'     test1 <- list( c(a='a',b='b',c='c'), c(a='d',b='e',c='f'))
#'     as.data.frame(test1)
#'
#'     test2 <- list( c('a','b','c'), c(a='d',b='e',c='f'))
#'     as.data.frame(test2)
#'
#'     test3 <- list('Row1'=c(a='a',b='b',c='c'), 'Row2'=c(var1='d',var2='e',var3='f'))
#'     as.data.frame(test3)
#'
#'     test4 <- list('Row1'=letters[1:5], 'Row2'=letters[1:7], 'Row3'=letters[8:14])
#'     as.data.frame(test4)
#'
#'     test5 <- list(letters[1:10], letters[11:20])
#'     as.data.frame(test5)
#'
#'     test6 <- list(list(letters), letters)
#'     as.data.frame(test6)
as.data.frame.list <- function(x, row.names=NULL, optional=FALSE, ...) {
    if(!all(unlist(lapply(x, class)) %in%
            c('raw','character','complex','numeric','integer','logical'))) {
        warning('All elements of the list must be a vector.')
        NextMethod(x, row.names=row.names, optional=optional, ...)
    }
    allequal <- all(unlist(lapply(x, length)) == length(x[[1]]))
    havenames <- all(unlist(lapply(x, FUN=function(x) !is.null(names(x)))))
    if(havenames) { #All the vectors in the list have names we can use
        colnames <- unique(unlist(lapply(x, names)))
        df <- data.frame(matrix(
            unlist(lapply(x, FUN=function(x) { x[colnames] })),
            nrow=length(x), byrow=TRUE))
        names(df) <- colnames
    } else if(allequal) { #No names, but are of the same length
        df <- data.frame(matrix(unlist(x), nrow=length(x), byrow=TRUE), ...)
        hasnames <- which(unlist(lapply(x, FUN=function(x) !is.null(names(x)))))
        if(length(hasnames) > 0) { #We'll use the first element that has names
            names(df) <- names(x[[ hasnames[1] ]])
        }
    } else { #No names and different lengths, we'll make our best guess here!
        warning(paste("The length of vectors are not the same and do not ",
                      "are not named, the results may not be correct.", sep=''))
        #Find the largest
        lsizes <- unlist(lapply(x, length))
        start <- which(lsizes == max(lsizes))[1]
        df <- x[[start]]
        for(i in (1:length(x))[-start]) {
            y <- x[[i]]
            if(length(y) < length(x[[start]])) {
                y <- c(y, rep(NA, length(x[[start]]) - length(y)))
            }
            if(i < start) {
                df <- rbind(y, df)
            } else {
                df <- rbind(df, y)
            }
        }
        df <- as.data.frame(df, row.names=1:length(x))
        names(df) <- paste('Col', 1:ncol(df), sep='')
    }
    if(missing(row.names)) {
        row.names(df) <- names(x)
    } else {
        row.names(df) <- row.names
    }
    return(df)
}

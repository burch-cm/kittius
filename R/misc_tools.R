
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

#' Find a Pattern in an .R Code File
#' @description Reads in a .R code file by line and searches for a given pattern.
#' @param .file character. The file to read in and search.
#' @param pattern character. A pattern to pass to grep, in RegExp format.
#' @param quiet logical. Should a warning be thrown if the pattern is not in the file?
#' @param ... pass through additional options
#' @importFrom readr read_lines
#' @import purrr
#' @return a data frame containing the line number and line text where the pattern was found in the file.
#' @export
#'
#' @examples
#' \dontrun{
#'     search_code("./R/misc_toolr.R", "export")
#' }
search_code <- function(.file, pattern, quiet = TRUE, ...) {
    rcode <- readr::read_lines(.file, ...)
    filename <- gsub(".*/", "", .file)
    pattern_loc <- grep(pattern, rcode)
    if (length(pattern_loc) == 0) {
        if (!quiet) warning("Pattern ", pattern, " not found in file ", .file, ".")
        return(NULL)
    }
    data.frame(file_name = as.character(filename),
               line_num = as.numeric(pattern_loc),
               code = as.character(rcode[pattern_loc]),
               stringsAsFactors = FALSE)
}

#' Search a Directory of R Files for a Pattern
#' Looks through all of the .R files in a directory for a given pattern in the code.
#' @param .dir character. The path to the directory containing the files to search.
#' @param pattern characte. The pattern, in RegExp format, for which to search the files.
#' @param as_list logical. Should search_dir return a list instead of a data frame?
#' @param drop_null logical. Drop the NULL values? Only applies when as_list is TRUE.
#' @param ... pass through additional options.
#' @importFrom purrr map map_df
#' @return a data frame containing the filename, line number, and code line text for any lines of code matching the pattern.
#' @export
#'
#' @examples
#' search_dir("./", "read.csv")
search_dir <- function(.dir, pattern, as_list = FALSE, drop_null = TRUE, ...) {
    .dir       <- gsub("/$", "", .dir)
    .filenames <- list.files(.dir, pattern = ".*R$")
    .dir_files <- file.path(.dir, .filenames)
    .dir_map   <- if (as_list) {
        a <- purrr::map(.dir_files, search_code, pattern)
        if (drop_null) {
            Filter(Negate(is.null), a)
        } else {
            a
        }
    } else {
        purrr::map_df(.dir_files, search_code, pattern)
    }
    return(.dir_map)
}

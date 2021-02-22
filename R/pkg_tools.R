#' Check if Package is Installed
#'
#' @param pkg character. The name of the package to check for.
#' @param quiet logical. Should the function suppress any console messages?
#'
#' @return A logical indicating whether the named package was found in the list of installed packages.
#' @export
#'
#' @examples
#' \dontrun{
#'     check_for("kittius")
#'     check_for("dplyr", quiet = FALSE)
#' }
check_for <- function(pkg, quiet = TRUE) {
    if (pkg %in% .packages(all.available = TRUE)) {
        if (!quiet) message("Package ", pkg, " installed.")
        return(TRUE)
    } else {
        if (!quiet) message("Package ", pkg, " not installed.")
        return(FALSE)
    }
}

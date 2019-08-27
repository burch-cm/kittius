
#' Calculate Tax Burden
#'
#' @param amount numeric. The amount of taxable income.
#' @param tax_table a data frame containing marinal tax rates.
#' @param total logical. Return the total amount paid?
#'
#' @return a numeric sum of taxes paid (if total = T) or a vector of taxes paid at the marginal rate
#' @export
#'
#' @examples
#' calc_tax(85000)
calc_tax <- function(amount, tax_table = NA, total = TRUE) {
    if (is.na(tax_table)) data(tax2019); tax_table <- tax2019
    single_tax <- function(x, tax_table, total) {
        paid <- vector("numeric", length = nrow(tax_table))
        for (i in 1:nrow(tax_table)) {
            if (x > tax_table$lower[i]) {
                paid[i] <- min(x - (max(0, tax_table$lower[i]-1)),
                               tax_table$upper[i] - (max(0, tax_table$lower[i]-1))
                ) *
                    tax_table$rate[i]
            }
        }
        if (total) return (sum(paid))
        return(paid)
    }

    if(total) {
        outs <- vector("numeric", length = length(amount))
        for (i in seq_along(amount)) {
            outs[i] <- single_tax(amount[i], tax_table, total)
        }
        return(outs)
    } else if (length(amount == 1)) {
        return(single_tax(amount, tax_table, total))
    } else {
        outs <- list()
        for (i in seq_along(amount)) {
            outs[[i]] <- single_tax(amount[i], tax_table, total)
        }
    }
}

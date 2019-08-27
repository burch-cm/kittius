## US 2019 Tax Brackets
# 10%	$0 to $9,700	        10% of taxable income
# 12%	$9,701 to $39,475	    $970 plus 12% of the amount over $9,700
# 22%	$39,476 to $84,200	    $4,543 plus 22% of the amount over $39,475
# 24%	$84,201 to $160,725	    $14,382.50 plus 24% of the amount over $84,200
# 32%	$160,726 to $204,100	$32,748.50 plus 32% of the amount over $160,725
# 35%	$204,101 to $510,300	$46,628.50 plus 35% of the amount over $204,100
# 37%	$510,301 or more	    $153,798.50 plus 37% of the amount over $510,300

lower <- c(0,
           9701,
           39476,
           84201,
           160726,
           204101,
           510301)
upper <- c(9700,
           39475,
           84200,
           160725,
           204100,
           510300,
           NA)
rate <- c(0.10,
          0.12,
          0.22,
          0.24,
          0.32,
          0.35,
          0.37)

tax2019 <- data.frame(lower, upper, rate)


calc_tax <- function(amount, tax_table = tax2019, total = TRUE) {
    
    single_tax <- function(x, tax_table, total) {
        paid <- vector("numeric", length = nrow(tax))
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

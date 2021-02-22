#' Find Differences Between A Data Set and an Updated Version
#'
#' @param .data a data.frame or tibble containing the updated data. At least the key column and one additional column must match the column names for the master data.
#' @param .master a data.frame or tibble containing the master VLD data to use as a baseline.
#' @param .key a character. The name of the column on which to match .data and .master. Must be present in both data frames.
#' @param return_all logical. Should the output include all rows and columns in the VLD? if FALSE, will only return the columns in both .data and .master with changed values
#' @param ignore_col_case logical. Should the column names be compared with or wthout case? if TRUE, will not match columns of differing cases, eg. Tag != tag. Defaults to FALSE, eg. Tag ~= tag.
#' @param ... additional arguments
#' @import dplyr
#' @return a data frame with base VLD dat
#' @export
#'
#' @examples
#' \dontrun{
#' my_updates %>% find_deltas(VLD_master, return_all = FALSE)
#' }
find_deltas <- function(.master, .data, .key = "VID", return_all = FALSE,
                        ignore_col_case = TRUE, ...) {
    # convert column names if needed
    if (ignore_col_case) {
        master_names <- names(.master)
        names(.master) <- tolower(master_names)
        data_names <- names(.data)
        names(.data) <- tolower(data_names)
        key_name <- .key
        .key <- tolower(key_name)
    }

    .data <- .data %>%
        select(names(.data)[names(.data) %in% names(.master)])

    s <- dplyr::filter(.master, (!!as.name(.key)) %in% .data[[.key]]) %>%
        dplyr::select(names(.data))
    resp <- dplyr::setdiff(.data, s)

    if (return_all) {
        out <- .master
        resp_names <- names(resp)[-1]
        out_ind <- which(.master[[.key]] %in% resp[[.key]])
        for (i in seq_along(names(resp))) {
            n <- resp_names[i]
            out[[n]] <- replace(.master[[n]], out_ind, resp[[n]])
        }
        return(out)
    } else {
        return(resp)
    }

}

#' Update Data Frame with Changed Fields
#'
#' @param .master data.frame. The initial data set.
#' @param .data data.frame with at least one column name shared with .master
#' @param .key character. The name of the shared key/index column between the data frames.
#' @param ignore_col_case logical. Should case be ignored when matching column names?
#' @param replace_names logical. Should the lowrcase names unsed in the function be replaced with the original data frame names?
#' @param return_list logical. Should a list with both the original data and updated data frames be returned? Defaults to FALSE.
#' @param ... additional arguments
#' @param na_value what value should replace NA values in the data?
#'
#' @return either a data.frame of a list of data.frames
#' @export
#'
#' @examples
#' \dontrun{vld %>% update_with(new_data1)}
update_with <- function(.master, .data, .key = "VID",
                        na_value = NA,
                        ignore_col_case = TRUE,
                        replace_names = FALSE,
                        return_list = FALSE,
                        ...) {
    # convert column names if needed
    if (ignore_col_case) {
        master_names <- names(.master)
        names(.master) <- tolower(master_names)
        data_names <- names(.data)
        names(.data) <- tolower(data_names)
        key_name <- .key
        .key <- tolower(key_name)
    }

    .data <- .data %>%
        distinct(!!as.name(.key))
    .data <- .data[names(.data)[names(.data) %in% names(.master)]]

    s <- dplyr::filter(.master, (!!as.name(.key)) %in% .data[[.key]]) %>%
        dplyr::select(names(.data))

    resp <- dplyr::setdiff(.data, s)

    updated <- .master
    resp_names <- names(resp)[!names(resp) %in% .key]
    out_ind <- which(.master[[.key]] %in% unique(resp[[.key]]))
    new_ind <- which(!resp[[.key]] %in% .master[[.key]])
    for (i in seq_along(names(resp))) {
        n <- resp_names[i]
        updated[[n]] <- replace(.master[[n]], out_ind, resp[[n]])
    }

    # add in new rows
    # updated <- cbind

    if (!is.na(na_value)) {
        updated[is.na(updated)] <- na_value
    }

    if (replace_names) {
        names(.master) <- master_names
        names(updated) <- master_names
    }

    if (return_list) {
        x <- list(original = .master,
                  updated  = updated)
        return(x)
    } else {
        return(updated)
    }

}




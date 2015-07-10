#' Reshape to Long
#'
#' Reshape \code{\link[base]{data.frame}} from wide to long format.
#'
#' @param x A \code{\link[base]{data.frame}} in wide format.
#' @return Returns a \code{\link[base]{data.frame}}.
#' @export
#' @examples
#' x <- r_data_frame(
#'         n = 500,
#'         id,
#'         age, age, age,
#'         grade, grade, grade
#' )
#' reshape_long(x)
#' library(dplyr)
#' x <- r_data_frame(
#'         n = 500,
#'         id,
#'         age,
#'         sex,
#'         r_series(date_stamp, 5, name = "Date"),
#'         r_series(likert, 5, name = "Question")
#' ) %>%
#'     reshape_long()
reshape_long <- function(x, idvar='ID') {
    rep.cols <- grep("_[0-9]+$", names(x), value=TRUE)
    times <- unique(as.numeric(sub('.*[_]', '', rep.cols)))
    newids <- c(outer(row.names(x), times, paste, sep='.'))
    x <- reshape(x, direction = "long", varying = rep.cols, sep="_",
            idvar=idvar, new.row.names=newids)
    x[do.call(order, x[,c(idvar,'time')]),]
}

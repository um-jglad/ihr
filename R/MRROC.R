#' Calculate the Median Recovery ROC (Rate of Change)
#'
#' @description
#' The function \code{MRROC} outputs the Median Recovery ROC (ROC < 0)
#'
#' @usage
#' MRROC(data, timelag = 1, inter_gap = 15, tz = "")
#'
#' @inheritParams roc
#'
#' @return A tibble object with two columns "id" and "MRROC"
#'
#' @export
#'
#' @examples
#' data(example_heart_1)
#' MRROC(example_heart_1)
#'

MRROC <- function(data, timelag = 1, inter_gap = 15, tz = ""){
  data = check_data_columns(data)

  out = roc(data, timelag = timelag, inter_gap = inter_gap, tz = tz) |>
    dplyr::filter(roc < 0) |>
    dplyr::group_by(id) |>
    dplyr::summarise(MRROC = median(roc, na.rm = T))

  return(out)
}

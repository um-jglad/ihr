#' Calculate the standard deviation of the rate of change
#'
#' @description
#' The function \code{sd_roc} produces the standard deviation of the rate of change
#' values in a tibble object.
#'
#' @usage
#' sd_roc(data, timelag = 1, inter_gap = 15, tz = "")
#'
#' @inheritParams roc
#'
#' @return A tibble object with two columns: subject id and standard deviation
#' of the rate of change values for each subject.
#'
#' @export
#'
#' @details
#' A tibble object with one row for each subject, a column for subject id
#' and a column for the standard deviation of the rate of change.
#'
#' When calculating rate of change, missing values will be linearly interpolated
#' when close enough to non-missing values.
#'
#' Calculated by taking the standard deviation of all the ROC values for each
#' individual subject. NA rate of change values are omitted from the
#' standard deviation calculation.

sd_roc <- function(data, timelag = 1, inter_gap = 15, tz = ""){
  hr = id = sd_roc = NULL
  rm(list = c("hr", "id", "sd_roc"))
  data = check_data_columns(data)

  out = roc(data, timelag,inter_gap, tz) |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      sd_roc = sd(roc, na.rm = TRUE)
    )
  return(out)
}

#' Calculate percentage of values above target thresholds
#'
#' @description
#' The function \code{above_percent} produces a tibble object with values equal to
#' the percentage of heart rate data above target values. The output columns
#' correspond to the subject id followed by the target values, and the
#' output rows correspond to the subjects. The values will be between 0
#' (no measurements) and 100 (all measurements).
#'
#' @usage
#' above_percent(data, targets_above = c(60, 85, 100))
#'
#' @param targets_above \strong{Default: (60, 85, 100).} Numeric vector of heart rate thresholds. Heart rate values from
#' data argument will be compared to each value in the targets_above vector.
#' @param data A DataFrame object with column names "id", "time", "hr".
#' Missing HR values (NA) are automatically excluded from calculations.
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' column for each target value is returned. NA's will be omitted from the glucose
#' values in calculation of percent.
#'
#' @return If a DataFrame object is passed, then a tibble object with
#' a column for subject id and then a column for each target value is returned. If a vector of heart rate
#' values is passed, then a tibble object without the subject id is returned. Wrap
#' `as.numeric()` around the latter to output a numeric vector.
#'
#' @export
#'
#'
#' @examples
#'
#' data(example_heart_1)
#'
#' above_percent(example_heart_1)
#' above_percent(example_heart_1, targets_above = c(100, 150, 180))
#'


above_percent <- function(data, targets_above = c(60, 85, 100)){
  x = target_val = id = time = hr = NULL
  rm(list = c("id", "target_val", "x", "time", "hr"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")
  targets_above = as.double(targets_above)
  out = lapply(
    targets_above,
    function(target_val) {
      data = data |>
        dplyr::group_by(id) |>
        dplyr::summarise(x = mean(hr > target_val, na.rm = TRUE) * 100) |>
        dplyr::mutate(target_val = paste0("above_", target_val))
      data
    })
  out = dplyr::bind_rows(out)
  out = tidyr::spread(data = out, key = target_val, value = x)
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}

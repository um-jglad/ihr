#' Calculate the thresholds for different subjects according to the provided quantile

#' @description
#' Computes the proper thresholds for different subjects based on the quantile,
#' outputting a tibble object with the subject ID and the thresholds.

#' @param data A DataFrame object with column names "id", "time", "hr".
#' Missing HR values (NA) are automatically excluded from calculations.
#' @param q \strong{Default = 0.5.} The quantile user wants to use for the thresholds

#' @return
#' If a dataframe object is passed, then a tibble object with a column for subject id and a column for the thresholds is returned.
#' 'NA' heart rate values are omitted from the calculation of the thresholds

#' @export
#' @examples
#' calc_thresholds(example_heart_1, q = 0.25)


calc_thresholds <- function(data, q = 0.5) {
  time = id = hr = NULL
  rm(list = c('time', 'id', 'hr'))

  if (!all(c("id", "hr") %in% colnames(data))) {
    stop("The dataset must contain 'id' and 'hr' columns.")
  }

  # Remove rows with missing HR values
  data <- dplyr::filter(data, !is.na(hr))

  # Compute quantile for each subject (id)
  thre_data <- data |>
    dplyr::group_by(id) |>
    dplyr::summarise(threshold = quantile(hr, q, na.rm = TRUE), .groups = "drop")

  return(thre_data)
}

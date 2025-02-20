calc_thresholds <- function(data, q = 0.5) {

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

#' Plot histogram of Rate of Change values (ROC)
#'
#' @description
#' The function hist_roc produces a histogram plot of ROC values
#'
#' @usage
#' hist_roc(data, timelag = 1, inter_gap = 15, tz = "")
#'
#' @inheritParams roc
#'
#' @export
#'
#' @examples
#' data(example_heart_1)
#' hist_roc(example_heart_1)

hist_roc <- function(data,  timelag = 15, inter_gap = 15, tz = "") {

  hr = id = roc = category = NULL
  rm(list = c("hr", "id", "roc", "category"))
  data = check_data_columns(data)

  data = data |>
    dplyr::group_by(id) |>
    dplyr::reframe(
      roc = roc(data.frame(id, time, hr), timelag, inter_gap, tz)$roc,
      category = cut(roc, breaks = c(-Inf, -3, -2, -1, 1, 2, 3, Inf),
                     labels = c("-Inf to -3", "-3 to -2", "-2 to -1",
                                "-1 to 1", "1 to 2", "2 to 3", "3 to Inf"))
    )

  colours = c("-Inf to -3" = "#0025FA", "-3 to -2" = "#197DE3",
              "-2 to -1" = "#B3FFF8", "-1 to 1" = "white",
              "1 to 2" = "#FEC7B6", "2 to 3" = "#FB5454",
              "3 to Inf" = "#9F0909")
  ggplot2::ggplot(data, ggplot2::aes(roc, fill = category)) +
    ggplot2::geom_histogram(binwidth = 0.1, alpha = 0.72, na.rm = TRUE) +
    ggplot2::facet_wrap(~id, scales = "free_x") +
    ggplot2::scale_fill_manual(values = colours, name = "Category (hr/min)")
}

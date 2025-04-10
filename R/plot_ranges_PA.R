#' Plot Time in Ranges as a bar plot
#'
#' @description
#' The function plot_ranges produces a barplot showing the percent of time in different stages of heart rate
#'
#' @usage
#' plot_ranges_PA(data)
#'
#' @param data A DataFrame object with column names "id", "time", "hr".
#' Missing HR values (NA) are automatically excluded from calculations.
#'
#'
#' @return Single subject bar chart showing percent in different glucose ranges.
#'
#' @export
#'
#' @details
#' Only a single subject's data may be used. sedentary/sleep (<20% HRR), light (20-39% HRR), moderate (40-59% HRR), and vigorous (>=60% HRR)
#' This plot is meant to be used as part of the Ambulatory Heart rate Profile (AHP)
#'
#' @references
#' Using Heart Rate and Accelerometry to Define Quantity and Intensity of Physical Activity in Older Adults
#' \doi{10.1093/gerona/gly029}
#'
#'
#' @examples
#'
#' data(example_heart_1)
#' plot_ranges_PA(example_heart_1)
#'

plot_ranges_PA <- function(data) {
  id = PA_stage = percent = NULL
  rm(list = c("id", "PA_stage", "percent"))

  subject <- unique(data$id)
  ns <- length(subject)
  if (ns > 1){
    subject <- subject[1]
    warning(paste("The provided data has", ns, "subjects. Plot will only be created for subject", subject))
    data <- dplyr::filter(data, id == subject)
  }

  # Summarize time in PA stages using summarize_PA logic
  summary <- summarize_PA(data)
  if (is.null(summary)) {
    message("Cannot create plot due to missing HRR classification.")
    return(NULL)
  }

  # Transform the summary into long format for plotting
  ranges <- summary |>
    tidyr::pivot_longer(
      cols = -id,
      names_to = "PA_stage",
      values_to = "percent"
    )

  # Define order and thresholds from summarize_PA()
  desired_order <- c("Vigorous", "Moderate", "Light", "Sedentary/Sleep")
  plot_order <- rev(desired_order)  # So sedentary is at the bottom

  # Define dynamic labels based on thresholds
  thresholds <- c("Sedentary/Sleep (<20% HRR)",
                  "Light (20–39% HRR)",
                  "Moderate (40–59% HRR)",
                  "Vigorous (≥60% HRR)")
  names(thresholds) <- c("Sedentary/Sleep", "Light", "Moderate", "Vigorous")

  colors <- c("#0073C2", "#48BA3C", "#F9B500", "#8E1B1B")

  ranges <- ranges |>
    dplyr::mutate(PA_stage = factor(PA_stage, levels = plot_order))

  ggplot2::ggplot(ranges, ggplot2::aes(x = 1, fill = PA_stage, y = percent)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(
      values = colors,
      drop = FALSE,
      labels = thresholds[plot_order]  # apply thresholds in correct order
    ) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, 10)) +
    ggplot2::labs(y = "Percentage of Time", title = "Physical Activity Distribution by %HRR") +
    ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank())
}

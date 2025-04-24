#' Plot Time in Ranges as a bar plot
#'
#' @description
#' The function plot_ranges produces a barplot showing the percent of time in different stages of heart rate
#'
#' @usage
#' plot_ranges_PA(data, tz)
#'
#' @param data A DataFrame object with column names "id", "time", "hr".
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning
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
plot_ranges_PA <- function(data, tz = "") {
  id = Stages = percent = NULL
  rm(list = c("id", "Stages", "percent"))

  subject <- unique(data$id)
  ns <- length(subject)
  if (ns > 1){
    subject <- subject[1]
    warning(paste("The provided data has", ns, "subjects. Plot will only be created for subject", subject))
    data <- dplyr::filter(data, id == subject)
  }

  summary <- summarize_PA(data, tz)
  if (is.null(summary)) {
    message("Cannot create plot due to missing HRR classification.")
    return(NULL)
  }

  # Get actual RHR and HRR
  HRR_info <- calculate_HRR(data)
  HRR_info <- dplyr::filter(HRR_info, id == subject)

  if (nrow(HRR_info) == 0 || any(is.na(HRR_info$RHR), is.na(HRR_info$HRR))) {
    message("Cannot compute threshold HR values for labeling.")
    return(NULL)
  }

  RHR <- HRR_info$RHR
  HRR <- HRR_info$HRR

  # Compute true HR thresholds
  hr_20 <- round(RHR + 0.20 * HRR, 1)
  hr_40 <- round(RHR + 0.40 * HRR, 1)
  hr_60 <- round(RHR + 0.60 * HRR, 1)

  ordered_stages <- c("Vigorous", "Moderate", "Light", "Sedentary/Sleep")

  ranges <- summary |>
    tidyr::pivot_longer(
      cols = -id,
      names_to = "Stages",
      values_to = "percent"
    ) |>
    dplyr::mutate(Stages = factor(Stages, levels = ordered_stages))

  # Dynamic labels using actual heart rate thresholds
  stage_labels <- c(
    "Vigorous" = paste0("Vigorous (>= RHR + 60%HRR) [", hr_60, " bpm]"),
    "Moderate" = paste0("Moderate (RHR + [40% - 60%]HRR) [", hr_40, " - ", hr_60 - 0.1, " bpm]"),
    "Light" = paste0("Light (RHR + [20% - 40%]HRR) [", hr_20, " - ", hr_40 - 0.1, " bpm]"),
    "Sedentary/Sleep" = paste0("Sedentary/Sleep (<= RHR + 20%HRR) [", hr_20 - 0.1, " bpm]")
  )

  stage_colors <- c(
    "Sedentary/Sleep" = "#0073C2",
    "Light" = "#48BA3C",
    "Moderate" = "#F9B500",
    "Vigorous" = "#8E1B1B"
  )

  ggplot2::ggplot(ranges, ggplot2::aes(x = 1, fill = Stages, y = percent)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(
      values = stage_colors[ordered_stages],
      labels = stage_labels[ordered_stages],
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, 10)) +
    ggplot2::labs(
      y = "Percentage of Time",
      title = paste0("Physical Activity Distribution by %HRR")
    ) +
    ggplot2::theme(
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )
}

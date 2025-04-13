#' Plot Ambulatory Heart Rate Profile (AHP) modal day
#'
#' @description
#' The function plot_ahp produces an AhP plot that collapses all data into a single 24 hr "modal day".
#'
#' @usage
#' plot_ahp(data, smooth = FALSE, span = 0.3,
#' inter_gap = 15, tz = "", title = FALSE)
#'
#' @param data DataFrame object with column names "id", "time", "hr"
#' @param inter_gap The maximum allowable gap (in minutes) for interpolation. The values will not be interpolated between the Heart Rate measurements that are more than inter_gap minutes apart.
#' @param tz  A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning
#' @param title Indicator whether the title of the plot should display the subject ID. The default is FALSE (no title).
#' @param smooth Boolean indicating whether quantiles should be smoothed before plotting, default is TRUE
#' @param span Optional parameter indicating span for loess smoothing. Default is 0.3, larger values result in more smoothing,
#' recommended to choose between 0.1 to 0.7.
#'
#' @return Plot of a 24 hr modal day collapsing all data to a single day.
#'
#' @export
#'
#' @examples
#' data(example_heart_1)
#' plot_ahp(example_heart_1)
#'

plot_ahp <- function (data, smooth = FALSE,
                      span = 0.3, inter_gap = 15, tz = "", title = FALSE) {

  # Summarizing data at a minute level
  data <- data |>
    dplyr::mutate(time = lubridate::floor_date(time, unit = "minute")) |>
    dplyr::group_by(id, time) |>
    dplyr::summarise(hr = mean(hr), .groups = "drop")

  hr = id = five = twentyfive = seventyfive = ninetyfive = times = times_numeric = value = NULL
  rm(list = c("hr",  "id", "five", "twentyfive", "seventyfive", "ninetyfive", "times", "times_numeric", "value"))

  subject = unique(data$id)
  ns = length(subject)
  if (ns > 1){
    subject = subject[1]
    warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
    data = data |> dplyr::filter(id == subject)
  }

  data_ip = HR2DayByDay(data, dt0 = 1, inter_gap = inter_gap, tz = tz)
  hr_ip = data_ip[[1]]
  quartiles <- apply(hr_ip, 2, quantile, probs = c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm = TRUE)
  # Next line of code adds labels, issue needs to be fixed
  q_labels <- dplyr::as_tibble(quartiles[, ncol(quartiles)])


  if (smooth) {
    # complete <- are indicators of minutes where observations were valid
    times_full = as.numeric(seq(data_ip[[3]]*60, 86400, by = data_ip[[3]]*60))
    complete <- complete.cases(quartiles[1, ], times_full)
    temp_data = dplyr::tibble(
      # Time for 24 Hour span
      times_numeric = as.numeric(seq(data_ip[[3]]*60, 86400, by = data_ip[[3]]*60))[complete],
      # Calculating Smoothing
      median = loess(quartiles[3, complete]~times_numeric, span = span)$fitted,
      five = loess(quartiles[1, complete]~times_numeric, span = span)$fitted,
      twentyfive = loess(quartiles[2, complete]~times_numeric, span = span)$fitted,
      seventyfive = loess(quartiles[4, complete]~times_numeric, span = span)$fitted,
      ninetyfive = loess(quartiles[5, complete]~times_numeric, span = span)$fitted
    )
    total_min <- as.numeric(length(times_full))
    plot_data <- data.frame(times_numeric = rep(NA, total_min),
                            median = rep(NA, total_min),
                            five = rep(NA, total_min),
                            twentyfive = rep(NA, total_min),
                            seventyfive = rep(NA, total_min),
                            ninetyfive = rep(NA, total_min)
    )
    # j represents current row of the temp_data, incremented for every addition
    j <- 1
    for(i in 1:length(times_full)){
      if(complete[i]){
        plot_data[i, ] <- temp_data[j, ]
        j <- j + 1
      }
      else{
        plot_data$times_numeric[i] <- times_full[i]
      }
    }
    # Add proper time variable
    plot_data <- plot_data |> dplyr::mutate(times = hms::as_hms(times_numeric))
  } else {
    plot_data = dplyr::tibble(
      # Time for 24 Hour span
      times = hms::as_hms(seq(data_ip[[3]]*60, 86400, by = data_ip[[3]]*60)),
      median = quartiles[3, ], five = quartiles[1, ], twentyfive = quartiles[2, ],
      seventyfive = quartiles[4, ], ninetyfive = quartiles[5, ]
    )
  }
  p = ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(ggplot2::aes(times, median), color = "black", size = 1) +
    ggplot2::geom_line(ggplot2::aes(times, five), linetype = "longdash", color = "#325DAA") +
    ggplot2::geom_line(ggplot2::aes(times, ninetyfive), linetype = "longdash", color = "#325DAA") +
    ggplot2::geom_ribbon(ggplot2::aes(times, ymin = seventyfive, ymax = ninetyfive),
                         fill = "#A7BEE7", alpha = 0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(times, ymin = five, ymax = twentyfive),
                         fill = "#A7BEE7", alpha = 0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(times, ymin = twentyfive, ymax = seventyfive),
                         fill = "#325DAA", alpha = 0.5) +
    ggplot2::scale_x_time(breaks = c(hms::as_hms(c('00:00:00', '03:00:00', '06:00:00', '09:00:00', '12:00:00',
                                                   '15:00:00', '18:00:00', '21:00:00', '24:00:00'))),
                          labels = c('12 am', '3 am', '6 am', '9 am', '12 pm',
                                     '3 pm', '6 pm', '9 pm', '12 am')) +
    ggplot2::ylab("Heart Rate [BPM]") + ggplot2::xlab(NULL) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(1,3,1,1), "lines")) +
    ggplot2::geom_text(data = q_labels,
                       ggplot2::aes(label = c("5%", "25%", "50%", "75%", "95%"), y = value),
                       x = 90700, hjust = 0, size = 3.25) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(plot.margin = ggplot2::unit(c(1,3,1,1), units = "line"))

  if (title){
    p + ggplot2::ggtitle(subject)
  }else{
    p
  }

}

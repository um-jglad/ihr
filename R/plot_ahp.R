plot_ahp <- function (data, LLTR = 70, ULTR = 180, smooth = TRUE,
                      span = 0.3, dt0 = 1, inter_gap = 15, tz = "", title = FALSE) {

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
    data = data %>% dplyr::filter(id == subject)
  }

  data_ip = HR2DayByDay(data, dt0 = 1, inter_gap = inter_gap, tz = tz)
  hr_ip = data_ip[[1]]
  quartiles <- apply(hr_ip, 2, quantile, probs = c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm = TRUE)
  q_labels <- dplyr::as_tibble(quartiles[, ncol(quartiles)])


  if (smooth) {
    plot_data = dplyr::tibble(
      times_numeric = as.numeric(seq(data_ip[[3]]*60, 86400, by = data_ip[[3]]*60)),
      median = loess(quartiles[3, ]~times_numeric, span = span)$fitted,
      five = loess(quartiles[1, ]~times_numeric, span = span)$fitted,
      twentyfive = loess(quartiles[2, ]~times_numeric, span = span)$fitted,
      seventyfive = loess(quartiles[4, ]~times_numeric, span = span)$fitted,
      ninetyfive = loess(quartiles[5, ]~times_numeric, span = span)$fitted,
      times = hms::as_hms(times_numeric)
    )
  } else {
    plot_data = dplyr::tibble(
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
    ggplot2::geom_hline(yintercept = LLTR, color = '#48BA3C') +
    ggplot2::geom_hline(yintercept = ULTR, color = '#48BA3C') +
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

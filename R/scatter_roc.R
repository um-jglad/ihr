scatter_roc <- function(data, timelag = 1, inter_gap = 15, tz = ""){
  data = check_data_columns(data)

  # Summarizing data at a minute level
  data <- data |>
    dplyr::mutate(time = lubridate::floor_date(time, unit = "minute")) |>
    dplyr::group_by(id, time) |>
    dplyr::summarise(hr = mean(hr), .groups = "drop")

  full_hr <- c()
  for(i in unique(data$id)){
    hr_data <- HR2DayByDay(dplyr::filter(data, id == i), dt0 = 1, inter_gap = inter_gap)
    hr_data <- c(t(hr_data$gd2d))
    full_hr <- rbind(full_hr, hr_data)
  }


  roc_data <- data |>
    dplyr::group_by(id) |>
    dplyr::reframe(
      roc = roc(data.frame(id, time, hr), timelag, inter_gap, tz)$roc)

  roc_data <- cbind(roc_data, hr_data) |>
    dplyr::filter(!is.na(roc))

  .p <- roc_data |>
    ggplot(aes(x = roc, y = hr_data)) +
    geom_point() +
    facet_wrap(~id) +
    scale_x_continuous(name = "Rate of Change") +
    scale_y_continuous(name = "Heart Rate (BPM)")

  return(.p)

}



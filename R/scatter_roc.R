scatter_roc <- function(data, timelag = 1, inter_gap = 15, tz = ""){

  id = hr = NULL
  rm(list = c("id", "hr"))

  data = check_data_columns(data)

  # Summarizing data at a minute level
  data <- data |>
    dplyr::mutate(time = lubridate::floor_date(time, unit = "minute")) |>
    dplyr::group_by(id, time) |>
    dplyr::summarise(hr = mean(hr), .groups = "drop")

  # Interpolating for all id, flattening to one row
  full_hr <- data.frame()
  for(i in unique(data$id)){
    hr_data <- HR2DayByDay(dplyr::filter(data, id == i), dt0 = 1, inter_gap = inter_gap)
    hr_data <- as.data.frame(c(t(hr_data$gd2d)))
    full_hr <- rbind(full_hr, hr_data)
  }

  colnames(full_hr) <- "hr"

  # Generating ROC for all ids
  roc_data <- data |>
    dplyr::group_by(id) |>
    dplyr::reframe(
      roc = roc(data.frame(id, time, hr), timelag, inter_gap, tz)$roc)


  roc_data <- cbind(roc_data, full_hr) |>
    dplyr::filter(!is.na(roc))

  return(roc_data)

  .p <- roc_data |>
    ggplot(aes(x = roc, y = hr)) +
    geom_point() +
    facet_wrap(~id) +
    scale_x_continuous(name = "Rate of Change") +
    scale_y_continuous(name = "Heart Rate (BPM)")

  return(.p)

}



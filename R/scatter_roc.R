scatter_roc <- function(data, timelag = 1, inter_gap = 15, tz = ""){
  data = check_data_columns(data)

  # Summarizing data at a minute level
    data <- data |>
      dplyr::mutate(time = lubridate::floor_date(time, unit = "minute")) |>
      dplyr::group_by(id, time) |>
      dplyr::summarise(hr = mean(hr), .groups = "drop")

  hr_data <- HR2DayByDay(data, dt0 = 1, inter_gap = inter_gap)
  hr_data <- c(t(hr_data))

  roc_data <- roc(data, timelag, inter_gap, tz) |>
    filter(!is.na(roc))


}



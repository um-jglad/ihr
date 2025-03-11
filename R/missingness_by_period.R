missingness_by_period<- function(data) {

  if (!all(c("id", "time", "hr") %in% colnames(data))) {
    stop("The dataset must contain 'id', 'time', and 'hr' columns.")
  }
  
  data <- data |>
    mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S"),
           time_date = as.Date(time),
           hour = as.integer(format(time, "%H")),
           minute = as.integer(format(time, "%M")),
           period = case_when(
             hour >= 3 & hour < 7 ~ "3am-7am",
             hour >= 7 & hour < 11 ~ "7am-11am",
             hour >= 11 & hour < 15 ~ "11am-3pm",
             hour >= 15 & hour < 19 ~ "3pm-7pm",
             hour >= 19 & hour < 23 ~ "7pm-11pm",
             TRUE ~ "11pm-3am"
           ))
  
  # Count the number of unique minutes per ID per period per day
  unique_minutes_per_period <- data |>
    group_by(id, period, time_date) |>
    summarise(unique_minutes = n_distinct(hour * 60 + minute), .groups = "drop")
  
  total_minutes <- 4 * 60
  
  # Compute missingness percentage per period per day
  missingness_per_day <- unique_minutes_per_period |>
    mutate(missing_percent = round(100 * (1 - unique_minutes / total_minutes), 2))
  
  period_missingness <- missingness_per_day |>
    group_by(id, period) |>
    summarise(mean_missing_percent = mean(missing_percent, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = period, values_from = mean_missing_percent)
  
  return(period_missingness)
}

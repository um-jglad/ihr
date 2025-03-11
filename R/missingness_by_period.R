#' Calculate the % missingness of the heart rate value during different time period

#' @description
#' The function missingness_by_period computes the percentage missingness of the heart rate data during six different time periods each day.
#' Choose 3am-7am as one of the time period due to the definition of resting heart rate in the reference. The other time periods are set following this 3am-7am period.

#' @usage
#' missingness_by_period(data)

#' @param data A DataFrame object with column names "id", "time", "hr".
#' Missing HR values (NA) are automatically excluded from calculations.

#' @return
#' If a dataframe object is passed, then a tibble object with a column for subject id and a column for each of percentage missingness is returned.
#' 'NA' heartrate values are omitted from the calculation of the summary values.

#' @export
#' @examples
#' data(example_heart_1)
#' missingness_by_period(example_heart_1)


missingness_by_period <- function(data) {

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

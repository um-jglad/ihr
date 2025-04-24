#' Calculate the % missingness of the heart rate value during different time period

#' @description
#' The function missingness_by_period computes the percentage missingness of the heart rate data during six different time periods each day.
#' Choose 3am-7am as one of the time period due to the definition of resting heart rate in the reference. The other time periods are set following this 3am-7am period.

#' @usage
#' missingness_by_period(data)

#' @param data A DataFrame object with column names "id", "time", "hr".
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning
#' Missing HR values (NA) are automatically excluded from calculations.

#' @return
#' If a dataframe object is passed, then a tibble object with a column for subject id and a column for each of percentage missingness is returned.
#' 'NA' heartrate values are omitted from the calculation of the summary values.

#' @export
#' @examples
#' data(example_heart_1)
#' missingness_by_period(example_heart_1)


missingness_by_period <- function(data, tz = "") {
  time = hr = id = period = hour = minute = unique_minutes = time_date = missing_percent = mean_missing_percent = NULL
  rm(list = c('time', 'hr', 'id'))

  if (!all(c("id", "time", "hr") %in% colnames(data))) {
    stop("The dataset must contain 'id', 'time', and 'hr' columns.")
  }

  data <- data |>
    dplyr::mutate(
      time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = tz),
      time_date = as.Date(time),
      hour = as.integer(format(time, "%H")),
      minute = as.integer(format(time, "%M")),
      period = dplyr::case_when(
        hour >= 3 & hour < 7 ~ "3am-7am",
        hour >= 7 & hour < 11 ~ "7am-11am",
        hour >= 11 & hour < 15 ~ "11am-3pm",
        hour >= 15 & hour < 19 ~ "3pm-7pm",
        hour >= 19 & hour < 23 ~ "7pm-11pm",
        TRUE ~ "11pm-3am"
      )
    )

  # Count unique minutes per ID per period per day
  unique_minutes_per_period <- data |>
    dplyr::group_by(id, period, time_date) |>
    dplyr::summarise(unique_minutes = dplyr::n_distinct(hour * 60 + minute), .groups = "drop")

  total_minutes <- 4 * 60  # Each period is 4 hours (4 * 60 minutes)

  # Compute missingness percentage per period per day
  missingness_per_day <- unique_minutes_per_period |>
    dplyr::mutate(
      missing_percent = dplyr::case_when(
        unique_minutes == 0 ~ 100,  # If no minutes were recorded, set to 100% missing
        TRUE ~ round(100 * (1 - unique_minutes / total_minutes), 2)  # Otherwise, calculate normally
      )
    )
  all_periods <- c("3am-7am", "7am-11am", "11am-3pm", "3pm-7pm", "7pm-11pm", "11pm-3am")
  missingness_per_day <- missingness_per_day |>
    tidyr::complete(id, period = all_periods, fill = list(missing_percent = 100))  # Fill missing periods with 100

  # Compute total days per ID (before grouping by period to avoid duplication)
  total_days_per_id <- data |>
    dplyr::group_by(id) |>
    dplyr::summarise(total_days = as.integer(difftime(max(time_date), min(time_date), units = "days")) + 1, .groups = "drop")

  # Compute mean missingness percentage per period per ID
  period_missingness <- missingness_per_day |>
    dplyr::group_by(id, period) |>
    dplyr::summarise(mean_missing_percent = mean(missing_percent, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = period, values_from = mean_missing_percent)

  # Merge total_days into the final output
  final_output <- dplyr::left_join(total_days_per_id, period_missingness, by = "id")

  return(final_output)
}

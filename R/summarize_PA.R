#' Calculate the time an individual spend in different stages(following the definition in the reference)

#' @description
#' The function summary_PA computes the time(in days) for different individuals spend in different stages
#' Using the thresholds: sedentary/sleep (<20% HRR), light (20–39% HRR), moderate (40–59% HRR), and vigorous (≥60% HRR)

#' @usage
#' summarize_PA(data)

#' @param data A DataFrame object with column names "id", "time", "hr".
#' Missing HR values (NA) are automatically excluded from calculations.

#' @return
#' If a dataframe object is passed, then a tibble object with a column for subject id and a column for each of summary values is returned.
#' 'NA' heartrate values are omitted from the calculation of the summary values.

#' @references
#' Using Heart Rate and Accelerometry to Define Quantity and Intensity of Physical Activity in Older Adults
#' \doi{10.1093/gerona/gly029}

#' @export
#' @examples
#' data(example_heart_1)
#' summarize_PA(example_heart_1)

summarize_PA <- function(data) {
  time = hr = id = hour = minute = mean_hr = HRR = PA_stage=total_hours = NULL

  data$time <- as.POSIXct(data$time, format = "%Y-%m-%d %H:%M:%S")

  # Round time down to the nearest minute
  data <- data |>
    dplyr::mutate(minute = lubridate::floor_date(time, unit = "minute"))

  # Average HR within each minute per participant
  min_hr_data <- data |>
    dplyr::group_by(id, minute) |>
    dplyr::summarize(mean_hr = mean(hr, na.rm = TRUE), .groups = "drop")

  # Calculate RHR and max HR per participant
  HRR_info <- calculate_HRR(data, method = "max-RHR")
  if (is.null(HRR_info)) {
    message("Missing RHR or HRR, cannot classify PA.")
    return(NULL)
  }

  classified <- dplyr::left_join(min_hr_data, HRR_info, by = "id") |>
    dplyr::mutate(
      pct_HRR = mean_hr / HRR * 100,
      PA_stage = dplyr::case_when(
        pct_HRR < 20 ~ "Sedentary/Sleep",
        pct_HRR < 40 ~ "Light",
        pct_HRR < 60 ~ "Moderate",
        pct_HRR >= 60 ~ "Vigorous",
        TRUE ~ NA_character_
      )
    )

  desired_order <- c("Sedentary/Sleep", "Light", "Moderate", "Vigorous")

  summary_table_raw <- classified |>
    dplyr::group_by(id, PA_stage) |>
    dplyr::summarize(total_hours = dplyr::n() / (24*60), .groups = "drop") |>
    tidyr::pivot_wider(names_from = PA_stage, values_from = total_hours, values_fill = 0)

  summary_table <- summary_table_raw |>
    dplyr::select(id, dplyr::all_of(intersect(desired_order, names(summary_table_raw))))
  return(summary_table)
}

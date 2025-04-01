summarize_PA <- function(data) {
  library(dplyr)
  library(lubridate)

  data$time <- as.POSIXct(data$time, format = "%Y-%m-%d %H:%M:%S")

  # Round time down to the nearest minute
  data <- data %>%
    mutate(minute = floor_date(time, unit = "minute"))

  # Average HR within each minute per participant
  min_hr_data <- data %>%
    group_by(id, minute) %>%
    summarize(mean_hr = mean(hr, na.rm = TRUE), .groups = "drop")

  # Calculate RHR and max HR per participant
  HRR_info <- calculate_HRR(data, method = "max-RHR")
  if (is.null(HRR_info)) {
    message("Missing RHR or HRR, cannot classify PA.")
    return(NULL)
  }

  classified <- left_join(min_hr_data, HRR_info, by = "id") %>%
    mutate(
      pct_HRR = (mean_hr - RHR) / HRR * 100,
      PA_stage = case_when(
        pct_HRR < 20 ~ "Sedentary/Sleep",
        pct_HRR < 40 ~ "Light",
        pct_HRR < 60 ~ "Moderate",
        pct_HRR >= 60 ~ "Vigorous",
        TRUE ~ NA_character_
      )
    )

desired_order <- c("Sedentary/Sleep", "Light", "Moderate", "Vigorous")

summary_table <- classified %>%
  group_by(id, PA_stage) %>%
  summarize(total_hours = n() / (24*60), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = PA_stage, values_from = total_hours, values_fill = 0) %>%
  dplyr::select(id, all_of(intersect(desired_order, names(.))))
  return(summary_table)
}

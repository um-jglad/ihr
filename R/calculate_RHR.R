calculate_RHR <- function(data) {
  data$time <- as.POSIXct(data$time, format="%Y-%m-%d %H:%M:%S")
  data$hour <- format(data$time, "%H")
  filtered_data <- subset(data, hour >= "03" & hour < "07")
  mean_hr <- mean(filtered_data$hr, na.rm = TRUE)
  return(mean_hr)
}

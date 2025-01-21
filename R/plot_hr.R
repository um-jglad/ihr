#' Plotting time series plot of heart rate measurements
#'
#' @param data Dataframe with column names ("id, "time", "hr")
#' @param LHR Lower heart rate set at default = 60 BPM
#' @param UHR Upper heart rate set at default = 100 BPM
#'
#' @return Heart rate time series plot for a single subject
#'
#'
#'


plot_hr <- function(data, LHR = 60, UHR = 100){
  # Testing to see if appropriate format
  data %>% select(id, time, hr)

  # Converting time to an appropriate format
  if(!lubridate::is.POSIXct(data$time)){
    data$time <- as.character(data$time)
    data$time <- as.POSIXct(data$time, format = "%Y-%m-%d %H:%M:%S")
  }

  # Plotting time series
  plot <- data %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(aes(x = time, y = hr, group = 1)) +
    ggplot2::geom_hline(yintercept = LHR, color = "red") +
    ggplot2::geom_hline(yintercept = UHR, color = "red") +
    ggplot2::scale_x_datetime(name = "Date") +
    ggplot2::scale_y_continuous(name = "Heart Rate (BPM)") +
    ggplot2::facet_wrap(~id)

  return(plot)
}


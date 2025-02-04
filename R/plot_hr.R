#' Plotting time series plot of heart rate measurements
#'
#' @description
#' The function 'plot_hr' supports time series plotting for both single and multiple subject data
#'
#' @param data Dataframe with column names ("id, "time", "hr")
#' @param LHR Lower Heart Rate set at \strong{default = 60}
#' @param UHR Upper Heart Rate set at \strong{default = 100}
#' @param from Starting time of plot inclusive (In format %Y-%m-%d)
#' @param to Ending time of plot exclusive (In format %Y-%m-%d)
#' @param agg Aggregate data by specified time interval
#' @param inter_gap Gap allowed between consecutive observations (in seconds)
#'
#' @details
#' See reasoning for default LHR and UHR values at \url{https://www.heart.org/en/health-topics/arrhythmia/about-arrhythmia/tachycardia--fast-heart-rate}
#'
#' @return Heart rate time series plot for a single subject
#'
#' @export
#'
#' @examples
#' data(example_heart_1)
#' plot_hr(example_heart_1)
#'


plot_hr <- function(data, LHR = 60, UHR = 100, from = "", to = "", agg = c('none', 'minute', 'hour'),
                    inter_gap = 60){
  id = time = hr = gap = time_group = NULL
  rm(list = c("id", "time", "hr", "gap", "time_group"))

  agg = match.arg(agg, c('none', 'minute', 'hour'))

  # Testing to see if appropriate format
  data |> dplyr::select(id, time, hr)

  # Converting time to an appropriate format
  if(!lubridate::is.POSIXct(data$time)){
    data$time <- as.character(data$time)
    data$time <- as.POSIXct(data$time, format = "%Y-%m-%d %H:%M:%S")
  }

  # Incorporating time range functionality
  if(from != ""){
    lower_time <- as.POSIXct(from, format = "%Y-%m-%d")
    data <- data |> dplyr::filter(time >= lower_time)
  }
  if(to != ""){
    upper_time <- as.POSIXct(to, format = "%Y-%m-%d")
    data <- data |> dplyr::filter(time < upper_time)
  }

  # Aggregating the data by time
  if(agg != 'none'){
    if((agg == "minute" & inter_gap < 60) | (agg == "hour" & inter_gap < 3600)){
      stop("Error: Adjust inter_gap (>= 60 for agg = 'min', >=3600 for agg = 'hour')")
    }
    data$time <- lubridate::floor_date(data$time, unit = agg)
    data <- data |>
      dplyr::group_by(id, time) |>
      dplyr::summarise(hr = mean(hr))
  }

  # Accounting for gaps
  gaps <- data |>
    dplyr::mutate(gap = ifelse(difftime(time, dplyr::lag(time), units = "secs") > inter_gap,
                               TRUE, FALSE), row = 1:length(time)) |>
    dplyr::slice(1, which(gap))
  gaps <- c(gaps$row, nrow(data) + 1)
  data <- data |>
    dplyr::mutate(time_group = rep(1:(length(gaps) - 1), diff(gaps)))

  # Plotting time series
  plot <- data |>
    ggplot2::ggplot() +
    ggplot2::geom_line(aes(x = time, y = hr, group = time_group)) +
    ggplot2::geom_hline(yintercept = LHR, color = "red") +
    ggplot2::geom_hline(yintercept = UHR, color = "red") +
    ggplot2::scale_x_datetime(name = "Time") +
    ggplot2::scale_y_continuous(name = "Heart Rate (BPM)") +
    ggplot2::facet_wrap(~id)

  return(plot)
}



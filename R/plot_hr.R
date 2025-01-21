#' Plot time series plot of heart rate measurements
#'
#' @param data Dataframe with column names ("id, "time", "hr")
#'
#'
#'
#'
#'
#'
#'


plot_hr <- function(data){
  plot <- data %>%
    ggplot2::ggplot(aes(x = time, y = hr)) +
    ggplot2::geom_line()

  return(plot)
}

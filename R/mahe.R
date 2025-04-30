#' Calculate Mean Amplitude of Heartrate Excursions
#'
#' @description
#' The function calculates MAHE values (a metric inspired by MAGE from glucose monitoring) and optionally returns a heart trace plot
#'
#' @param data DataFrame object with column names "id", "time", "hr"
#' @param short_ma \strong{Default: 5.} Integer for period length of the short moving average. Must be positive and less than `long_ma`. (Recommended <15)
#' @param long_ma \strong{Default: 32.} Integer for period length for the long moving average. Must be positive and greater than `short_ma`. (Recommended >20)
#' @param from \strong{Default: First observed time.} Starting time of the data inclusive (In format %Y-%m-%d)
#' @param to \strong{Default: Last observed time.} Ending time of the data exclusive (In format %Y-%m-%d)
#' @param return_type \strong{Default: "num".} One of ("num", "df-seg", "df-exc). Will return either a single number for the "MAHE over the entire trace" (weighted by segment length), a DataFrame with the MAHE value for each segment, or a DataFrame with excursion level details on amplitude, time length, and heart rate at peak and nadirs
#' @param direction \strong{Default: "avg".} One of ("avg", "service", "max", "plus", or "minus"). Algorithm will calculate one of the following: MAHE+ (nadir to peak), MAHE- (peak to nadir), MAHEavg = avg(MAHE+, MAHE-), MAHEmax = max(MAHE+, MAHE-), or automatically choose MAHE+/MAHE- based on the first countable excursion (i.e., "service"). NOTE: the selection of peak-to-nadir or nadir-to-peak is chosen independently on each segment, thus MAHEservice may choose peak-to-nadir on one segment and nadir-to-peak on another, for example.
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning
#' @param inter_gap \strong{Default: 15.} The maximum allowable gap (in minutes) for interpolation. The values will not be interpolated between the Heart Rate measurements that are more than inter_gap minutes apart.
#' @param plot \strong{Default: FALSE.} Boolean. If `TRUE`, returns a plot that visualizes all identified peaks and nadirs, excursions, and  missing gaps.
#' @param max_gap \strong{Default: 180.} Integer for the maximum length of a gap in minutes before the trace is split into segments and MAGE is calculated on each segment independently.
#' @param title  \strong{Default: "Heart Rate Trace Trace - Subject ID".} Title for the ggplot.
#' @param xlab \strong{Default: "Time".} Label for x-axis of ggplot.
#' @param ylab \strong{Default: "Glucose Level".} Label for y-axis of ggplot.
#' @param show_ma \strong{Default: FALSE.} Boolean. If TRUE, plots the moving average lines on the plot.
#' @param show_excursions \strong{Default: TRUE.} Boolean. If TRUE, shows identified excursions as arrows from peak-to-nadir/nadir-to-peak on the plot.
#'
#' @return A tibble object with two columns: the subject id and corresponding MAHE value.
#'
#' @export
#'
#' @references
#' Service et al. (1970) Mean amplitude of glycemic excursions, a measure of diabetic instability
#' \emph{Diabetes}  \strong{19} .644-655,
#' \doi{10.2337/diab.19.9.644}.
#'
#' Fernandes, Nathaniel J., et al. "Open-source algorithm to calculate mean amplitude of glycemic excursions using short and long moving averages."
#' Journal of diabetes science and technology 16.2 (2022): 576-577. \doi{10.1177/19322968211061165}
#'
#' @examples
#' data(example_heart_1)
#' mahe(example_heart_1)
#'

mahe <- function(data,
                 short_ma = 5, long_ma = 32,
                 from = "", to = "",
                 return_type = c('num', 'df-seg', 'df-exc'),
                 direction = c('avg', 'service', 'max', 'plus', 'minus'),
                 tz = "", inter_gap = 15,
                 max_gap = 180,
                 plot = FALSE, title = NA, xlab = NA, ylab = NA, show_ma = FALSE, show_excursions = TRUE) {

  direction = match.arg(direction, c('avg', 'service', 'max', 'plus', 'minus'))
  return_type = match.arg(return_type, c('num', 'df-seg', 'df-exc'))

  return(mahe_ma(data, short_ma = short_ma, long_ma = long_ma, from = from, to = to, return_type=return_type, direction=direction,
                 plot = plot, inter_gap = inter_gap, max_gap = max_gap, tz = tz,
                 title = title, xlab = xlab, ylab = ylab, show_ma = show_ma, show_excursions=show_excursions))
}


mahe_ma <- function(data,
                    short_ma = 5, long_ma = 32,
                    from = "", to = "",
                    return_type = c('num', 'df-seg', 'df-exc'),
                    direction = c('avg', 'service', 'max', 'plus', 'minus'),
                    inter_gap = 15, tz = "",
                    max_gap = 180,
                    plot = FALSE, title = NA, xlab = NA, ylab = NA, show_ma = FALSE, show_excursions=TRUE) {
  id = . = MAHE = hr = NULL
  rm(list = c("id", ".", "MAHE", "hr"))

  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")
  direction = match.arg(direction, c('avg', 'service', 'max', 'plus', 'minus'))
  return_type = match.arg(return_type, c('num', 'df-seg', 'df-exc'))

  # Summarizing data at a minute level
  data <- data |>
    dplyr::mutate(time = lubridate::floor_date(time, unit = "minute")) |>
    dplyr::group_by(id, time) |>
    dplyr::summarise(hr = mean(hr), .groups = "drop")

  if(from != ""){
    lower_time <- as.POSIXct(from, format = "%Y-%m-%d", tz = tz)
    data <- data |> dplyr::filter(time >= lower_time)
  }
  if(to != ""){
    upper_time <- as.POSIXct(to, format = "%Y-%m-%d", tz = tz)
    data <- data |> dplyr::filter(time < upper_time)
  }

  out <- data |>
    dplyr::filter(!is.na(hr)) |>
    dplyr::group_by(id) |>
    dplyr::do(MAHE = mahe_ma_single(., short_ma = short_ma, long_ma = long_ma, return_type=return_type, direction=direction,
                                    plot = plot, inter_gap = inter_gap, max_gap = max_gap, tz = tz,
                                    title = title, xlab = xlab, ylab = ylab, show_ma = show_ma, show_excursions = show_excursions, static_or_gui='ggplot'))

  # Check if a ggplot or number in list is returned - convert the latter to a number
  if(class(out$MAHE[[1]])[1] == "data.frame") {
    # No processing on DataFrames is needed
    out <- out
  }
  else if(class(out$MAHE[[1]])[1] == "numeric" | is.na(out$MAHE[[1]][1])) {
    out <- out |> dplyr::mutate(MAHE = as.numeric(MAHE))
  }
  # else must be ggplot output
  else {
    out <- ggpubr::ggarrange(plotlist = out$MAHE, nrow = 1, ncol = 1)
  }

  if (is_vector) {
    out$id = NULL
  }

  return(out)
}


#' Calculate the Rate of Change at each time point (ROC)
#'
#' @description
#' The function \code{roc} produces rate of change values in a tibble object.
#'
#' @usage
#' roc(data, timelag = 1, inter_gap = 15, tz = "")
#'
#' @param data DataFrame object with column names "id", "time", "hr"
#' @param timelag Integer indicating the time period (# minutes) over which rate
#' of change is calculated. Default is 1, e.g. rate of change is the change in
#' heart rate over the past 1 minute divided by 1
#' @param inter_gap \strong{Default: 15.} The maximum allowable gap (in minutes) for interpolation. The values will not be interpolated between the glucose measurements that are more than inter_gap minutes apart.
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning
#'
#' @return A tibble object with three columns: subject id, rate of change values, time of observation
#'
#' @export
#'
#' @details
#' A tibble object with a column for subject id, a column for ROC values, and a column for the time associated is
#' returned. A ROC value is returned for each time point for all the subjects. Thus
#' multiple rows are returned for each subject. If the rate of change cannot be
#' calculated, the function will return \code{NA} for that point.
#'
#' The heart rate values are linearly interpolated over a time grid starting at the
#' beginning of the first day of data and ending on the last day of data. Because
#' of this, there may be many \code{NA}s at the beginning and the end of the roc values
#' for each subject. These \code{NA}s are a result of interpolated time points that do
#' not have recorded heart rate values near them because recording had either not
#' yet begun for the day or had already ended.
#'
#' The ROC is calculated as \eqn{\frac{HR(t_i) - HR(t_{i-1})}{t_i - t_{i-1}}}
#' where \eqn{HR_i} is the Heart Rate measurement at time \eqn{t_i} and \eqn{HR_{i-1}} is the
#' Heart Rate measurement at time \eqn{t_{i-1}}. The time difference between the points,
#' \eqn{t_i - t_{i-1}}, is selectable and set at a default of 1 minute.
#'
#' @author Original code in iglu by Elizabeth Chun, David Buchanan
#'
#' @references
#' Clarke et al. (2009) Statistical Tools to Analyze Continuous Glucose Monitor Data,
#' Diabetes
#' \emph{Diabetes Technology and Therapeutics} \strong{11} S45-S54,
#' \doi{10.1089/dia.2008.0138}.
#'
#' @examples
#'
#' data(example_heart_1)
#' roc(example_heart_1)
#'

roc <- function(data, timelag = 1, inter_gap = 15, tz = ""){

  hr = NULL
  rm(list = c("hr"))


  # Summarizing data at a minute level
  data <- data |>
    dplyr::mutate(time = lubridate::floor_date(time, unit = "minute")) |>
    dplyr::group_by(id, time) |>
    dplyr::summarise(hr = mean(hr), .groups = "drop")

  roc_single = function (data, timelag = 5) {
    data_ip = HR2DayByDay(data, dt0 = 1, inter_gap = inter_gap, tz = tz)
    hr_ip_vec = as.vector(t(data_ip[[1]]))
    dt0 = data_ip[[3]]
    unique_days = data_ip[[2]]

    if (timelag < dt0) {
      message(paste("Parameter timelag cannot be less than the data collection frequency: " ,
                    dt0, " , function will be evaluated with timelag = ", dt0, sep = ""))
      timelag <- dt0
    }

    out = data.frame(
      roc = c(rep(NA, timelag/dt0),
            diff(hr_ip_vec, lag = timelag/dt0)/timelag),
      time = seq(
        from = as.POSIXct(paste(unique_days[1], "00:00:00")),
        to = as.POSIXct(paste(unique_days[length(unique_days)], "23:59:00")),
        by = paste(timelag, "mins")
      )
    )
    return(out)
  }


  id = roc = NULL
  rm(list = c("id", "roc"))
  data = check_data_columns(data)

  out = data |>
    dplyr::group_by(id) |>
    dplyr::reframe(
       roc_single(data.frame(id, time, hr), timelag)
    ) |>
    dplyr::ungroup()


  return(out)
}

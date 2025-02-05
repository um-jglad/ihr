check_data_columns =  function(data, id = 'id', time = 'time', hr = 'hr', time_check = FALSE, tz = ""){
  if (is.vector(data)) {
    output = as.double(data)
    output = data.frame(hr = output,
                        id = 1,
                        time = NA_real_)
    attr(output, "is_vector") = TRUE
  } else {
    cols_in = c(id, time, hr) %in% names(data)
    if (!all(cols_in)) {
      s = setdiff(c(id, time, hr), names(data))
      msg = paste0("Columns: ", paste0(s, collapse = ", "),
                   " are not present in the data")
      stop(msg)
    }
    if (time_check) {
      data = check_data_time(data, tz = tz)
    }
    output = data[, c(id, time, hr), drop = FALSE]
    colnames(output) = c("id", "time", "hr")
    attr(output, "is_vector") = FALSE
  }
  return(output)
}

# checks for time format and any repeated/duplicate timestamps
check_data_time <- function(data, tz = ""){

  id = NULL
  hr = NULL
  rm(list = c("id", "hr"))

  check_reps_single <- function(data) {
    hr = NULL
    rm(list = c("hr"))
    check_reps = diff(data$time) == 0

    # no duplicates for this subject, return data and exit
    if (!any(check_reps)) {
      return(data)
    }

    warning(paste0(sum(check_reps), " duplicated timestamps found for subject: ", data$id[1],
                   ". Heart Rate values averaged for those timestamps"))

    # note rle is robust to any number of repeated identical timestamps
    # (i.e. can have 2+ duplicates)
    grouping = rle(as.numeric(data$time))$lengths
    data = data |>
      dplyr::mutate(
        grouping = rep(1:length(grouping), grouping)
      ) |>
      dplyr::group_by(grouping) |>
      dplyr::summarise(
        id = id[1], time = time[1], hr = mean(hr, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::select(id, time, hr)

    # returns data after averaging duplicates
    return(data)
  }

  if (!lubridate::is.POSIXct(data$time)){ # Check if already in date format
    tr = as.character(data$time)
    data$time = as.POSIXct(tr, format='%Y-%m-%d %H:%M:%S', tz = tz)

    if (any(is.na(data$time))) {
      stop('Automatic identification of timezone unsuccessful. Please manually set `tz` parameter or double check if the timezone is correct.')
    }
  }

  out = data |>
    dplyr::group_by(id) |>
    dplyr::reframe(
      check_reps_single(data.frame(id, time, hr))
    ) |>
    dplyr::ungroup()

  return(out)
}

HR2DayByDay <- function(data, dt0 = NULL, inter_gap = 45, tz = ""){

  # complete.cases only works with POSIXct, not POSIXlt, so check for correct time format
  if (!lubridate::is.POSIXct(data$time)){
    tr = as.character(data$time)
    data$time = as.POSIXct(tr, format='%Y-%m-%d %H:%M:%S', tz = tz)
  }
  data = data[complete.cases(data),]

  ns = length(unique(data$id))

  if (ns > 1){
    id = NULL
    rm(list = c("id"))

    first = unique(data$id)[1]
    data = data |> dplyr::filter(id == first)
    warning(paste("Data contains more than 1 subject. Only the first subject with id", first,  "is used for output."))
  }

  ### Get heart rate data
  g = as.numeric(data$hr) # in case it's not

  ### Get time data
  if (lubridate::is.POSIXct(data$time)){ # Check if already in date format
    tr = data$time
  }else{
    tr = as.character(data$time)
    tr = as.POSIXct(tr, format='%Y-%m-%d %H:%M:%S', tz = tz)
    # Check if any NAs from conversion, this happens if wrong time format (e.g. 25:00:00) or wrong time zone which will affect daylight savings time
    if (any(is.na(tr))){
      warning(paste("During time conversion,", sum(is.na(tr)), "values were set to NA. Check the correct time zone specification."))
      g = g[!is.na(tr)]
      tr = tr[!is.na(tr)]
    }
  }

  timeindex = 2:length(tr)
  timediff = difftime(tr[timeindex], tr[timeindex - 1], units = "mins")

  ### Check for time sorting
  if (min(timediff) < 0){
    warning(paste("The times for subject", unique(data$id), "are not in increasing order! The times will be sorted automatically."))
    index = order(tr)
    tr = tr[index]
    g = g[index]
    timediff = difftime(tr[timeindex], tr[timeindex - 1], units = "mins")
  }

  if (any(timediff == 0)){
    warning(paste("Subject", unique(data$id), "has repeated heart rate measuremements at exactly the same times. Only the last one of repeated values is used in calculations."))
    index = which(timediff == 0)
    tr = tr[-index]
    g = g[-index]
    timediff = timediff[-index]
  }

  ### Automatically identify grid width dt0
  if (is.null(dt0)){
    dt0 = as.double(round(median(timediff, na.rm = TRUE)))
  }

  if (dt0 > inter_gap){
    stop(paste("Identified measurements frequency,", dt0, "min, is above the maximal interpolation gap of", inter_gap, "min."))
  }

  ### Check for misaligned grid length dt0 across days, and adjust if needed
  if (1440 %% dt0 != 0){
    if (dt0 > 20){ # Larger grid lengths are set to 20 min
      dt0 = 20
    }else{ # Smaller grid lengths are rounded so that they are divisible by 5 min
      remainder = dt0 %% 5
      if (remainder > 2){
        dt0 = dt0 + 5 - remainder
      }else{
        dt0 = dt0 - remainder
      }
    }
  }

  ### Create ideal grid to interpolate over, from minimal to maximal day
  ndays = ceiling(as.double(difftime(max(tr), min(tr), units = "days")) + 1)
  dti = rep(dt0, ndays * 24 * 60 /dt0)
  dti_cum = cumsum(dti)
  dti_cum = lubridate::minutes(dti_cum)

  # Set up starting point at 00:00am on the first day
  minD = min(tr) # 1st day of measurements
  lubridate::hour(minD) = 0 # set to 00am
  lubridate::minute(minD) = 0 # set to 00:00
  lubridate::second(minD) = 0 # set to 00:00:00

  # Create a set of time points for interpolation
  time_out = minD + dti_cum

  ### Interpolate on the ideal grid
  new <- as.data.frame(stats::approx(x = tr, y = g, xout = time_out))

  ### Adjust to that there is no interpolation between values > inter_gap appart
  ### Thanks to weather_interp function from weathercan R package for inspiration
  ### Adjust so that there is no interpolation between values > inter_gap apart
  # First, determine if such gaps exist
  gap_start = which(timediff > inter_gap)
  ngaps = length(gap_start)
  if (ngaps > 0){
    # If gaps present, need to set to NA for each out
    for (g in 1:ngaps){
      # Identify affected times
      time_covered = (new$x > tr[gap_start[g]]) & (new$x < tr[gap_start[g] + 1])
      # Set those heart rate values to NA
      new$y[time_covered] = NA
    }
  }

  # Next, from ti remove all the ones that are more than dt0 min away from t0
  gd2d = matrix(new$y, nrow = ndays, byrow = TRUE)

  # Assign rownames that correspond to actual dates
  actual_dates = as.Date(minD, tz = tz) + lubridate::days(0:(ndays - 1))

  return(list(gd2d = gd2d, actual_dates = actual_dates, dt0 = dt0))
}

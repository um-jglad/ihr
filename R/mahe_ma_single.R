mahe_ma_single <- function(data,
                           short_ma = 5, long_ma = 32,
                           return_type = c('num', 'df-seg', 'df-exc'),
                           direction = c('avg', 'service', 'max', 'plus', 'minus'),
                           tz = "", inter_gap = 45,
                           max_gap = 180,
                           plot = FALSE, title = NA, xlab = NA, ylab = NA, show_ma = FALSE, show_excursions = TRUE,
                           static_or_gui=c('plotly', 'ggplot')) {

  ## pre-0. Turn all tibbles --> DataFrame
  data = as.data.frame(data)

  ## 0. Calculates MAHE on 1 segment of CGM trace
  mahe_atomic <- function(.data) {
    nmeasurements = list_cross = types = count = crosses = num_extrema = minmax = indexes = s1 = s2 = standardD = heights = nadir2peak = idx = peak_or_nadir = plus_or_minus = first_excursion = max_direction = hr = NULL
    rm(list=c('nmeasurements', 'list_cross', 'types', 'count', 'crosses', 'num_extrema', 'minmax', 'indexes', 's1', 's2', 'standardD', 'heights', 'nadir2peak', 'idx', 'peak_or_nadir', 'plus_or_minus', 'first_excursion', 'hr'))

    if (all(is.na(.data$hr))) {
      return(data.frame(start=utils::head(.data$time, 1), end=utils::tail(.data$time, 1), mahe=NA, plus_or_minus=NA, first_excursion=NA))
    }

    nmeasurements = nrow(.data)

    if (nmeasurements < 7){
      # The number of measurements is too small for MAHE calculation.
      return(data.frame(start=utils::head(.data$time, 1), end=utils::tail(.data$time, 1), mahe=NA, plus_or_minus=NA, first_excursion=NA))
    } else if (nmeasurements < long_ma){
      # The total number of measurements is smaller than the long moving average window
      return(data.frame(start=utils::head(.data$time, 1), end=utils::tail(.data$time, 1), mahe=NA, plus_or_minus=NA, first_excursion=NA))
    } else if (sd(.data$hr, na.rm = TRUE) < 1){
      # There is no glucose variation in the block
      return(data.frame(start=utils::head(.data$time, 1), end=utils::tail(.data$time, 1), mahe=NA, plus_or_minus=NA, first_excursion=NA))
    }

    # 2c. Calculate the moving average values
    .data <- .data |>
      dplyr::mutate(MA_Short = zoo::rollapply(hr, width = short_ma, FUN = mean,
                                              align = 'right', fill = NA, na.rm = TRUE),
                    MA_Long  = zoo::rollapply(hr, width = long_ma, FUN = mean,
                                              align = 'right', fill = NA, na.rm = TRUE)) |>
      # fill in leading NA's from using rolling mean
      dplyr::mutate(MA_Short = replace(MA_Short, 1:short_ma, MA_Short[short_ma]),
                    MA_Long  = replace(MA_Long, 1:long_ma, MA_Long[long_ma]),
                    DELTA_SHORT_LONG = MA_Short - MA_Long)

    # 2d. Create a preallocated list of crossing point ids & type
    idx = as.numeric(rownames(.data))
    types = list2env(list(REL_MIN=0, REL_MAX=1))

    list_cross <- list("id" = rep.int(NA, nmeasurements), "type" = rep.int(NA, nmeasurements))

    # always add 1st point
    list_cross$id[1] <- idx[1]
    list_cross$type[1] <- ifelse(.data$DELTA_SHORT_LONG[1] > 0, types$REL_MAX, types$REL_MIN)
    count = 2

    for(i in 2:length(.data$DELTA_SHORT_LONG)) {
      if(
        !is.na(.data$hr[i]) && !is.na(.data$hr[i-1]) &&
        !is.na(.data$DELTA_SHORT_LONG[i]) && !is.na(.data$DELTA_SHORT_LONG[i-1])
      ) {
        # crossing point if DELTA changes sign or curr DELTA is 0
        if(.data$DELTA_SHORT_LONG[i] * .data$DELTA_SHORT_LONG[i-1] < 0) {
          list_cross$id[count] <- idx[i]
          list_cross$type[count] <- ifelse(.data$DELTA_SHORT_LONG[i] < .data$DELTA_SHORT_LONG[i-1], types$REL_MIN, types$REL_MAX)
          count <- count + 1
        }

        # needed for gaps, where DELTA_SHORT_LONG(i-1 | i-2) = NaN
        # Q: Why "match(list_cross$id[count-1], idx)"? A: get the index from start of .data, since the rowname in the overall DF might not match the index in .data since they may not both start @ 1 (i.e., if multiple gaps)
        else if (!is.na(.data$DELTA_SHORT_LONG[i]) && (.data$DELTA_SHORT_LONG[i] * .data$DELTA_SHORT_LONG[match(list_cross$id[count-1], idx)] < 0)) {
          prev_delta = .data$DELTA_SHORT_LONG[match(list_cross$id[count-1], idx)]

          list_cross$id[count] <- idx[i]
          list_cross$type[count] <- ifelse(.data$DELTA_SHORT_LONG[i] < prev_delta, types$REL_MIN, types$REL_MAX)
          count <- count + 1
        }
      }
    }

    # Add last point to capture excursion at end
    list_cross$id[count+1] <- utils::tail(idx, 1)
    list_cross$type[count+1] <- ifelse(.data$DELTA_SHORT_LONG[nrow(.data)] > 0, types$REL_MAX, types$REL_MIN)

    # Filter for non-na values then combine into a table
    list_cross$id <- list_cross$id[!is.na(list_cross$id)]
    list_cross$type <- list_cross$type[!is.na(list_cross$type)]

    crosses <- do.call(cbind.data.frame, list_cross)

    # 2e. Calculate min and max glucose values from ids and types in crosses + store indexes for plotting later
    num_extrema = nrow(crosses)-1
    minmax <- rep(NA_real_, num_extrema)
    indexes <- rep(NA_real_, num_extrema)

    for(i in 1:num_extrema) {
      s1 <- ifelse(i == 1, crosses[i, 1], indexes[i-1]) # left boundary: prev turning point
      s2 <- crosses[i+1,1]  # right boundary: next crossing point

      if(crosses[i, "type"] == types$REL_MIN) {
        minmax[i] <- min(.data[as.character(s1:s2), ]$hr, na.rm = TRUE)
        indexes[i] <- which.min(.data[as.character(s1:s2), ]$hr)+s1-1
      } else {
        minmax[i] <- max(.data[as.character(s1:s2), ]$hr, na.rm = TRUE)
        indexes[i] <- which.max(.data[as.character(s1:s2), ]$hr)+s1-1
      }
    }

    # excursion elimination
    differences = t(outer(minmax, minmax, `-`))
    standardD <- sd(.data$hr, na.rm = TRUE)
    N = length(minmax)

    # MAHE+
    mahe_plus_heights = c()
    mahe_plus_tp_pairs = list()
    j = prev_j = 1

    while (j <= N) {
      delta = differences[prev_j:j,j] # minmax_j - minmax_i st. j >= i

      max_v = max(delta) # max does left-side accumulation
      i = which.max(delta) + prev_j - 1 # add `prev_j - 1` to adjust for offset since only subsetting prev_j:j

      if (max_v >= standardD) {
        # we've found left excursion, nadir2peak > sd
        for (k in j:N) {
          if (minmax[k] > minmax[j]) {
            j = k
          }

          if (differences[j, k] < -1*standardD || k == N) {
            max_v = minmax[j] - minmax[i]
            # we've found the first large enough right side || unmatched left excursion
            mahe_plus_heights = append(mahe_plus_heights, max_v)
            mahe_plus_tp_pairs[[length(mahe_plus_tp_pairs) + 1]] = c(i, j)

            prev_j = k
            j = k
            break
          }
        }
      } else {
        j = j + 1
      }
    }

    # MAHE-
    mahe_minus_heights = c()
    mahe_minus_tp_pairs = list()
    j = prev_j = 1

    while (j <= N) {
      delta = differences[prev_j:j, j] # minmax_j - minmax_i st. j >= i

      min_v = min(delta) # min does left-side accumulation
      i = which.min(delta) + prev_j - 1

      if (min_v <= -1*standardD) {
        for (k in j:N) {
          if (minmax[k] < minmax[j]) {
            j = k
          }

          if (differences[j, k] > standardD || k == N) {
            min_v = minmax[j] - minmax[i]
            mahe_minus_heights = append(mahe_minus_heights, min_v)
            mahe_minus_tp_pairs[[length(mahe_minus_tp_pairs) + 1]] = c(i, j, k)

            prev_j = j
            j = k
            break
          }
        }
      }
      else {
        j = j + 1
      }
    }

    if (length(mahe_minus_heights) == 0 && length(mahe_plus_heights) == 0) {
      return(data.frame(start=utils::head(.data$time, 1), end=utils::tail(.data$time, 1), mahe=NA, plus_or_minus=NA, first_excursion=NA))
    }

    is_plus_first = ifelse((length(mahe_plus_heights) > 0) && (length(mahe_minus_heights) == 0 || mahe_plus_tp_pairs[[1]][2] <= mahe_minus_tp_pairs[[1]][1]), TRUE, FALSE)
    mahe_plus = data.frame(start=utils::head(.data$time, 1), end=utils::tail(.data$time, 1),  mahe=ifelse(length(mahe_plus_heights), mean(mahe_plus_heights, na.rm = TRUE), NA), plus_or_minus="PLUS", first_excursion=is_plus_first)
    mahe_minus = data.frame(start=utils::head(.data$time, 1), end=utils::tail(.data$time, 1), mahe=ifelse(length(mahe_minus_heights), abs(mean(mahe_minus_heights, na.rm = TRUE)), NA), plus_or_minus="MINUS", first_excursion=!is_plus_first)
    is_plus_max = ifelse(mahe_plus$mahe >= mahe_minus$mahe, TRUE, FALSE)

    # save data for plotting
    pframe = parent.frame()
    pframe$all_data = base::rbind(pframe$all_data, .data)

    for (e in mahe_plus_tp_pairs) {
      pframe$all_tp_indexes = base::rbind(pframe$all_tp_indexes, data.frame(idx=indexes[e[1]], peak_or_nadir="NADIR", plus_or_minus="PLUS", first_excursion=is_plus_first, max_direction=is_plus_max))
      pframe$all_tp_indexes = base::rbind(pframe$all_tp_indexes, data.frame(idx=indexes[e[2]], peak_or_nadir="PEAK", plus_or_minus="PLUS", first_excursion=is_plus_first, max_direction=is_plus_max))
    }

    for (e in mahe_minus_tp_pairs) {
      pframe$all_tp_indexes = base::rbind(pframe$all_tp_indexes, data.frame(idx=indexes[e[1]], peak_or_nadir="PEAK", plus_or_minus="MINUS", first_excursion=!is_plus_first, max_direction=!is_plus_max))
      pframe$all_tp_indexes = base::rbind(pframe$all_tp_indexes, data.frame(idx=indexes[e[2]], peak_or_nadir="NADIR", plus_or_minus="MINUS", first_excursion=!is_plus_first, max_direction=!is_plus_max))
    }

    return(base::rbind(mahe_plus, mahe_minus))
  }

  ## 1. Preprocessing
  MA_Short = MA_Long = DELTA_SHORT_LONG = TP = id = .xmin = .xmax = gap = x = y = xend = yend = hours = weight = idx = peak_or_nadir = plus_or_minus = first_excursion = max_direction = hr = Excursion = timediff = test = avg = NULL
  rm(list = c("MA_Short", "MA_Long", "DELTA_SHORT_LONG", "TP", ".xmin", ".xmax", "id", "gap", "x", "y", "xend", "yend", "hours", "weight", "idx", "peak_or_nadir", "plus_or_minus", "first_excursion", "hr", "Excursion", "timediff", "test", "avg"))

  data = check_data_columns(data)

  # 1.1 Interpolate over uniform grid
  # Note: always interpolate to minute grid
  data_ip <- HR2DayByDay(data, dt0 = 1, inter_gap = inter_gap, tz = tz)
  day_one = lubridate::as_datetime(data_ip$actual_dates[1])
  ndays = length(data_ip$actual_dates)

  # 1.2 Generate grid times by starting from day one and cumulatively summing
  # > replicate dt0 by number of measurements (total minutes/dt0)
  time_ip =  day_one + lubridate::minutes(
    cumsum(
      rep(data_ip$dt0, ndays * 24 * 60 /data_ip$dt0)
    )
  )

  # 1.3 Recalculate short_ma and long_ma because short and long are based on 5 minutes originally
  # > Multiply by 5 to get length in min
  # > Divide by dt0 to get rounded number of measurements that are roughly equal to original short/long ma definition
  short_ma = round(short_ma*5/data_ip$dt0)
  long_ma = round(long_ma*5/data_ip$dt0)

  ## 2. Change to interpolated data (times and glucose)
  # > change data into id, interpolated times, interpolated glucose (t to get rowwise)
  # > drop NA rows before first glucose reading
  # > then drop NA rows after last glucose reading
  # > Label NA glucose as gap (gap = 1)
  data <- data |>
    dplyr::reframe(id = rep(id[1], length(time_ip)), time = time_ip, hr = as.vector(t(data_ip$gd2d))) |>
    dplyr::slice(which(!is.na(hr))[1]:dplyr::n()) |>
    dplyr::slice(1:utils::tail(which(!is.na(.data$hr)), 1)) |>
    dplyr::mutate(gap = dplyr::if_else(is.na(hr), 1, 0))

  runlen <- rle(data$gap)

  # 3. Sanity Checks
  # 3.1 By definition, long > short MA.
  if (short_ma >= long_ma){
    warning("The short moving average window size should be smaller than the long moving average window size for correct MAHE calculation. Swapping automatically.")
    temp = short_ma
    short_ma = long_ma
    long_ma = temp
  }

  if (nrow(data) < 7){
    warning(paste0("The number of measurements (", length(data$id), ") is too small for MAHE calculation. Returning NA."))

    return_type = match.arg(return_type, c('num', 'df-seg', 'df-exc'))

    if (return_type == 'df-exc' | return_type == 'df-seg') {
      return(data.frame(start=utils::head(data$time, 1), end=utils::tail(data$time, 1), mahe=NA, plus_or_minus=NA, first_excursion=NA))
    } else {
      return(NA)
    }
  } else if (nrow(data) < long_ma){
    warning(paste0("The total number of measurements (", length(data$id), ") is smaller than the long moving average. Returning NA."))

    return_type = match.arg(return_type, c('num', 'df-seg', 'df-exc'))

    if (return_type == 'df-seg' | return_type == 'df-exc') {
      return(data.frame(start=utils::head(data$time, 1), end=utils::tail(data$time, 1), mahe=NA, plus_or_minus=NA, first_excursion=NA))
    } else {
      return(NA)
    }
  }

  # 3.2 Are any gaps > 12 hours?
  # > since runlen counts by measurements, compare to number of measurements corresponding to 12hrs (720 mins)
  # > take ceiling and add 1 to make edge cases less likely to give this message
  if (any(runlen$lengths[runlen$values == 1] > (ceiling(720/data_ip$dt0) + 1))) {
    message(paste0("Gap found in data for subject id: ", data$id[1], ", that exceeds 12 hours."))
  }

  # 4. Time Series Segmentation: split gaps > max_gap into separate segments
  is_qualifying_gap = runlen$values == 1 & (runlen$lengths*data_ip$dt0 > max_gap)

  if (!any(is_qualifying_gap)) {
    # there are no gaps
    dfs = list()
    dfs[[1]] <- data
  } else {
    # there are gaps
    end_boundary = cumsum(runlen$lengths)[is_qualifying_gap]
    start_boundary = end_boundary - runlen$lengths[is_qualifying_gap] + 1 # add 1 b/c both sides inclusive

    dfs = list()

    for (i in seq_along(end_boundary)) {
      curr_gap_start = start_boundary[i]
      curr_gap_end = end_boundary[i]

      if (i == 1) {
        # add 1st data sequence
        if (curr_gap_start > 1) {
          dfs[[length(dfs)+1]] <- data[1:(curr_gap_start - 1), ]
        }
      }

      dfs[[length(dfs) + 1]] <- data[curr_gap_start:curr_gap_end, ]

      if (curr_gap_end < nrow(data)) {
        data_start = curr_gap_end + 1
        data_end = ifelse(i == length(end_boundary), nrow(data), start_boundary[i+1] - 1)

        dfs[[length(dfs) + 1]] <- data[data_start:data_end, ]
      }
    }
  }

  # 5. Calculate MAHE on each identified segment
  all_data = c()
  all_tp_indexes = setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("idx", "peak_or_nadir", "plus_or_minus", "first_excursion"))
  return_val = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("start", "end", "mahe", "plus_or_minus", "first_excursion"))

  for (e in dfs) {
    return_val = base::rbind(return_val, mahe_atomic(e))
  }

  # 6. Plotting
  if(plot) {
    # 6.1 Label 'Peaks' and 'Nadirs'
    direction = match.arg(direction, c('avg', 'service', 'max', 'plus', 'minus'))
    static_or_gui = match.arg(static_or_gui, c('ggplot', 'plotly'))

    if (direction == 'avg') {
      tp_indexes <- dplyr::select(all_tp_indexes, idx, peak_or_nadir, plus_or_minus)
    } else if (direction == 'service') {
      tp_indexes <- dplyr::filter(all_tp_indexes, first_excursion==TRUE) |> dplyr::select(idx, peak_or_nadir, plus_or_minus)
    } else if (direction == 'max') {
      tp_indexes <- dplyr::filter(all_tp_indexes, max_direction==TRUE) |> dplyr::select(idx, peak_or_nadir, plus_or_minus)
    } else {
      tp_indexes <- dplyr::filter(all_tp_indexes, plus_or_minus==ifelse(direction == 'plus', "PLUS", "MINUS")) |> dplyr::select(idx, peak_or_nadir, plus_or_minus)
    }

    plotting_data <- data |>
      tibble::rownames_to_column(var = 'idx') |>
      dplyr::mutate(idx = as.numeric(idx)) |>
      dplyr::left_join(tp_indexes, by = 'idx') |>
      dplyr::left_join(dplyr::select(all_data, time, MA_Short, MA_Long), by = 'time')

    # 6.2 Set a default Title
    title <- ifelse(is.na(title), paste("Heart Rate Trace - Subject ", data$id[1]), title)

    # 6.3 Label Gaps in Data
    interval <- data_ip$dt0

    # filter out hr NAs to enable correct gap identification
    plotting_data <- plotting_data |> dplyr::filter(gap != 1)

    # Find the start and end of each gap and merge/sort the two (Must do separately to solve the problem of "back to back" gaps not having correct start & end time)
    .gap_start <- plotting_data |>
      dplyr::filter(abs(difftime(time, dplyr::lead(time), units = "min")) > 2*interval)

    .gap_end <- plotting_data |>
      dplyr::filter(difftime(time, dplyr::lag(time), units = "min") > interval*2)

    .gaps <- rbind(.gap_start, .gap_end) |>
      dplyr::arrange(time) |>
      dplyr::mutate(time = dplyr::if_else(dplyr::row_number() %% 2 == 1, time+interval/2, time-interval/2)) # Offset the gap time slightly so not covering peaks/nadirs in some edge cases

    .gaps <- if(nrow(.gaps) %% 2 == 1) { .gaps[1:(nrow(.gaps)-1), ] } else { .gaps } # make the df even

    .gaps <- data.frame(.xmin = .gaps$time[c(TRUE, FALSE)],
                        .xmax = .gaps$time[c(FALSE, TRUE)])

    # extraneous for ggplot; need for plotly
    .ymin <- min(plotting_data$hr)
    .ymax <- max(plotting_data$hr)

    # 6.4 Generate ggplot
    colors <- c("NADIR" = "blue", "PEAK"="red", "Short MA" = "#009E73", "Long MA" = "#D55E00", "Excursion" = "brown")
    gap_colors <- c("Gap"="purple")

    .p <- ggplot2::ggplot(plotting_data, ggplot2::aes(x=time, y=hr)) +
      ggplot2::ggtitle(title) +
      ggplot2::geom_point() +
      ggplot2::geom_point(data = base::subset(plotting_data, plotting_data$peak_or_nadir != ""), ggplot2::aes(color = peak_or_nadir), fill='white', size=2) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::labs(x=ifelse(!is.na(xlab), xlab, "Time"), y=ifelse(!is.na(ylab), ylab, 'Heart Rate (BPM)')) +
      ggplot2::theme(
        legend.key = ggplot2::element_rect(fill='white'),
        legend.title = ggplot2::element_blank(),
      )

    # add excursion_visualization
    if (show_excursions == TRUE) {
      plus <- plotting_data |> dplyr::filter(plus_or_minus == "PLUS") |> dplyr::arrange(idx)
      minus <- plotting_data |> dplyr::filter(plus_or_minus == "MINUS") |> dplyr::arrange(idx)

      if (nrow(plus) %% 2 != 0) {
        stop("There appears to be an error in ihr::mahe_ma_single's plotting functionality. Please rerun with `show_excursions = FALSE`. Also, please file a bug report on GitHub: https://github.com/irinagain/iglu/issues")
      }

      if (nrow(minus) %% 2 != 0) {
        stop("There appears to be an error in ihr::mahe_ma_single's plotting functionality. Please rerun with `show_excursions = FALSE`. Also, please file a bug report on GitHub: https://github.com/irinagain/iglu/issues")
      }

      arrows = plus |> dplyr::filter(peak_or_nadir == "NADIR") |> dplyr::select(x = time, xend = time, y = hr) |> dplyr::mutate(yend = base::subset(plus, peak_or_nadir == "PEAK")$hr)
      arrows = rbind(arrows, minus |> dplyr::filter(peak_or_nadir == "PEAK") |> dplyr::select(x = time, xend = time, y = hr) |> dplyr::mutate(yend = base::subset(minus, peak_or_nadir == "NADIR")$hr))

      # plotly does not support rendering arrows by default - we use a workaround below (see ~line 487 when we return the plot)
      if (static_or_gui == "ggplot") {
        if (nrow(arrows) > 0) {
          .p <- .p + ggplot2::geom_segment(data = arrows, ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color="Excursion"), arrow = grid::arrow(length = grid::unit(0.2, "cm")))
        }
      }
    }

    # add segment boundaries
    segment_boundaries <- return_val |> dplyr::filter(!is.na(plus_or_minus))

    for (e in segment_boundaries$start) {
      .p <- .p + ggplot2::geom_vline(xintercept = e, color="black", linetype = "solid", show.legend = TRUE)
    }

    for (e in segment_boundaries$end) {
      .p <- .p + ggplot2::geom_vline(xintercept = e, color="black", linetype = "dotted", show.legend = TRUE)
    }

    if (nrow(.gaps)) {
      .p <- .p + ggplot2::geom_rect(data=.gaps, ggplot2::aes(
        xmin=.xmin,
        xmax=.xmax,
        ymin= ifelse(static_or_gui == "plotly", .ymin, -Inf),
        ymax= ifelse(static_or_gui == "plotly", .ymax, Inf),
        fill = 'Gap'
      ),
      alpha=0.2, inherit.aes = FALSE, show.legend = T, na.rm = TRUE) +
        ggplot2::scale_fill_manual(values=gap_colors)
    }

    if(show_ma == TRUE) {
      .p <- .p + ggplot2::geom_line(ggplot2::aes(y = MA_Short, group = 1, color="Short MA")) + #Exclude by default because ggplot becomes too crowded
        ggplot2::geom_line(ggplot2::aes(y = MA_Long, group = 2, color="Long MA"))
    }

    # 4d. Return plot
    if (static_or_gui == 'plotly') {
      .p <- plotly::ggplotly(.p)

      if (show_excursions == TRUE && nrow(arrows) > 0) {
        t <- .p |> plotly::add_annotations(
          text = "",
          showarrow = TRUE,
          arrowcolor="brown",
          arrowhead = 1,
          arrowsize = 1,
          x = as.numeric(arrows$x),
          y = arrows$yend,
          ax = as.numeric(arrows$xend),
          ay = arrows$y,
          xref="x",
          axref="x",
          yref="y",
          ayref="y"
        )

        return(t)
      }

      return(.p)
    } else {
      return(.p)
    }

  } else {
    return_type = match.arg(return_type, c('num', 'df-seg', 'df-exc'))
    direction = match.arg(direction, c('avg', 'service', 'max', 'plus', 'minus'))


    if (return_type == 'df-seg') {
      return(return_val)
    }

    if(return_type == 'df-exc'){

      tp_indexes <- dplyr::select(all_tp_indexes, idx, peak_or_nadir, plus_or_minus)

      exc_data <- data |>
        tibble::rownames_to_column(var = 'idx') |>
        dplyr::mutate(idx = as.numeric(idx)) |>
        dplyr::left_join(tp_indexes, by = 'idx') |>
        dplyr::left_join(dplyr::select(all_data, time, MA_Short, MA_Long), by = 'time')

      exc_data <- exc_data |>
        dplyr::filter(!is.na(peak_or_nadir)) |>
        dplyr::arrange(time) |>
        dplyr::mutate(Excursion = abs(hr - dplyr::lead(hr)),
               timediff = abs(time - dplyr::lead(time))) |>
        dplyr::select(id, time, peak_or_nadir, hr, Excursion, timediff) |>
        dplyr::filter(Excursion != 0 | is.na(Excursion))

      # Any excursion/hr values during gaps are invalid excursion values
      exc_data <- exc_data |>
        dplyr::mutate(Excursion = dplyr::if_else(timediff > (60 * max_gap), NA, Excursion))

      dummy <- data.frame(id = exc_data$id[1], time = NA, peak_or_nadir = "NADIR", hr = NA,
                          Excursion = NA, timediff = NA)

      # Format in Nadir-Peak-Nadir format
      if(exc_data$peak_or_nadir[1] == "PEAK"){
        # if the first observation doesn't start at nadir we input a dummy nadir point
        exc_data <- rbind(dummy, exc_data)
      }
      if(exc_data$peak_or_nadir[nrow(exc_data)] == "PEAK"){
        # if the last observation isn't a nadir we input a dummy nadir point
        exc_data <- rbind(exc_data, dummy)
      }

      gap_idx <- which(exc_data$timediff >= (60 * max_gap))

      # For where a gap
      if(length(gap_idx) > 0){
        # j represents added number of gaps
        j <- 0
        for(i in gap_idx){
          # We either end current excursion or start next on a peak
          if(exc_data$peak_or_nadir[i + j + 1] == "PEAK" | exc_data$peak_or_nadir[i + j] == "PEAK"){
            exc_data <- exc_data |>
              tibble::add_row(dummy, .after = i + j)
            j <- j + 1
          }
        }
      }

      # Recognize location of every "Nadir-Peak-Nadir" triples, these are recognized excursions
      exc_idx <- which(exc_data$peak_or_nadir == "NADIR" &
                         dplyr::lead(exc_data$peak_or_nadir, 1) == "PEAK" &
                         dplyr::lead(exc_data$peak_or_nadir, 2) == "NADIR")

      exc_data <- exc_data |>
        dplyr::mutate(start = NA,
               end = NA,
               plus = NA,
               minus = NA,
               avg = NA,
               start_nadir = NA,
               peak = NA,
               end_nadir = NA)

      for(i in exc_idx){
        exc_data$start[i] <- exc_data$time[i]
        exc_data$end[i] <- exc_data$time[i + 2]
        exc_data$plus[i] <- exc_data$Excursion[i]
        exc_data$minus[i] <- exc_data$Excursion[i + 1]
        exc_data$avg[i] <- ((exc_data$Excursion[i] + exc_data$Excursion[i + 1]) / 2)
        exc_data$start_nadir[i] <- exc_data$hr[i]
        exc_data$peak[i] <- exc_data$hr[i + 1]
        exc_data$end_nadir[i] <- exc_data$hr[i + 2]
      }

      exc_data$start <- format(as.POSIXct(exc_data$start, origin = "1970-01-01", tz = "UTC"),
                               "%Y-%m-%d %H:%M:%S")
      exc_data$end <- format(as.POSIXct(exc_data$end, origin = "1970-01-01", tz = "UTC"),
                             "%Y-%m-%d %H:%M:%S")

      exc_data <- exc_data[exc_idx, ] |>
        dplyr::select(start, end, plus, minus, avg, start_nadir, peak, end_nadir)

      return(exc_data)
    }


    # filter by various options
    if (direction == 'plus') {
      res = return_val |> dplyr::filter(plus_or_minus == 'PLUS')
    } else if (direction == 'minus') {
      res = return_val |> dplyr::filter(plus_or_minus == 'MINUS')
    } else if (direction == 'avg') {
      res = return_val |> dplyr::filter(!is.na(mahe))
    } else if (direction == 'max') {
      res = return_val |> dplyr::group_by(start, end) |> dplyr::filter(mahe == max(mahe)) |> dplyr::ungroup() # Source: https://stackoverflow.com/a/24237538/21711054
    } else {
      res = return_val |> dplyr::filter(first_excursion == TRUE)
    }

    if (nrow(res) == 0) {
      return(NA)
    }

    res = res |>
      dplyr::mutate(hours = end - start) |>
      dplyr::mutate(weight = as.numeric(hours/as.numeric(sum(hours)))) |> # weight each segment's mahe value contribution by segment length
      dplyr::summarize(sum(mahe*weight))

    return(as.numeric(res))
  }
}

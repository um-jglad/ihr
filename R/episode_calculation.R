### algorithm description
# (1) apply the following to each subject (episode_single)
#       (a) interpolate to create equidistant grid
#       (b) split into contiguous segments based on gaps
#       (c) classify events in each segment (event_class)
#       (d) summarize episodes (episode_summary)
# (a) event_class: label events of each type for each segment
#       (a) must be >= duration (function input is # idx to get # minutes)
#       (b) ends at >= dur_length (function input is top level dur_length/dt0)
# (b) episode_summary: calculate summary statistics
#       (a) return for each type of episode: # episodes, mean duration, mean hr value

event_class = function(data, level_type, threshold, event_duration, end_duration) {

  event = level = pos_start = pos_end = NULL
  rm(list = c("event", "level", "pos_start", "pos_end"))

  # if low use < else if high use >
  if (level_type == 'low') {

    annotated = data
    annotated$level = annotated$hr < threshold

  } else if (level_type == 'high') {

    annotated = data
    annotated$level = annotated$hr > threshold

  }

  level_rle = rle(annotated$level)$lengths
  annotated$event = rep(1:length(level_rle), level_rle)
  annotated = annotated |>
    dplyr::group_by(event) |>
    dplyr::mutate(
      # possibly event; where duration is met
      pos_start = level[1] && (dplyr::n() >= event_duration),
      # if possible event, add start on first index of event
      start = c(ifelse(pos_start[1], "start", NA_character_),
                rep(NA_character_, dplyr::n()-1)),
      # add possible ends (always need to check for end duration)
      pos_end = !level[1] && (dplyr::n() >= end_duration),
      end = c(ifelse(pos_end[1], "end", NA_character_),
              rep(NA_character_, dplyr::n()-1))
    )

  ### for each possible end find the matching start
  starts = which(!is.na(annotated$start))
  # add 0 for initial "previous end" to be 0 and length + 1 for last point to be end
  ends = c(0, which(!is.na(annotated$end)), nrow(data) + 1)
  # *_ref are the matched pairs
  pairs = data.frame(
    starts_ref = starts,
    ends_ref = sapply(starts, function(x) {min(ends[ends > x]) - 1} )
  )

  # no episodes in this segment
  if (nrow(pairs) == 0) {
    output = rep(0, nrow(data))
    return (output)
  } else {

    # remove intervening 'starts'
    pairs = pairs[!duplicated(pairs$ends_ref), ]

    # vectorize seq function to create seqs for contiguous events
    vseq = Vectorize(seq.default, c("from", "to"))
    event_idx = unlist(vseq(pairs$start, pairs$end))
    event_label = rep(1:nrow(pairs), (pairs$end - pairs$start)+1)

    output = rep(0, nrow(data))
    output[event_idx] = event_label

    # return event vector with length = nrow(data); not event = 0, event given group label
    # label is unique per subject-segment
    return(output)
  }
}

# label exlusive events (1 vs 0 if not)
lv1_excl = function (data) {

  segment = lv1 = lv2 = NULL
  rm(list = c("segment", "lv1", "lv2"))

  colnames(data) = c("id", "time", "hr", "segment", "lv1", "lv2")

  out = data |>
    dplyr::group_by(segment, lv1) |>
    dplyr::mutate(
      # if goes to lv2 = 0, not exclusive, else keep lv1 label (unique within subj-seg)
      excl = ifelse(any(lv2 > 0), 0, lv1)
    ) |>
    dplyr::ungroup()

  return(out$excl)
}

# function to calculate summary statistics for each type of episode
episode_summary = function (data, dt0) {

  event = segment = hr = NULL
  rm(list = c("event", "segment", "hr"))

  episode_summary_helper = function (data, level_label, dt0) {
    hr = NULL
    rm(list = c("hr"))
    data = data[, c(1:4, which(colnames(data) == level_label))]
    colnames(data) = c("id", "time", "hr", "segment", "event")


    # if none of this event exists, return 0 or NA and exit
    if (all(data$event == 0)) {
      output = c(0, 0, NA, 0)
      return(output)
    }

    # calculate summary metrics
    events = data[data$event != 0, 3:5]
    data_sum = events |>
      dplyr::group_by(segment, event) |>
      dplyr::summarise(
        # for each event, pull duration in minutes and mean glucose
        event_duration = dplyr::n() * dt0,
        event_hr = mean(hr),
        .groups = "drop"
      )

    # eps/recording time, not incl gaps
    avg_ep_per_day = nrow(data_sum)/(nrow(data) * dt0 / 60 / 24)
    avg_ep_duration = mean(data_sum$event_duration)
    avg_ep_hr = mean(data_sum$event_hr)
    total_episodes = nrow(data_sum)

    output = c(avg_ep_per_day, avg_ep_duration, avg_ep_hr, total_episodes)
    return(output)
  }

  labels = c("lv1_low", "lv2_low", "ext_low", "lv1_high", "lv2_high",
             "lv1_low_excl", "lv1_high_excl")
  out_list = sapply(labels, function(x) episode_summary_helper(data, x, dt0))

  output = data.frame(
    type = c(rep("low", 3), rep("high", 2), "low", "high"),
    level = c("lv1", 'lv2', 'extended', 'lv1', 'lv2', 'lv1_excl', 'lv1_excl'), # lv1/lv2/extended
    avg_ep_per_day = out_list[1, ],
    avg_ep_duration = out_list[2, ],
    avg_ep_hr = out_list[3, ],
    total_episodes = out_list[4, ]
  )

  return(output)
}

# classify episodes for all segments for one subject
episode_single = function(data, lv1_low, lv2_low, lv1_high, lv2_high,
                          dur_length, end_length, return_data, dt0, inter_gap, tz) {

  id = segment = hr = NULL
  rm(list = c("id", "segment", "hr"))

  ### interpolate and segment to deal with gaps and uneven grid
  data_ip <- HR2DayByDay(data, dt0 = 5, inter_gap = inter_gap, tz = tz)
  dt0 = data_ip[[3]]

  # find first day and number of days
  day_one = as.POSIXct(data_ip[[2]][1], tz = tz)
  ndays = length(data_ip[[2]])
  # generate grid times by starting from day one and cumulatively summing
  time_ip =  day_one + cumsum(
    # replicate dt0 by number of measurements (total minutes/dt0)
    rep(dt0 * 60, ndays * 24 * 60 /dt0))

  # interpolated dataframe
  new_data = data.frame(
    id = data$id[1],
    time = time_ip,
    # t to get rowwise vector of a matrix
    hr = as.vector(t(data_ip[[1]]))
  )


  if (dur_length %% dt0 != 0) {
    warning("Episode duration is not a multiple of recording interval, smallest multiple
            that is greater than input dur_length chosen for computation")
  }

  dur_idx = ceiling(dur_length/dt0)
  end_idx = ceiling(end_length/dt0)

  ### create segment indices to split by NA gaps then remove gaps
  na_idx = is.na(new_data$hr)
  segment_rle = rle(na_idx)$lengths
  segment_data = new_data
  segment_data$segment = rep(1:length(segment_rle), segment_rle)
  segment_data = segment_data[!is.na(segment_data$hr), ]

  # classify events for each segment
  ep_per_seg = segment_data |>
    dplyr::group_by(segment) |>
    dplyr::mutate(
      lv1_low = event_class(data.frame(id, time, hr), "low", lv1_low, dur_idx, end_idx),
      lv2_low = event_class(data.frame(id, time, hr), "low", lv2_low, dur_idx, end_idx),
      lv1_high = event_class(data.frame(id, time, hr), "high", lv1_high, dur_idx, end_idx),
      lv2_high = event_class(data.frame(id, time, hr), "high",lv2_high, dur_idx, end_idx),
      # extended low heart rate defined as >120 minutes of low heart rate
      ext_low = event_class(data.frame(id, time, hr), "low", !!lv1_low, 120/dt0 + 1, end_idx)
    ) |>
    dplyr::mutate(
      # if both levels will be 0, else will be 1
      lv1_low_excl = lv1_excl(data.frame(id, time, hr, segment, lv1_low, lv2_low)),
      lv1_high_excl = lv1_excl(data.frame(id, time, hr, segment, lv1_high, lv2_high)),
    )

  # if return data, then return without calculating summaries
  if (return_data) {
    return(ep_per_seg)
  }

  # calculate summary statistic
  output = episode_summary(ep_per_seg, dt0)

  return(output)
}


#' Calculates low/high heartrate episodes with summary statistics
#' @name episode_calculation
#'
#' @description
#' The function determines episodes or events, calculates summary statistics,
#' and optionally returns data with episode label columns added
#'
#'
#' @param data DataFrame object with column names "id", "time", "hr"
#' @param lv1_low Numeric value specifying a low heart rate threshold for level 1
#' @param lv2_low Numeric value specifying a low heart rate threshold for level 2
#' @param lv1_high Numeric value specifying a high heart rate threshold for level 1
#' @param lv2_high Numeric value specifying a high heart rate threshold for level 2
#' @param dur_length Numeric value specifying the minimum duration in minutes to be
#' considered an episode. Note dur_length should be a multiple of the data recording
#' interval otherwise the function will round up to the nearest multiple. Default
#' is 15 minutes to match consensus.
#' @param end_length Numeric value specifying the minimum duration in minutes of
#' improved heart rate for an episode to end. Default is equal to dur_length to match consensus.
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning
#' @param return_data Boolean indicating whether to also return data with episode labels.
#' Defaults to FALSE which means only episode summary statistics will be returned
#' @param inter_gap The maximum allowable gap (in minutes) for interpolation.
#' @param dt0 Grid Length
#'
#'
#' @return If return_data is FALSE, a single dataframe with columns:
#' \item{id}{Subject id}
#' \item{type}{Type of episode - either low or high}
#' \item{level}{Level of episode - one of lv1, lv2, extended, lv1_excl}
#' \item{avg_ep_per_day}{Average number of episodes per day calculated as
#' (total # episodes)/(recording time in days (24hrs))}
#' \item{avg_ep_duration}{Average duration of episodes in minutes}
#' \item{avg_ep_hr}{Average heart rate in the episode in minutes}
#' \item{total_episodes}{Total number of episodes in the subject's heart rate trace}
#'
#' If return_data is TRUE, returns a list where the first entry is the episode summary dataframe
#' (see above) and the second entry is the input data with episode labels added. Note
#' the data returned here has been interpolated using the HR2DayByDay() function.
#' Mostly for use with epicalc_profile function. Format of the second list entry is:
#' \item{id}{Subject id}
#' \item{time}{Interpolated timestamps}
#' \item{hr}{heart rate in minutes}
#'
#' @details Note we have classified lv2 as a subset of lv1 since we find the consensus to be
#' slightly ambiguous. For lv1 exclusive of lv2, please see lv1_excl which summarises
#' episodes that were exclusively lv1 and did not cross the lv2 threshold. Also note,
#' low extended refers to episodes that are >120 consecutive minutes below lv1 low
#' and ends with at least 15 minutes of normal heart rate.
#'
#' @references
#' “What Your Heart Rate Is Telling You.” Harvard Health, 13 June 2023,
#'  www.health.harvard.edu/heart-health/what-your-heart-rate-is-telling-you.
#'
#' @export
#'
#'
#' @examples episode_calculation(example_heart_1, lv1_low = 100, lv1_high = 120)
#'


episode_calculation = function(data, lv1_low = 60,lv2_low = 55, lv1_high= 85, lv2_high = 100,
                               dur_length = 15, end_length = 15, return_data = FALSE,
                               dt0 = NULL, inter_gap = 45, tz = "") {

  id = hr = NULL
  rm(list = c("id", "hr"))


  if (dur_length > inter_gap) {
    warning("Interpolation gap parameter less than episode duration, data gaps may cause
            incorrect computation")
  }
  out <- data |>
    dplyr::group_by(id) |>
    # calculate episodes for each subject and return summary stats not data
    dplyr::reframe(episode_single(data.frame(id, time, hr), lv1_low = lv1_low, lv2_low = lv2_low,
                                  lv1_high = lv1_high, lv2_high = lv2_high, return_data = FALSE,
                                  dur_length = dur_length,end_length = end_length, dt0 = dt0,
                                  inter_gap = inter_gap, tz = tz)) |>
    dplyr::ungroup()


  if (return_data) {
    ep_data = data |>
      dplyr::group_by(id) |>
      # return labeled episode data
      dplyr::reframe(episode_single(data.frame(id, time, hr), lv1_low = lv1_low, lv2_low = lv2_low,
                                    lv1_high = lv1_high, lv2_high = lv2_high, return_data = TRUE,
                                    dur_length = dur_length,end_length = end_length, dt0 = dt0,
                                    inter_gap = inter_gap, tz = tz)) |>
      dplyr::ungroup()
    output = list(episodes = out, data = ep_data)
    return(output)
  }

  return(out)

}

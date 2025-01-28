#' Calculate Mean Amplitude of Heartrate Excursions
#'
#' @description
#' The function calculates MAHE values (a metric inspired by MAGE from glucose monitoring) and optionally returns a heart trace plot
#'
#' @param data DataFrame object with column names "id", "time", "hr"
#'

mahe <- function(data,
                 short_ma = 5, long_ma = 32,
                 return_type = c('num', 'df'),
                 direction = c('avg', 'service', 'max', 'plus', 'minus'),
                 tz = "", inter_gap = 45,
                 max_gap = 180,
                 plot = FALSE, title = NA, xlab = NA, ylab = NA, show_ma = FALSE, show_excursions = TRUE) {

  direction = match.arg(direction, c('avg', 'service', 'max', 'plus', 'minus'))

  return(mahe_ma(data, short_ma = short_ma, long_ma = long_ma, return_type=return_type, direction=direction,
                 plot = plot, inter_gap = inter_gap, max_gap = max_gap, tz = tz,
                 title = title, xlab = xlab, ylab = ylab, show_ma = show_ma, show_excursions=show_excursions))
}


mahe_ma <- function(data,
                    short_ma = 5, long_ma = 32,
                    return_type = c('num', 'df'),
                    direction = c('avg', 'service', 'max', 'plus', 'minus'),
                    inter_gap = 45, tz = "",
                    max_gap = 180,
                    plot = FALSE, title = NA, xlab = NA, ylab = NA, show_ma = FALSE, show_excursions=TRUE) {
  id = . = MAHE = NULL
  rm(list = c("id", ".", "MAHE"))

  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")
  direction = match.arg(direction, c('avg', 'service', 'max', 'plus', 'minus'))

  out <- data %>%
    dplyr::filter(!is.na(hr)) %>%
    dplyr::group_by(id) %>%
    dplyr::do(MAHE = mahe_ma_single(., short_ma = short_ma, long_ma = long_ma, return_type=return_type, direction=direction,
                                    plot = plot, inter_gap = inter_gap, max_gap = max_gap, tz = tz,
                                    title = title, xlab = xlab, ylab = ylab, show_ma = show_ma, show_excursions = show_excursions, static_or_gui='ggplot'))

  # Check if a ggplot or number in list is returned - convert the latter to a number
  if(class(out$MAHE[[1]])[1] == "numeric" | is.na(out$MAHE[[1]][1])) {
    out <- out %>% dplyr::mutate(MAHE = as.numeric(MAHE))
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

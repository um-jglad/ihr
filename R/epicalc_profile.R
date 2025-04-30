#' Display Episode Calculation statistics for selected subject
#' @name epicalc_profile
#'
#' @inheritParams episode_calculation
#' @param subject String corresponding to subject id
#'
#' @return A plot displaying (1) the statistics for the episodes and (2) the episodes colored by level.
#'
#' @export
#'
#' @seealso episode_calculation()
#'
#' @examples
#' epicalc_profile(example_heart_1)
#'
epicalc_profile <- function(data,
                            dur_length = 15, end_length = 15, subject = NULL,
                            dt0 = 1, inter_gap = 15, tz = "") {

  id = hr = event = Moderate = Sedentary = Vigorous = num_levels = NULL
  rm(list = c("id", "hr", "event", "Moderate", "Sedentary", "Vigorous", "num_levels"))

  if (!is.null(subject)) {
    data = data[data$id == subject, ]
  }
  data <- data |>
    dplyr::mutate(time = lubridate::floor_date(time, unit = "minute")) |>
    dplyr::group_by(id, time) |>
    dplyr::summarise(hr = mean(hr), .groups = "drop")

  ns = length(unique(data$id))
  if (ns > 1) {
    subject = unique(data$id)[1]
    warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
    data = data[data$id == subject, ]
  }

  episodes = episode_calculation(data,
                                 return_data = TRUE,
                                 dur_length = dur_length,
                                 end_length = end_length,
                                 dt0 = dt0,
                                 inter_gap = inter_gap,
                                 tz = tz)

  ep_summary = episodes[[1]]
  ep_data = episodes[[2]]
  stages = c("Sedentary", "Moderate", "Vigorous")


  tableStat = data.frame(matrix(ncol = 4, nrow = 6))
  colnames(tableStat) <- c("", stages)
  HRR_info <- calculate_HRR(data)
  if (is.null(HRR_info)) {
    return(NULL)
  }
  RHR <- HRR_info$RHR[1]
  maxHR <- HRR_info$max_hr[1]
  HRR <- maxHR - RHR
  tableStat[1, ] <- c("Thresholds",
                      paste0("<", round(0.2 * HRR + RHR, 1) - 0.1, " BPM"),
                      paste0(">=", round(0.4 * HRR + RHR, 1), " BPM"),
                      paste0(">=", round(0.6 * HRR + RHR, 1), " BPM"))

  tableStat[2, ] <- c("Avg Episodes/Day",
                      format(round(ep_summary$avg_ep_per_day[1], 2), nsmall = 2),
                      format(round(ep_summary$avg_ep_per_day[2], 2), nsmall = 2),
                      format(round(ep_summary$avg_ep_per_day[3], 2), nsmall = 2))

  tableStat[3, ] <- c("Mean Duration",
                      paste0(format(round(ep_summary$avg_ep_duration[1], 2), nsmall = 2), " min"),
                      paste0(format(round(ep_summary$avg_ep_duration[2], 2), nsmall = 2), " min"),
                      paste0(format(round(ep_summary$avg_ep_duration[3], 2), nsmall = 2), " min"))

  tableStat[4, ] <- c("Mean Heart Rate",
                      paste0(format(round(ep_summary$avg_ep_hr[1], 2), nsmall = 2), " BPM"),
                      paste0(format(round(ep_summary$avg_ep_hr[2], 2), nsmall = 2), " BPM"),
                      paste0(format(round(ep_summary$avg_ep_hr[3], 2), nsmall = 2), " BPM"))

  tableStat[5, ] <- c("Total Episodes",
                      format(round(ep_summary$total_episodes[1], 2), nsmall = 2),
                      format(round(ep_summary$total_episodes[2], 2), nsmall = 2),
                      format(round(ep_summary$total_episodes[3], 2), nsmall = 2))

  tableStat[6, ] <- ""

  mytheme <- gridExtra::ttheme_minimal(base_size = 10, padding = grid::unit(c(4,2),"mm"))
  t1 <- gridExtra::tableGrob(tableStat, rows = NULL, theme = mytheme)
  t1 <- gtable::gtable_add_grob(t1,
                                grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 5)),
                                t = 1, b = nrow(tableStat), l = 1, r = ncol(tableStat))

  separators <- replicate(ncol(t1) - 2,
                          grid::segmentsGrob(x1 = grid::unit(0, "npc"), gp=grid::gpar(lty=2)),
                          simplify=FALSE)

  t1 <- gtable::gtable_add_grob(t1, grobs = separators,
                                t = 2, b = nrow(t1), l = seq_len(ncol(t1)-2)+2)
  padding <- grid::unit(0.5,"line")

  title <- grid::textGrob(paste0("Episode Metrics - ", data$id[1]), gp=grid::gpar(fontsize=18), x=0, hjust=0)
  footnote <- grid::textGrob(paste0("An episode is >= ", dur_length, " continuous minutes"), x=1, hjust=1,
                             gp=grid::gpar(fontface="italic", fontsize = 8))

  t1 <- gtable::gtable_add_rows(t1, heights = grid::grobHeight(title) + padding, pos = 0)
  t1 <- gtable::gtable_add_rows(t1, heights = grid::grobHeight(footnote)+ padding)
  t1 <- gtable::gtable_add_grob(t1, list(title, footnote),
                                t = c(1, nrow(t1)), l = c(1,2), r = ncol(t1))


  ep_data_long = ep_data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      num_levels = sum(c(Sedentary != 0, Moderate != 0, Vigorous != 0)),
      class = ifelse(
        # either no types - normal, one type - keep nonzero, subset - choose lv2
        num_levels == 0, "Normal",
        ifelse(num_levels == 1,
               stages[which(c(Sedentary != 0, Moderate != 0, Vigorous != 0))],
               c("Vigorous")[which(c(Vigorous != 0))])
      ),
      class = factor(class, levels = c("Sedentary", "Normal", "Moderate", "Vigorous"))
    )
  stage_colors <- c("#0073C2","#48BA3C", "#F9B500","#8E1B1B")

  p1 <- ggplot2::ggplot(ep_data_long, ggplot2::aes(x = time, y = hr, color = class)) +
    ggplot2::geom_point(alpha = 0.7, size = 1) +
    ggplot2::scale_color_manual(values = stage_colors, drop = FALSE,
                                labels = c("Sedentary", "Normal", "Moderate", "Vigorous")) +
    ggplot2::labs(x = "Time", y = "Heart Rate (BPM)", color = "Stage") +
    ggplot2::theme_minimal()

  pFinal <- (
    patchwork::wrap_elements(t1) + patchwork::plot_layout()) / p1

  return(pFinal)
}

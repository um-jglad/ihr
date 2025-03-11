above_percent <- function(data, targets_above = c(60, 85, 100)){

  x = target_val = id = NULL
  rm(list = c("id", "target_val", "x"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")
  targets_above = as.double(targets_above)
  out = lapply(
    targets_above,
    function(target_val) {
      data = data |>
        dplyr::group_by(id) |>
        dplyr::summarise(x = mean(hr > target_val, na.rm = TRUE) * 100) |>
        dplyr::mutate(target_val = paste0("above_", target_val))
      data
    })
  out = dplyr::bind_rows(out)
  out = tidyr::spread(data = out, key = target_val, value = x)
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}

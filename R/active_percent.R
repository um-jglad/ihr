active_percent <- function(data) {
  # Ensure necessary columns exist
  if (!all(c("Id", "Time") %in% colnames(data))) {
    stop("The dataset must contain 'Id' and 'Time' columns.")
  }
  
  # Convert Time column to Date-Time format
  data <- data %>%
    mutate(Time = as.POSIXct(Time, format = "%m/%d/%Y %I:%M:%S %p"),
           Time_minute = floor_date(Time, "minute")) 
  
  # Group by Id and compute active percentage and measurement period length
  active_data <- data %>%
    group_by(Id) %>%
    summarize(
      total_minutes = as.numeric(difftime(max(Time), min(Time), units = "mins")), 
      unique_minutes = n_distinct(Time_minute),  
      active_percent = (unique_minutes / total_minutes) * 100  
    )
  
  return(active_data)
}

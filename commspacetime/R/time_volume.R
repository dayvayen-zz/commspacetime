#' Time-volume function
#'
#' The last function in the series of modeling commmunication space-time.
#' 
#' @param duration How long each vocalization lasts for (seconds)
#' @param rate How often each vocalization is produced per minute
#' @param radius Calculated from the radius_calc.r function--radius of the sphere
#' @param sphere If the vocalization is produced above the ground, this is set to true. If it's produced on the ground, it's set to false.
#' @export
#' @examples
#' time_volume(0.02, 37, 19, sphere = F)

time_volume <- function(duration, rate, radius, sphere = T) {
  total_time <- duration*rate
  if(sphere == T) {
    vol <- (4/3)*pi*radius^3
  } else {
    vol <- (2/3)*pi*radius^3
  }
  time_vol <- vol*total_time
  list1 <- list(time_vol, vol)
  names(list1) <- c("time.volume", "volume")
  return(list1)
}
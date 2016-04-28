#' Radius calculation
#' 
#' This function calculates the radius of the sphere or hemisphere of 
#' communication. This can then be used in future calculations.
#' It requires an initialized vector to work with--the upper limit of this
#' can be determined by your species' vocalization propoerties. 
#' @param signal The source level (dB re 20 microPa) of the vocalization.
#' @param noise The ambient noise level (dB re 20 microPa) of the place where the animal is vocalizing.
#' @param r_vector The initialized vector of possible radius values.
#' @param absorption The atmospheric absorption calculated by the absorption.R function. Set to 1 unless otherwise specified.
#' @param perception The detection threshold for your species in question, if known. By default, this is set to zero, which indicates the best possible scenario in which any signal can be perceived if it is above the level of background noise.
#' @export
#' @examples
#' find_r(90, 65, r_vector, absorption = 1, perception)



find_r <- function(signal, noise, r_vector, absorption = 1, perception = 0) {
  attenuation = signal - 20*log10(r_vector) - absorption*r_vector
  r <- r_vector[which.min(abs(noise - attenuation))]
  return(r)
}

r_vector <- seq(0, 3000, 0.01)

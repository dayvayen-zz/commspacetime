#' Absorption function
#'
#' If the vocalizations are far enough away that atmospheric absorption
#' might become an issue, use this function to calculate it to increase
#' the accuracy of your radius calculation. 
#' @param f Frequency of vocalization (Hz)
#' @param temp Temperature 
#' @param C If your temperature is in Celsius, mark TRUE. If Fahrenheit, mark FALSE.
#' @param rh Relative humidity (percentage)
#' @param p Barometric pressure. Set to 101.325 as a default. 
#' @return Atmospheric absorption--this gets multiplied by the radius in that calculation.
#' @export
#' @examples
#' find_absorption(f = 2250, temp = 10, C = T, rh = 75, p)

find_absorption <- function(f, temp, C = TRUE, rh, p = 101.325) {
  if(C == T) {
    tempk = temp + 273.15
  } else {
    tempk = (temp + 459.67) * (5/9)
  }
  psat <- 101.325 * 10^(-6.8346*((273.16/tempk)^1.261) + 4.6151)
h <- rh * (psat/101.325)
frO <- 24 + 4.04*10^4*h*((.02+h)/.391 +h)
x <- 1/(10*log((exp(1))^2))
frN <- (tempk/293.15)^(1/2)*(9+280*h*exp(-4.17*((tempk/293.15)^(-1/3)-1)))
t0 <- 293.15
t01 <- 273.16
z <- 0.1068 * exp(-3352/tempk) * (frN + f^2/frN)^-1
y = (tempk/t0)^(-5/2) * (0.01275 * exp(-2239.1/tempk) * 
                           (frO+f^2/frO)^-1 + z)
a = 8.686 * f^2 * ((1.84 * 10^-11 * (tempk/t0)^(1/2)) + y)
a = round(a, 5)
return(a)
}

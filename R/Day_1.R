
# Day 1: Part 1 functions -------------------------------------------------

#' Mass to fuel conversion function
#'
#' @param mass_val the value of the mass to be converted to fuel
#'
#' @return
#' @export
#'
#' @examples
day1MassToFuelConverter <- function(mass_val){
        fuel_val <- floor(mass_val / 3) - 2
        fuel_val
}

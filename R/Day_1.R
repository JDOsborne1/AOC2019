
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



# Day 1: Part 2 Functions -------------------------------------------------

#' Recursive Mass to Fuel conversion function
#'
#' @param mass_value the value of the mass to be converted to fuel recursively
#'
#' @return
#' @export
#'
#' @examples
day1MassToFuelConverter_complete <- function(mass_value){
        if(mass_value <= 0){
                #when the calculation returns 0 or negative fuel, then return 0 fuel
                return(0)
        } else {
                # Otherwise recursively add the fuel needed
                return_val <- day1MassToFuelConverter(mass_value) + day1MassToFuelConverter_complete( day1MassToFuelConverter(mass_value) )
                # in the event that the final calculation requires 0 or negative fuel, then return 0
                return(ifelse(return_val <= 0, 0, return_val) )
        }
}

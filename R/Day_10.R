#' Input Parser for Day 10 of the AOC Challenge
#'
#' @param input_chars the input character
#'
#' @return
#' @export
#'
#' @examples
day10InputParser <- function(input_chars){

        input_chars_parsed <- input_chars%>%
                strsplit("\n") %>%
                unlist() %>%
                strsplit("")

        input_matrix <- do.call(cbind, input_chars_parsed) %>% t()

        which(input_matrix == "#", arr.ind = TRUE) %>%
                as_tibble() %>%
                mutate(coords = map2(col-1L, row-1L, c)) %>%
                select(coords)

}


#' Vector Length Calculator
#'
#' @param vect the vector in question
#'
#' @return
#' @export
#'
#' @examples
day10LengthCalc <- function(vect){
        sqrt(sum(vect * vect)) * sign(vect[1])
}



#' Vector Magnitude Calculator
#'
#' @param vect the vector in question
#'
#' @return
#' @export
#'
#' @examples
day10MagnitudeCalc <- function(vect){
        sqrt(sum(vect * vect))
}
#' Asteroid View Count
#'
#' @param current_coord the coordinate to "look" from
#' @param asteroid_list the list of possible asteroids to see
#'
#' @return
#' @export
#'
#' @examples
day10AsteroidViewCount <- function(current_coord, asteroid_list){
        asteroid_list %>%
                mutate(diffs = map(coords, function(x) x - current_coord)) %>%
                # mutate(diff_length = map(diffs, day10LengthCalc)) %>%
                mutate(diff_mag = map(diffs, day10MagnitudeCalc)) %>%
                filter(diff_mag != 0) %>%
                mutate(norm_direction = map2(diffs, diff_mag, function(x, y) x/y)) %>%
                mutate(
                        norm_direction_x = map(norm_direction, function(x) round(x[1], 5))
                        , norm_direction_y = map(norm_direction, function(x) round(x[2], 5))
                ) %>%
                unnest(cols = c(norm_direction_x, norm_direction_y, diff_mag)) %>%
                arrange(diff_mag) %>%
                distinct(
                        norm_direction_x
                        , norm_direction_y
                        , .keep_all = T
                ) %>%
                nrow()
}

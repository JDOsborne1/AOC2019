
# Transforming the movemnt command into a coord ---------------------------
#' Move codes to Coord
#'
#' @param move_code the move code to transform of the format "[a-zA-Z]\d*"
#'
#' @return
#' @export
#'
#' @examples
day3MoveCodeToCoords <- function(move_code){
        direction <- stringr::str_extract(move_code, "^.")

        distance <- stringr::str_extract(move_code, "\\d*$")

        coord <- dplyr::case_when(
                direction == "R" ~ c(1L,0L)
                , direction == "L" ~ c(-1L,0L)
                , direction == "U" ~ c(0L,1L)
                , direction == "D" ~ c(0L,-1L)
                #, TRUE ~ stop("Unknown move code")
        )

        coord %>%
                list() %>%
                rep(distance)

}

# Taking a set of movement transforms and making the list of transforms ---

#' Move code to transform: List Edition
#'
#' @param list_of_move_codes the list of move codes
#'
#' @return
#' @export
#'
#' @examples
day3ListOfMoveCodesToListOfTransforms <- function(list_of_move_codes){
        list_of_move_codes %>%
                purrr::map(day3MoveCodeToCoords) %>%
                {Reduce(f = append, x = .)}
}

# stringing multiple commands together to build out full path -------------

#' Written wrapper around `+`
#'
#' @param x LHS
#' @param y RHS
#'
#' @return
#' @export
#'
#' @examples
plus <- function(x, y){
        x + y
}

#' Generate tibble path from set of transforms
#'
#' @param list_of_transforms the list of transforms to be applied
#' @param init the starting coords, defaults to c(0,0)
#'
#' @return
#' @export
#'
#' @examples
day3GeneratePathFromMoveTransform <- function(list_of_transforms, init = c(0,0)){
        Reduce(
                x = list_of_transforms
                , f = plus
                , init = init
                , accumulate = TRUE
        ) %>%
                tibble::enframe(
                        name = NULL
                )
}

#' Generate the path from the string
#'
#' @param input_string the input string
#'
#' @return
#' @export
#'
#' @examples
day3GetPathFromString <- function(input_string){
        input_list <- strsplit(input_string, ",") %>% unlist()

        input_list_of_transforms <- day3ListOfMoveCodesToListOfTransforms(input_list)

        input_path <- day3GeneratePathFromMoveTransform(input_list_of_transforms)

        input_path

}

# Taking two paths and determining where it crosses -----------------------

#' Determine the crossing points of two instruction strings
#'
#' @param input_string_1 the first instruction string
#' @param input_string_2 the second instruction string
#'
#' @return
#' @export
#'
#' @examples
day3DetermineCrossing <- function(input_string1, input_string2){


        input_path_1 <- input_string1 %>%
                day3GetPathFromString() %>%
                dplyr::mutate(str.value = as.character(value))
        input_path_2 <- input_string2 %>%
                day3GetPathFromString() %>%
                dplyr::mutate(str.value = as.character(value))

        input_path_1 %>%
                dplyr::select(-value) %>%
                dplyr::inner_join(input_path_2, by = "str.value") %>%
                dplyr::filter(str.value != "c(0, 0)")
}

# Using the manhattan distance eq to determine minimum distance -----------

#' Manhattan Distance between two vectors
#'
#' @param vect_target the out of origin vector
#' @param vect_reference the origin vector, assumed to be c(0,0)
#'
#' @return
#' @export
#'
#' @examples
day3ManhattanDistance <- function(vect_target, vect_reference = c(0, 0)){
        vect_distance <- vect_target - vect_reference

        vect_distance %>%
                abs() %>%
                sum()
}


# Wrapper to return the closest crossover ---------------------------------

#' Determine the closest crossover
#'
#' @param input_string1 the first input string
#' @param input_string2 the second input string
#' @param vect_reference the point of origin
#'
#' @return
#' @export
#'
#' @examples
day3DetermineClosestCrossover <- function(input_string1, input_string2, vect_reference = c(0,0)) {
        day3DetermineCrossing(input_string1, input_string2) %>%
                dplyr::mutate(distance = purrr::map(value, day3ManhattanDistance, vect_reference = vect_reference)) %>%
                dplyr::select(-value) %>%
                #tidyr::unnest(cols = distance) %>%
                tidyr::unnest() %>%
                dplyr::filter(distance == min(distance)) %>%
                dplyr::pull(distance) %>%
                as.integer()
}



# Taking two paths and getting the path length to each crossing -----------


#' Determine the crossing points of two instruction strings
#'
#' @param input_string_1 the first instruction string
#' @param input_string_2 the second instruction string
#'
#' @return
#' @export
#'
#' @examples
day3DetermineShortestPath <- function(input_string_1, input_string_2){
        input_path_1 <- input_string_1 %>%
                day3GetPathFromString() %>%
                dplyr::mutate(str.value = as.character(value))
        input_path_2 <- input_string_2 %>%
                day3GetPathFromString() %>%
                dplyr::mutate(str.value = as.character(value))

        intersections <- input_path_1 %>%
                dplyr::select(-value) %>%
                dplyr::inner_join(input_path_2, by = "str.value") %>%
                dplyr::filter(str.value != "c(0, 0)")


        path_lengths_1 <- input_path_1 %>%
                dplyr::select(-value) %>%
                tibble::rownames_to_column() %>%
                dplyr::filter(str.value %in% intersections$str.value) %>%
                dplyr::mutate_at(dplyr::vars(rowname), as.integer) %>%
                dplyr::mutate_at(dplyr::vars(rowname), function(x) x - 1L) %>%
                dplyr::group_by(str.value) %>%
                dplyr::filter(rowname == min(rowname)) %>%
                dplyr::ungroup() %>%
                dplyr::rename(min.path.length.1 = rowname)
        path_lengths_2 <- input_path_2 %>%
                dplyr::select(-value) %>%
                tibble::rownames_to_column() %>%
                dplyr::filter(str.value %in% intersections$str.value) %>%
                dplyr::mutate_at(dplyr::vars(rowname), as.integer) %>%
                dplyr::mutate_at(dplyr::vars(rowname), function(x) x - 1L) %>%
                dplyr::group_by(str.value) %>%
                dplyr::filter(rowname == min(rowname)) %>%
                dplyr::ungroup() %>%
                dplyr::rename(min.path.length.2 = rowname)

        intersections %>%
                dplyr::left_join(path_lengths_1, by = "str.value") %>%
                dplyr::left_join(path_lengths_2, by = "str.value") %>%
                dplyr::mutate(total.path.length = min.path.length.1 + min.path.length.2) %>%
                dplyr::select(str.value, total.path.length) %>%
                dplyr::filter(total.path.length == min(total.path.length)) %>%
                dplyr::pull(total.path.length) %>%
                as.integer()

}

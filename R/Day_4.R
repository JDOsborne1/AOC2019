

# Functions to fullfill the tests -----------------------------------------

#' Monotonicity Check
#'
#' @param a_vector_of_values the Vector to check
#'
#' @return
#' @export
#'
#' @examples
day4CheckSequenceMonotonicity <- function(a_vector_of_values){
        vector_range <- 1:length(a_vector_of_values)

        vector_range %>%
                purrr::map(function(x) !isFALSE(a_vector_of_values[x-1] <= a_vector_of_values[x])) %>%
                as.logical() %>%
                all()
}

#' Check sequence length
#'
#' @param a_vector_of_values the Vector to check
#' @param required_length the Length required, defaults to 6
#'
#' @return
#' @export
#'
#' @examples
day4CheckSequenceLength <- function(a_vector_of_values, required_length = 6){
        length(a_vector_of_values) == required_length
}

#' Sequence Range checker
#'
#' @param a_vector_of_values the Vector to check
#' @param min_val the lower bound
#' @param max_val the upper bound
#'
#' @return
#' @export
#'
#' @examples
day4CheckSequenceRange <- function(a_vector_of_values, min_val, max_val){
        vector_of_values_number <- a_vector_of_values %>%
                glue::glue_collapse() %>%
                as.integer()

        all(
        vector_of_values_number >= min_val
        ,
        vector_of_values_number <= max_val
        )
}

#' Check the presence of double numbers
#'
#' @param a_vector_of_values The vector to check
#'
#' @return
#' @export
#'
#' @examples
day4CheckSequenceDouble <- function(a_vector_of_values){
        vector_range <- 1:length(a_vector_of_values)
        vector_range %>%
                purrr::map(function(x) isTRUE(a_vector_of_values[x-1] == a_vector_of_values[x])) %>%
                as.logical() %>%
                any()
}


# Function to split a sequence into a vector of values --------------------

day4VectorSplitSequence <- function(sequence) {
        vector_of_values <- sequence %>%
                strsplit("") %>%
                unlist()
        vector_of_values
}


# Wrapper for all tests ---------------------------------------------------

#' Sequence checker
#'
#' @param min_val the lower bound
#' @param max_val the upper bound
#' @param vector_split_sequence the vector sequence to check
#'
#' @return
#' @export
#'
#' @examples
day4CheckSequence <- function(vector_split_sequence, min_val, max_val){
        all(
                day4CheckSequenceDouble(vector_split_sequence)
                ,
                day4CheckSequenceLength(vector_split_sequence)
                ,
                day4CheckSequenceMonotonicity(vector_split_sequence)
                ,
                day4CheckSequenceRange(vector_split_sequence, min_val = min_val, max_val = max_val)
        )
}

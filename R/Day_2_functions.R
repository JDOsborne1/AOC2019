
# Day 2: Part 1 functions -------------------------------------------------


#' String to Intcode converter
#'
#' @param string_intcode The inmput string of the intcode value
#'
#' @return an integer vector of the intcode
#' @export
#'
#' @examples
day2StringToIntcode <- function(string_intcode){
        string_intcode %>%
                strsplit(",") %>%
                unlist() %>%
                as.integer()
}

#' Incoder Function
#'
#' @param intcode the list of intcode values
#'
#' @return the calculated list of intcode values
#' @export
#'
#' @examples
day2IntcodeCalculator <- function(intcode){
        op_locale <- 1L

        for(i in 1:length(intcode)){

                op_code <- intcode[op_locale]

                if(op_code == 1){
                        # Addition
                        intcode[intcode[op_locale+3]+1]  <- intcode[intcode[op_locale+1]+1] + intcode[intcode[op_locale+2]+1]
                        op_locale <- op_locale + 4
                } else if(op_code == 2){
                        # multiplication
                        intcode[intcode[op_locale+3]+1]  <- intcode[intcode[op_locale+1]+1] * intcode[intcode[op_locale+2]+1]
                        op_locale <- op_locale + 4
                } else if(op_code != 99){
                        stop("error: unknown op_code")
                } else {
                        #print("Reached terminus")
                        break()
                }
        }
        intcode
}

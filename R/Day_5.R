#' Title
#'
#' @param parameterised_opcode
#'
#' @return
#' @export
#'
#' @examples
day5OpcodeInterpreter <- function(parameterised_opcode){
        parameterised_opcode_refined <- stringr::str_pad(
                parameterised_opcode
                , 2L
                , "left"
                ,"0"
        )
        op_code <- stringr::str_extract( parameterised_opcode_refined, "\\d{2}$")
        argument_lookup <- list(
                "01" = 3
                , "02" = 3
                , "99" = 0
                , "03" = 1
                , "04" = 1
                )

        argument_length <- as.integer(argument_lookup[op_code])

        full_opcode <- stringr::str_pad(
                parameterised_opcode
                , 2L + argument_length
                , "left"
                ,"0"
        )
        full_opcode_vector <- unlist(stringr::str_split(full_opcode, ""))

        opcode_parameters <- full_opcode_vector[1:argument_length]

        output_vals <- list()
        output_vals$op_code <- op_code
        output_vals$opcode_parameters <- rev(opcode_parameters)
        output_vals$full_opcode_vector <- full_opcode_vector
        output_vals
}


#' Title
#'
#' @param intcode
#' @param position
#' @param mode
#'
#' @return
#' @export
#'
#' @examples
day5IntcodeExtractor <- function(intcode, position, mode = "0"){
        if(mode == "0"){
                intcode[intcode[position]+1]
        } else if(mode == "1") {
                intcode[position]
        }
}


#' Title
#'
#' @param intcode
#'
#' @return
#' @export
#'
#' @examples
day5IntcodeCalculator <- function(intcode, input = "NiL"){
        op_locale <- 1L
        movement_lookup <- list("01" = 4, "02" = 4, "99" = 1, "03" = 2, "04" = 2 )

        for(i in 1:length(intcode)){

                interpreted_op_code <- day5OpcodeInterpreter(intcode[op_locale])


                movement <- as.integer(movement_lookup[interpreted_op_code$op_code])

                print(glue::glue("Operation is: {glue::glue_collapse(intcode[op_locale:(op_locale+movement)], sep = ', ')}"))

                if(interpreted_op_code$op_code == "01"){
                        # Addition
                        intcode[intcode[op_locale+3]+1]  <- day5IntcodeExtractor(
                                intcode
                                , op_locale+1
                                , interpreted_op_code$opcode_parameters[1]
                                ) +
                                day5IntcodeExtractor(
                                        intcode
                                        , op_locale+2
                                        , interpreted_op_code$opcode_parameters[2]
                                )
                        op_locale <- op_locale + movement
                } else if(interpreted_op_code$op_code == "02"){
                        # multiplication
                        intcode[intcode[op_locale+3]+1]  <- day5IntcodeExtractor(
                                intcode
                                , op_locale+1
                                , interpreted_op_code$opcode_parameters[1]
                                ) *
                                day5IntcodeExtractor(
                                intcode
                                , op_locale+2
                                , interpreted_op_code$opcode_parameters[2]
                        )
                        op_locale <- op_locale + movement
                } else if(interpreted_op_code$op_code == "03"){
                        # saving
                        intcode[intcode[op_locale+1]+1]  <- input
                        op_locale <- op_locale + movement
                } else if(interpreted_op_code$op_code == "04"){
                        # printing
                        output_var <- day5IntcodeExtractor(
                                intcode
                                , op_locale+1
                                , interpreted_op_code$opcode_parameters[1]
                        )

                        if(output_var != 0) {

                                #print(intcode)
                                stop("non zero exit at: {op_locale}")
                                }

                        print(output_var)
                        op_locale <- op_locale + movement
                } else if(interpreted_op_code$op_code != "99"){
                        stop("error: unknown op_code")
                } else {
                        print("Reached terminus")
                        break()
                }
        }
        intcode
}

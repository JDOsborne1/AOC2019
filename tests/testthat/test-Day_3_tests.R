test_that("ShortestCrossover works", {
  expect_equal(day3DetermineClosestCrossover(
          input_string1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
          , input_string2 = "U62,R66,U55,R34,D71,R55,D58,R83"
  )
  ,
  159
  )
  expect_equal(day3DetermineClosestCrossover(
          input_string1 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
          , input_string2 = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
  )
  ,
  135
  )
})


test_that("Move code transform works", {
        expect_equal(
                day3MoveCodeToCoords("R1")
                ,
                list(c(1,0))
        )
        up_10 <- day3MoveCodeToCoords("U10")
        expect_equal(
                length(up_10)
                ,
                10
        )
        expect_equal(
                sum(unlist(up_10))
                ,
                10
        )
})

test_that("Move code transform works for lists", {
        move_codes <- c("R2", "U3")
        reference_list <- list(
                c(1,0)
                , c(1,0)
                , c(0, 1)
                , c(0, 1)
                , c(0, 1)
        )
        expect_equal(
                day3ListOfMoveCodesToListOfTransforms(move_codes)
                ,
                reference_list
                )
})

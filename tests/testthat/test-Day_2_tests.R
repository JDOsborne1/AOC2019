test_that("intcoding works", {
  expect_equal(
          day2IntcodeCalculator(
                  day2StringToIntcode(
                          "1,9,10,3,2,3,11,0,99,30,40,50"
                          )
          )
          , day2StringToIntcode(
                  "3500,9,10,70,2,3,11,0,99,30,40,50"
          )
          )
  expect_equal(
          day2IntcodeCalculator(
                  day2StringToIntcode(
                          "1,0,0,0,99"
                          )
          )
          , day2StringToIntcode(
                  "2,0,0,0,99"
          )
          )
  expect_equal(
          day2IntcodeCalculator(
                  day2StringToIntcode(
                          "2,3,0,3,99"
                          )
          )
          , day2StringToIntcode(
                  "2,3,0,6,99"
          )
          )
  expect_equal(
          day2IntcodeCalculator(
                  day2StringToIntcode(
                          "2,4,4,5,99,0"
                          )
          )
          , day2StringToIntcode(
                  "2,4,4,5,99,9801"
          )
          )
  expect_equal(
          day2IntcodeCalculator(
                  day2StringToIntcode(
                          "1,1,1,4,99,5,6,0,99"
                          )
          )
          , day2StringToIntcode(
                  "30,1,1,4,2,5,6,0,99"
          )
          )
})

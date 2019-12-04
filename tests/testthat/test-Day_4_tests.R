test_that("multiplication works", {
  expect_true(day4CheckSequence("111111"))
  expect_false(day4CheckSequence("223450"))
  expect_false(day4CheckSequence("123789"))
})

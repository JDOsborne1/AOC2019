test_that("multiplication works", {
  expect_true(day4CheckSequence(day4VectorSplitSequence("111111"), 0, 1000000))
  expect_false(day4CheckSequence(day4VectorSplitSequence("223450"), 0, 1000000))
  expect_false(day4CheckSequence(day4VectorSplitSequence("123789"), 0, 1000000))
})

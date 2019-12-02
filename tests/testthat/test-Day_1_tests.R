
test_that("Fuel conversion works", {
  expect_equal(day1MassToFuelConverter(12), 2)
  expect_equal(day1MassToFuelConverter(14), 2)
  expect_equal(day1MassToFuelConverter(1969), 654)
  expect_equal(day1MassToFuelConverter(100756), 33583)
})

test_that("Complete Fuel conversion works", {
  expect_equal(day1MassToFuelConverter_complete(1969), 966)
  expect_equal(day1MassToFuelConverter_complete(100756), 50346)
})




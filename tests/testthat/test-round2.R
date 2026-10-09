#round2_001
test_that("round2 returns fixed decimals as character", {
  expect_equal(round2(1.2, 2), "1.20")
  expect_equal(round2(1.234, 2), "1.23")
  expect_equal(round2(1, 2), "1.00")
  expect_type(round2(1.2, 2), "character")
})

#round2_002
test_that("round2 drops leading zeros by default when all |x| < 1", {
  expect_equal(round2(0.34, 2), ".34")
  expect_equal(round2(c(0.34, 0.5), 2), c(".34", ".50"))
  expect_equal(round2(-0.34, 2), "-.34")
})

#round2_003
test_that("round2 keeps leading zeros when leading.zero = TRUE", {
  expect_equal(round2(0.34, 2, leading.zero = TRUE), "0.34")
  expect_equal(round2(c(0.34, 0.5), 2, leading.zero = TRUE), c("0.34", "0.50"))
})

#round2_004
test_that("round2 applies leading-zero rule all-or-none for vectors", {
  expect_equal(round2(c(0.34, 1.25), 2), c("0.34", "1.25"))
  expect_equal(round2(c(0.34, 1.25), 2, leading.zero = FALSE), c("0.34", "1.25"))
})

#round2_005
test_that("round2 handles NA and non-numeric input", {
  result <- round2(c(0.34, NA, 0.5), 2)
  expect_equal(result[1], ".34")
  expect_true(is.na(result[2]))
  expect_equal(result[3], ".50")
  expect_error(round2("a"), "must be numeric")
})

#round2_006
test_that("round2 keeps leading zero when |x| >= 1", {
  expect_equal(round2(1.2, 2), "1.20")
  expect_equal(round2(0, 2), ".00")
})

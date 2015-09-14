data("faithful")
test_that("incorrect",{
  expect_equal(lm(eruptions~waiting,faithful)==linreg(eruptions~waiting,faithful),TRUE)
})
test_that("incorrect!",{
  expect_equal(3==3,TRUE)
})
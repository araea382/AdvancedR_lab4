data("faithful")
test_that("incorrect",{
  expect_equal(lm(eruptions~waiting,faithful)==linreg(eruptions~waiting,faithful),TRUE)
})
data("iris")
test_that("cubic is correct",{
expect_equal(lm(Sepal.Length~Sepal.Width+Petal.Length^2,iris)==
  linreg(Sepal.Length~Sepal.Width+Petal.Length^2,iris),TRUE)
})
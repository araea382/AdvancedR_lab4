data("faithful")
test_that("incorrect",{
  lm(eruptions~waiting,faithful)
})

#this test will fail because they are not absolutely identical.

data("faithful")
m<-lm(eruptions~waiting,faithful)==
 n<- linreg(eruptions~waiting,faithful)
test_that("incorrect",{
  expect_equal(sum(as.numeric(unlist(m[1]))-as.numeric(unlist(n))),0)
})


data("iris")
lm.iris<-lm(Sepal.Length~Sepal.Width+Petal.Length^2,iris)==
lin.iris<-  linreg(Sepal.Length~Sepal.Width+Petal.Length^2,iris)
test_that("cubic is correct",{
expect_equal(sum(as.numeric(unlist(lm.iris[1]))-as.numeric(unlist(lin.iris))),0)
})
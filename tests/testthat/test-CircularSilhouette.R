#opposite
test_that("linear and quadratic should be equal", {
  o=c(19,0,4,6,10,12,15)
  c=c(1,1,2,2,2,2,2)
  circumference=20
  linear=circular.sil(o, c, circumference, "linear")
  quadratic=circular.sil(o, c, circumference, "quadratic")
  expect_equal(linear, quadratic)
})
test_that("linear and manual should be equal", {
  o=c(19,0,4,6,10,12,15)
  c=c(1,1,2,2,2,2,2)
  circumference=20
  linear=circular.sil(o, c, circumference, "linear")
  manual=(5.6/6.6+5.4/6.4-1.75/6.25+1.25/6.5+5.25/9.5+2.75/7.5-2/6.5)/7
  expect_equal(linear, manual)
})

#non-opposite
test_that("linear and quadratic should be equal", {
  o=c(0,1,5,6,8,9)
  c=c(1,1,2,2,3,3)
  circumference=10
  linear=circular.sil(o, c, circumference,"linear")
  quadratic=circular.sil(o, c, circumference,"quadratic")
  expect_equal(linear, quadratic)
})
test_that("linear and manual should be equal", {
  o=c(0,1,5,6,8,9)
  c=c(1,1,2,2,3,3)
  circumference=10
  linear=circular.sil(o, c, circumference, "linear")
  manual=(2/3+9/5+5/7)/6
  expect_equal(linear, manual)
})
#linear data
test_that("fast linear and manual should be equal", {
  o=c(0,1,5,6,8,9)
  c=c(1,1,2,2,3,3)
  fast=fast.sil(o, c)
  manual=(9/11+7/9+2*(5/7+3/5))/6
  expect_equal(fast, manual)
})

#two data
test_that("linear and quadratic should be equal", {
  o=c(2,8)
  c=c(1,2)
  circumference=10
  linear=circular.sil(o, c, circumference,"linear")
  quadratic=circular.sil(o, c, circumference,"quadratic")
  expect_equal(linear, quadratic)
})
test_that("linear and manual should be equal", {
  o=c(2,8)
  c=c(1,2)
  circumference=10
  linear=circular.sil(o, c, circumference,"linear")
  manual=-1
  expect_equal(linear, manual)
})

#disordered cluster
test_that("linear and quadratic should be equal", {
  o=c(0,1,2,3,4,5,6,7,8,9)
  c=c(1,1,1,4,4,4,2,2,3,3)
  circumference=10
  linear=circular.sil(o, c, circumference,"linear")
  quadratic=circular.sil(o, c, circumference,"quadratic")
  expect_equal(linear, quadratic)
})
test_that("linear and manual should be equal", {
  o=c(0,1,2,3,4,5,6,7,8,9)
  c=c(1,1,1,4,4,4,2,2,3,3)
  circumference=10
  linear=circular.sil(o, c, circumference,"linear")
  manual=(2.7+2/3)/10
  expect_equal(linear, manual)
})




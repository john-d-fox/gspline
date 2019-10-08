set.seed(12345)
x <- runif(200, 0, 10)
y <- cos(1.25*(x + 1)) + x/5 + 0.5*rnorm(200)

t <- c(10/3, 20/3)
x1 <- x 
x2 <- (x - t[1]) * (x > t[1])
x3 <- (x - t[2]) * (x > t[2])
m1 <- lm(y ~ poly(x1, degree=3, raw=TRUE) + I(x2^3) + I(x3^3))

sp2 <- gspline(knots=t)
m2 <- lm(y ~ sp2(x))

test_that("cubic regression spline computed correctly", {
  expect_equal(fitted(m1), fitted(m2))
})

sp3 <- gspline(x, knots=t)
m3 <- lm(y ~ sp3(x))

test_that("stable cubic regression spline computed correctly", {
  expect_equal(fitted(m1), fitted(m3))
})

test_that("stable cubic regression spline basis is orthonormal", {
  expect_equal(crossprod(sp3(x)), diag(5))
})

if (require(splines)){
  
  sp4 <- gspline(knots=c(0, 10/3, 20/3, 10), 
                   degree=c(1, 3, 3, 3, 1), 
                   smoothness=c(2, 2, 2, 2))
  m4 <- lm(y ~ sp4(x))
  
  m5 <- lm(y ~ ns(x, knots=c(10/3, 20/3), Boundary.knots=c(0, 10)))
  
  test_that("natural cubic regression spline computed correctly", {
    expect_equal(fitted(m4), fitted(m5))
  })
  
  sp6 <- gspline(x, knots=c(0, 10/3, 20/3, 10), 
                 degree=c(1, 3, 3, 3, 1), 
                 smoothness=c(2, 2, 2, 2))
  m6 <- lm(y ~ sp6(x))
  
  test_that("stable natural cubic regression spline computed correctly", {
    expect_equal(fitted(m5), fitted(m6))
  })
  
  test_that("stable natural cubic regression spline basis is orthonormal", {
    expect_equal(crossprod(sp6(x)), diag(3))
  })
  
  m7 <- lm(y ~ bs(x, knots=t))
  
  test_that("stable cubic regression spline agrees with B-spline", {
    expect_equal(fitted(m1), fitted(m3))
  })
}
# because of scoping issues this test can't be performed by testthat

library(gspline)

set.seed(12345)
x <- runif(200, 0, 10)
y <- cos(1.25*(x + 1)) + x/5 + 0.5*rnorm(200)
D <- data.frame(x, y)

t <- c(10/3, 20/3)

sp2 <- gspline(knots=t)
m2 <- lm(y ~ sp2(x), data=D)

sp3 <- gspline(x, knots=t)
m3 <- lm(y ~ sp3(x), data=D)

wm2 <- wald(m2)[[1]]$estimate
wm3 <- wald(m3)[[1]]$estimate
all.equal(wm2, wm3, check.attributes=FALSE)

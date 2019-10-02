#' 
#' Tests for gspline3 and wald3
#' 
#' 2019_10_01 GM
#' 
spans <- function(A,B) {
	opts <- options(warn = -1)
	on.exit(options(opts))
	c('A in B' = sum((lsfit(B,A, intercept = FALSE)$residual)^2),
	  'B in A' = sum((lsfit(A,B, intercept = FALSE)$residual)^2))
}

library(car)
library(magrittr)
library(latticeExtra)
library(gspline)
set.seed(11333)
dd <- data.frame( x = 1:10, z= rep(c(0, 1), each=5))
dd$y <- with(dd, x^3 + z + 1e3*rnorm(x))
xyplot(y ~ x, dd, groups = z, pch = 16)

# Using original gspline

sp <- gspline( 5, 2, 1)
fit <- lm(y ~ z + sp(x),dd)
summary(fit)
wald3(fit)
sp(c(3,5), D = 2)
L <- list('2nd deriv. at 3 and 5' = cbind(0, 0, sp(c(3,5), D = 2)))
wald3(fit, L)

# using gspline3, raw spline

sp3 <- gspline3(dd$x, knots=5, degree=2, smoothness=1)
fit3 <- lm(y ~ z + sp3(x),dd)
S(fit3)
wald3(fit3)
wald3(fit3, L) # consistent with est. coefs. and original
anova(fit3)
Anova(fit3)

# using gspline3, stable == TRUE

sp3s <- gspline3(dd$x, knots=5, degree=2, smoothness=1, stable = TRUE)
fit3s <- lm(y ~ z + sp3s(x),dd)
{
  sp3s(dd$x[1:10])
  gspline:::inwald(TRUE) 
  print(sp3s(dd$x[1:10]))
  gspline:::inwald(FALSE)
}
summary(fit3s)      # spline components have generic names
wald3(fit3s)
wald3(fit3)

wald3(fit3s, L)
wald3(fit, L)   # same

## with interaction

set.seed(11333)
dd <- data.frame( x = sample(1:10, 1000, replace=TRUE), z= runif(1000))
dd$y <- with(dd, x^3*z + 100*rnorm(x))

# raw spline

fit.i <- lm(y ~ z*sp3(x),dd)
summary(fit.i)
anova(fit.i)
Anova(fit.i)
L0 <- list('2nd deriv. at 3 and 5 | z = 0' = 
             cbind(0,0,
                   sp(c(3,5,5,5), D = 2, limit = c(1,1,-1,0)),0,0,0))
wald3(fit.i, L0) 
L1 <- list('2nd deriv. at 3 and 5 | z = 1' = 
             cbind(0,0,sp(c(3,5,5,5), D = 2, limit = c(1,1,-1,0)),
                   sp(c(3,5,5,5), D = 2, limit = c(1,1,-1,0))))
wald3(fit.i, L1)
#
#  Interaction with stable spline
#

fit.is <- lm(y ~ z*sp3s(x),dd) # stable spline fit

summary(fit.is)
summary(fit.i) # different

anova(fit.is)
anova(fit.i) # same

Anova(fit.is)
Anova(fit.i)  # same

wald3(fit.is, L0) 
wald3(fit.i, L0) # same

wald3(fit.is, L1) 
wald3(fit.i, L1)  # same 

wald3(fit.is, ":")
wald3(fit.i, ":")  # same

wald3(fit.is, "C") # regular expression is applied to raw names
wald3(fit.i, "C")  # same


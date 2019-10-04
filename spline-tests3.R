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

# the following was removed from gspline.R

if(FALSE) {
# testing code
    sp <- gspline(c(-1,0,1),3,2)
    sp(c(-1,0,2,3))
    sp(c(-1,-1,-1,0,2,3),2,limit=c(-1,0,1,0,0,0))
    print(sp)
    sp <- gspline(c(-2,0,2),3,1)
    sp(seq(-1,2))
    sp(seq(-1,2),1)
    sp(c(0,0,0,0),c(0,1,2,3))
    sp(c(0,0,0,0),c(0,1,2,3), limit = -1)

    # test periodic splines
    zd <- data.frame(x=-10:10)
    zd <- within(zd, y <- x + abs(x) + .01*rnorm(x))
    sp0 <- gspline(0,1,0)
    fit <- lm(y ~ sp0(x), zd)
    summary(fit)
    print(sp)

    # problem?: parameters estimate from left, not right

    zd <- within(zd, yd <- x + abs(x) + I(x > 0) + .01 * rnorm(x))
    spd <- gspline(0,1, -1)
    fit <- lm(yd ~ spd(x), zd)
    summary(fit)
    spd(c(0,0,0,0,0,0), D=c(0,0,0,1,1,1), limit = c(-1,0,1,-1,0,1)) %>%
        {Lmat <- .}
    wald(fit, cbind(0,Lmat))
    plot(yd ~ x, zd)
    print(sp)

    # both level and slope are limits from the left:
    # probably should leave that way because it's easier
    # to add change than to go back

    # TODO: need to add limit directions on G matrix

    load("../data/Unemp.RData", verbose = T)
    plot(unemployment ~ date, data=Unemp, type="b")
    dim(Unemp)
    dd <- Unemp
    dd$y <- dd$unemployment
    dd$x <- 1:nrow(dd)
    f1 <- function(x) cbind(Sin=sin(2*pi*x/12),Cos=cos(2*pi*x/12))  # fundamental
    f2 <- function(x) structure(f1(x/2), names = c('Sin2','Cos2'))
    f3 <- function(x) structure(f1(x/3), names = c('Sin3','Cos3'))
    f4 <- function(x) structure(f1(x/4), names = c('Sin4','Cos4'))
    per <- gspline(c(3,6,9,12),c(1,2,2,2,1),0, periodic = TRUE)
    per2 <- gspline(c(3,6,9,12),2,1, periodic = TRUE)
    per3 <- gspline(c(3,6,9,12),3,2, periodic = TRUE)
    per4 <- gspline(c(3,6,9,12),4,3, periodic = TRUE)
    perstep <- gspline(c(3,6,9,12),0,-1, periodic = TRUE)

    per
    fits <- list(
        hetero = lm(y ~ per(x), dd),
        quad = lm(y ~ per2(x), dd),
        cubic =  lm(y ~ per3(x), dd),
        quartic = lm(y ~ per4(x), dd),
        step = lm(y ~ perstep(x), dd),
        f1 = lm(y ~ f1(x), dd),
        f2 = lm(y ~ f1(x) + f2(x), dd),
        f3 = lm(y ~ f1(x) + f2(x) + f3(x), dd),
        f4 = lm(y ~ f1(x) + f2(x) + f3(x) + f4(x), dd)
    )
    fits %>% lapply(summary)
    fits %>% sapply(AIC)
    pred <- list(x=seq(0,24,.01))
    plot(c(0,24),c(6.5,8.5), type = 'n')
    fits %>% lapply(function(f) lines(pred$x,predict(f, newdata = pred)))
    sp <- gspline(nrow(dd)*1:7/8,2,1)
    sp3 <- gspline(nrow(dd)*1:7/8,3,2)
    fit <- lm(y ~ per3(x) +sp(x), dd)
    fit3 <- lm(y ~ per3(x) +sp3(x), dd)
    fit4 <- lm(y ~ per3(x) +sp3(x), dd)
    with(dd, {
        plot(date,y, pch = '.',cex = 2)
        lines(date, predict(fit))
        lines(date, predict(fit3), col = 'red')
    }
    )
    pred <- data.frame(x=seq(1,24,.01))
    with(pred, {
        plot(c(1,24), c(9,11), xlab = 'month', ylab = 'unemployment', type = 'n')
        lines(x, predict(fit, newdata = pred))
    })
    AIC(fit,fit3)
    library(effects)
    allEffects(fit)
    dd$month <- dd$x %% 12
    fit <- lm(y ~ sp3(x) + per3(month), dd)
    summary(fit)
    plot(allEffects(fit))
    #
    # Test names
    #
    sp <- gspline(c(0,1,2,3), 2, c(-1,0,1,2))
    sp(0:4)
    knots(sp)
    (evs <- expand.grid(k=knots(sp), D = 1:3))
    with(evs, sp(k,D,limit = 0))
    evs$salt <- apply(with(evs, abs(sp(k, D, limit = 0))), 1, sum) > 1e-14
    evs
    dd <- expand.grid(x = seq(-1, 4, .1))
    dd <- within(dd, y <- x^2 + .1* rnorm(x) + (x > 0))
    library(latticeExtra)
    library(spida2)
    library(gnew)
    xyplot(y ~ x , dd)
    fit <- lm(y ~ sp(x),dd)
    dd$fit <- predict(fit)
    trellis.focus()
    panel.xyplot(dd$x, dd$fit, type = 'l')
    trellis.unfocus()
    summ(fit)
    evs <- expand.grid(limit = -1:1, x=-1:4, D = 0:2)
    with(evs, sp(x,D,limit))
    evs$intercept <- with(evs, 1*(D==0))
    Lmat <- with(evs, cbind(intercept, sp(x,D, limit)))
    knots(sp)
    rownames(Lmat) <- with(evs, paste(x,D,limit))
    wald(fit, Lmat)
}

# moved from wald.R

# Test
if (FALSE) {
  library(nlme)
  fit <-
    lme(mathach ~ ses * Sex * Sector, hs, random = ~ 1 | school)
  summary(fit)
  pred <-
    expand.grid(
      ses = seq(-2, 2, 1),
      Sex = levels(hs$Sex),
      Sector = levels(hs$Sector)
    )
  pred
  wald(fit, model.matrix(fit, data = pred))
  model.matrix(fit, data = pred)
  model.matrix(~ ses * Sex * Sector, data = pred)
}


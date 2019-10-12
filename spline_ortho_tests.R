#' 
#' Tests for periodic gsplines and wald
#' 
#' 2019_10_11 GM
#' 
#' 
#' Testing splines orthogonal to intercept
#' using scale or sweep
#' 
# library(rbenchmark)
# mat <- matrix(rnorm(100000), ncol = 100)
# benchmark('scale' = {
#   z <- scale(mat)
# },
# 'sweep' = {
#   z <- sweep(mat, 2, colMeans(mat))
# }, 
# replications = 1000,
# columns = c("test", "replications", "elapsed",
#                       "relative", "user.self", "sys.self")
# )
#' 
#' Note: sweep takes 25% as much time as scale
#' 
spans <- function(A,B) {
  # tests whether the column span of A is in the column span of B
  # and vice-versa
	opts <- options(warn = -1)
	on.exit(options(opts))
	c('A in B' = sum((lsfit(B,A, intercept = FALSE)$residual)^2),
	  'B in A' = sum((lsfit(A,B, intercept = FALSE)$residual)^2))
}


# library(latticeExtra)
library(gspline)
set.seed(123)
dd <- data.frame( x = -1:10, z= rep(c(0, 1), each=6))
dd$y <- with(dd, x^3 + z + 1e2*rnorm(x))
xyplot(y ~ x, dd, groups = z, pch = 16, cex = 2)

# Defining different splines

# raw spline
sp <- gspline(, 5, 2, 1)   
# full stable
sps <- gspline(0:10, 5, 2, 1, stable = TRUE)    
# stable no rescale
spsnr <- gspline(0:10, 5, 2, 1, stable = TRUE, rescale = FALSE)    
# stable not orthogonal to intercept
spsno <- gspline(0:10, 5, 2, 1, stable = TRUE, ortho2intercept = FALSE)    
# stable not orthogonal to intercept
spsnor <- gspline(0:10, 5, 2, 1, stable = TRUE, 
                  ortho2intercept = FALSE,
                  rescale = FALSE)    
sp_list <- list(sp, sps, spsnr, spsno, spsnor)
names(sp_list) <- c('raw','stable','no rescale','no ortho to int.','no rescale and no ortho to int.')
sp_basis <- lapply(sp_list, function(f) f(0:10))
sp_basis

#' Equality of bases:
#'
#' Note that 'stable' without 'ortho2intercept' produces a basis
#' that spans the same space as the raw basis.
#' 
#' Using ortho2intercept produced bases for a different
#' space, however, the spaces spanned by the bases together with
#' the 1-vector are the same:
#'   
lapply(sp_basis[-1], function(b) spans(b, sp(0:10)))

lapply(sp_basis[-1], function(b)
  spans(cbind(1,b), cbind(1,sp(0:10))))
#'
#' Equality of fitted values with intercept 
#'
ddx <- merge(dd, list(x = seq(-2,11,.1)), all = T)

fitraw <- lm(y ~ sp(x), ddx)
fitstable <- lm(y ~ sps(x), ddx)
fitno.rescale <- lm(y ~ spsnr(x), ddx)
fitno.orth <- lm(y ~ spsno(x), ddx)
fitno.orth.rescale <- lm(y ~ spsnor(x), ddx)

mods <- list(fitraw, fitstable, fitno.rescale,
             fitno.orth, fitno.orth.rescale)

ddx$yhat__raw <- predict(fitraw, newdata = ddx)
ddx$yhat__stable <- predict(fitstable, newdata = ddx)
ddx$yhat__no.rescale <- predict(fitno.rescale, newdata = ddx)
ddx$yhat__no.orth <- predict(fitno.orth, newdata = ddx)
ddx$yhat__no.orth.rescale <- predict(fitno.orth.rescale, newdata = ddx)

ddl <- spida2::tolong(ddx, sep = '__')
head(ddl)
spida2::gd(lty=1:5, fill = c('red','blue'))
xyplot(yhat ~ x, ddl, groups = time, type = 'l', auto.key = T) +
xyplot(y ~ x, ddx, groups = z, pch = 21, cex = 2) 

lapply(mods, summary) # similar
lapply(mods, wald)    # similar
#'
##' Models without intercept ----
#'
#' With models without intercepts, the splines
#' with bases orthogonal to the 1-vector give
#' different results than the raw spline
#' 
fitraw <- lm(y ~ sp(x) - 1, ddx)
fitstable <- lm(y ~ sps(x) - 1, ddx)
fitno.rescale <- lm(y ~ spsnr(x) - 1, ddx)
fitno.orth <- lm(y ~ spsno(x) - 1, ddx)
fitno.orth.rescale <- lm(y ~ spsnor(x) - 1, ddx)

mods <- list(fitraw, fitstable, fitno.rescale,
             fitno.orth, fitno.orth.rescale)

ddx$yhat__raw <- predict(fitraw, newdata = ddx)
ddx$yhat__stable <- predict(fitstable, newdata = ddx)
ddx$yhat__no.rescale <- predict(fitno.rescale, newdata = ddx)
ddx$yhat__no.orth <- predict(fitno.orth, newdata = ddx)
ddx$yhat__no.orth.rescale <- predict(fitno.orth.rescale, newdata = ddx)

ddl <- spida2::tolong(ddx, sep = '__')
head(ddl)
spida2::gd(lty=1:5, fill = c('red','blue'))
xyplot(yhat ~ x, ddl, groups = time, type = 'l', auto.key = T) +
xyplot(y ~ x, ddx, groups = z, pch = 21, cex = 2) 
#'
#' Only models with bases that are not orthogonal to the intercept
#' give results similar to the 
#'
lapply(mods, summary) # only mod
lapply(mods, wald)    # similar

#'
##' Periodic splines ----
#'

set.seed(123)
dd <- data.frame(x = seq(-5, 20,.1))
dd <- within(dd, {
  y <- x + sin(2*pi*x/7)  + .5 *rnorm(x) 
})
xyplot(y ~ x, dd)
#'
#'  Note that the x argument is ignored for periodic splines,
#'  rescaling is based on the knots.
#'
spp <- gspline(1, knots = c(5,7), degree = c(2,2,2), 
              smoothness = c(-1,1),
              periodic = TRUE)
spps <- gspline(1, knots = c(5,7), degree = c(2,2,2), 
              smoothness = c(-1,1),
              periodic = TRUE,
              stable = TRUE)
sppsnr <- gspline(1, knots = c(5,7), degree = c(2,2,2), 
              smoothness = c(-1,1),
              periodic = TRUE,
              rescale = FALSE,
              stable = TRUE)
sppsno <- gspline(1, knots = c(5,7), degree = c(2,2,2), 
              smoothness = c(-1,1),
              periodic = TRUE,
              ortho2intercept = FALSE,
              stable = TRUE)
sppsnor <- gspline(1, knots = c(5,7), degree = c(2,2,2), 
              smoothness = c(-1,1),
              periodic = TRUE,
              rescale = FALSE,
              ortho2intercept = FALSE,
              stable = TRUE)

fitpraw <- lm(y ~ x + spp(x), dd)
fitpstable <- lm(y ~ x + spps(x), dd)
fitpno.rescale <- lm(y ~ x + sppsnr(x), dd)
fitpno.orth <- lm(y ~ x + sppsno(x), dd)
fitpno.orth.rescale <- lm(y ~ x + sppsnor(x), dd)



dd$yhat__raw <- predict(fitpraw, newdata = dd)
dd$yhat__stable <- predict(fitpstable, newdata = dd)
dd$yhat__no.rescale <- predict(fitpno.rescale, newdata = dd)
dd$yhat__no.orth <- predict(fitpno.orth, newdata = dd)
dd$yhat__no.orth.rescale <- predict(fitpno.orth.rescale, newdata = dd)

ddl <- spida2::tolong(dd, sep = '__')
head(ddl)
spida2::gd(lty=1:5, fill = c('red','blue'))
xyplot(yhat ~ x, ddl, groups = time, type = 'l', auto.key = T) +
xyplot(y ~ x, ddx, pch = 21, cex = 2) 


pmods <- list(fitpraw, fitpstable, fitpno.rescale,
             fitpno.orth, fitpno.orth.rescale)
lapply(pmods, summary) # similar
lapply(pmods, wald)    # similar


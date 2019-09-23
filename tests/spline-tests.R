set.seed(11333)
dd <- data.frame( x = 1:10, z= rep(c(0, 1), each=5))
dd$y <- with(dd, x^3 + z + 1e3*rnorm(x))
sp <- gspline( 5, 2, 1)
fit <- lm(y ~ z + sp(x),dd)
summary(fit)
sp(c(2,5), D = 2)
wald(fit, cbind(0, 0, sp(c(3,5), D = 2)))
wald(fit, L2ortho(fit, cbind(0, 0, sp(c(3,5), D = 2))))



sp2 <- gspline2(dd$x, knots=5, degree=2, smoothness=TRUE)
fit2 <- lm(y ~ z + sp2(x),dd)
summary(fit2)
wald(fit2, cbind(0, 0, sp2(c(3,5), D = 2))) # potentially wrong
wald(fit2, L2ortho(fit2, cbind(0, 0, sp2(c(3,5), D = 2))))

set.seed(11333)
dd <- data.frame( x = sample(1:10, 1000, replace=TRUE), z= runif(1000))
dd$y <- with(dd, x^3*z + 10*rnorm(x))
fit.i <- lm(y ~ z*sp(x),dd)
summary(fit.i)
cbind(0,0,sp(c(3,5), D = 2),0,0,0)
wald(fit.i, cbind(0, 0, sp(c(3,5), D = 2),0,0,0)) 
wald(fit.i, L2ortho(fit.i, cbind(0, 0, sp(c(3,5), D = 2),0,0,0)))
cbind(0,0,sp(c(3,5), D = 2),sp(c(3,5), D = 2))
wald(fit.i, cbind(0, 0, sp(c(3,5), D = 2),sp(c(3,5), D = 2)))   
wald(fit.i, L2ortho(fit.i, cbind(0, 0, sp(c(3,5), D = 2),sp(c(3,5), D = 2))))

fit.i2 <- lm(y ~ z*sp2(x),dd)
summary(fit.i2)
cbind(0,0,sp2(c(3,5), D = 2),0,0,0)
wald(fit.i2, cbind(0, 0, sp2(c(3,5), D = 2),0,0,0)) # potentially wrong
wald(fit.i2, L2ortho(fit.i2, cbind(0, 0, sp2(c(3,5), D = 2),0,0,0)))


cbind(0,0,sp2(c(3,5), D = 2),sp2(c(3,5), D = 2))
wald(fit.i2, cbind(0, 0, sp2(c(3,5), D = 2), sp2(c(3,5), D = 2))) # potentially wrong
wald(fit.i2, L2ortho(fit.i2, cbind(0, 0, sp2(c(3,5), D = 2), sp2(c(3,5), D = 2))))

set.seed(11333)
dd <- data.frame( x = sample(1:10, 1000, replace=TRUE), z= runif(1000))
dd$y <- with(dd, x^3 + z^2 + 10*rnorm(x))

sp <- gspline( 5, 2, 1)
spz <- gspline(knots=c(1/2, 2/3))

sp2 <- gspline2(dd$x, knots=5, degree=2, smoothness=1, orthonormalize=TRUE)
spz2 <- gspline2(dd$z, knots=c(1/2, 2/3), orthonormalize=TRUE)
fit.zx <- lm(y ~ spz(z) + sp(x),dd)
summary(fit.zx)
cbind(0, 0, 0, 0, 0, 0, sp(c(3,5), D = 2))
wald(fit.zx, cbind(0, 0, 0, 0, 0, 0, sp(c(3,5), D = 2)))
fit.zx2 <- lm(y ~ spz2(z) + sp2(x),dd)
summary(fit.zx2)
wald(fit.zx2, L2ortho(fit.zx2, cbind(0, 0, 0, 0, 0, 0, sp2(c(3,5), D = 2))))

fit.zxi <- lm(y ~ spz(z)*sp(x),dd)
summary(fit.zxi)

l1 <- spz(.5,D=0)
l1
l2 <- sp(c(3,5),D=1)
l2
L <- cbind(0,0,0,0,0,0,l2,l1[,1]*l2,l1[,2]*l2,l1[,3]*l2,l1[,4]*l2,l1[,5]*l2)
L
wald(fit.zxi, L)

fit.zxi2 <- lm(y ~ spz2(z)*sp2(x), dd)
summary(fit.zxi2)
wald(fit.zxi2, L)
wald(fit.zxi2, L2ortho(fit.zxi2, L))

## ----

set.seed(11333)
dd <- data.frame( x = sample(1:10, 1000, replace=TRUE), z= runif(1000))
dd$y <- with(dd, x^3 + z^2 + 10*rnorm(x))

# raw splines

sp <- gspline( 5, 2, 1)
spz <- gspline(knots=c(1/2, 2/3))

fit.zxi <- lm(y ~ spz(z)*sp(x),dd)
summary(fit.zxi)

l1 <- spz(.5,D=0)
l1
l2 <- sp(c(3,5),D=1)
l2
L <- cbind(0,0,0,0,0,0,l2,l1[,1]*l2,l1[,2]*l2,l1[,3]*l2,l1[,4]*l2,l1[,5]*l2)
zapsmall(L)
wald(fit.zxi, L)

# orthogonal splines

sp2 <- gspline2(dd$x, knots=5, degree=2, smoothness=1, orthonormalize=TRUE)
spz2 <- gspline2(dd$z, knots=c(1/2, 2/3), orthonormalize=TRUE)

fit.zxi2 <- lm(y ~ spz2(z)*sp2(x), dd)
summary(fit.zxi2) # same fit

wald(fit.zxi2, L2ortho(fit.zxi2, L)) # same test

# orthogonal splines with rescaling

rescale <- function(x, periodic=FALSE){
	range <- range(x)
	if (periodic) function (x) (x - range[1])/(range[2] - range[1])
	else function(x) 2*(x - mean(range))/(range[2] - range[1])
}

fx <- rescale(dd$x)
fz <- rescale(dd$z)

spr <- gspline(fx(5), 2, 1)
spzr <- gspline(knots=fz(c(1/2, 2/3)))


sp2r <- gspline2(fx(dd$x), knots=fx(5), degree=2, smoothness=1, orthonormalize=TRUE)
spz2r <- gspline2(fz(dd$z), knots=fz(c(1/2, 2/3)), orthonormalize=TRUE)

l1r <- spz2r(fz(.5),D=0)
l1r
l2r <- sp2r(fx(c(3,5)),D=1)
l2r
Lr <- cbind(0,0,0,0,0,0,l2r,l1r[,1]*l2r,l1r[,2]*l2r,l1r[,3]*l2r,l1r[,4]*l2r,l1r[,5]*l2r)
Lr

fit.zxi2r <- lm(y ~ spz2r(fz(z))*sp2r(fx(x)), dd)
summary(fit.zxi2r) # same fit

wald(fit.zxi2r, L2ortho(fit.zxi2r, Lr)) # different test

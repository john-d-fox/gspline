#
# tests for Dspline
# 
# Summary:
# GM, 2020_01_02
#
# These are the examples for Dspline

library(gspline)
library(lattice)
library(latticeExtra)
dd <- Unemployment

head(dd)
xyplot(unemployment ~ date, dd)

dd <- within(dd, {
  years <- as.numeric(date - as.Date('2000-01-01'))/365.25
})
xyplot(unemployment ~ years, dd)
sp2 <- gspline(
  -5:20,
  knots = c(0,3,8.8,13,18),
  degree = 2,
  smooth = c(1,1,-1,1,1))
sp3 <- gspline(
  -5:20,
  knots = c(0,3,8.8,13,18),
  degree = 3,
  smooth = c(2,2,-1,2,2))
fit2 <- lm(unemployment ~ sp2(years), dd)
fit3 <- lm(unemployment ~ sp3(years), dd)
summary(fit2)
summary(fit3)

dd$fit2 <- predict(fit2)
dd$fit3 <- predict(fit3)
xyplot(unemployment ~ years, dd) +
  xyplot(fit2 ~ years, dd, type = 'l', col = 'red') +
  xyplot(fit3 ~ years, dd, type = 'l', col = 'blue') 

spper <- gspline(knots = c(3,6,9,12)/12, degree = 2, smooth = 1,periodic = TRUE)  

fit2p <- lm(unemployment ~ sp2(years) + spper(years), dd) 
fit3p <- lm(unemployment ~ sp3(years) + spper(years), dd)
dd$fit2p <- predict(fit2p)
dd$fit3p <- predict(fit3p)

xyplot(unemployment ~ years, dd) +
  xyplot(fit2p ~ years, dd, type = 'l', col = 'red') +
  xyplot(fit3p ~ years, dd, type = 'l', col = 'blue') 

dd <- within(dd, {
  res2 <- unemployment - fit2
  res3 <- unemployment - fit3
  res2p <- unemployment - fit2p
  res3p <- unemployment - fit3p
})

xyplot(res2 ~ years, dd, col = 'red', type = 'l') +
xyplot(res3p ~ years, dd, col = 'blue', type = 'l')

dd_deriv <- Dspline(fit3, sp3)
head(dd_deriv)

xyplot(coef ~ years, dd_deriv, type = 'l') 

xyplot(coef ~ years, dd_deriv, type = 'l',
       fit = dd_deriv$coef,
       upper = dd_deriv$coef + dd_deriv $ se,
       lower = dd_deriv$coef - dd_deriv $ se,
       subscript = T) +
  layer(panel.fit(...))

dd_deriv2 <- Dspline(fit3, sp3, D = 2)

xyplot(coef ~ years, dd_deriv2, type = 'l',
       fit = dd_deriv2$coef,
       upper = dd_deriv2$coef + dd_deriv2 $ se,
       lower = dd_deriv2$coef - dd_deriv2 $ se,
       subscript = T) +
  layer(panel.fit(...))

# since the splines are additive

dd_per3 <- Dspline(fit3p, sp3, D = 1)
dd_per3p <- Dspline(fit3p, spper, D = 1)

# To compute the combined SEs we would need to add the Lmatrices
# and use the wald function with that

dd_per3 <- within(dd_per3, {
  coefper <- dd_per3p$coef
  coefdetrended <- coef
  coefcombined <- coefdetrended + coefper 
  date <- dd$date
})

xyplot(coefcombined ~ date, dd_per3, type = 'l')+
xyplot(coefper ~ date, dd_per3, type = 'l', col = 'black')+
xyplot(coefdetrended ~ date, dd_per3, type = 'l', col = 'red')

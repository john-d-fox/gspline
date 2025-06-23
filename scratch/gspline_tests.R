# 
# gspline tests
# 
# Summary:
# GM, 2020_01_02
#
# Using more than one stable spline in a model can lead to
# nonsense results, presumably because of a conflict in the
# two splines relocating of the origin.
#
# Also the stable splines fail to generate raw hypothesis
# matrices with models in which the range of the predictors
# does not seem outlandish.
# 

#
# Comparing male and female wages, SLID  1994
#
# Raw splines give sensible results but not stable splines
#
library(gspline)
library(car)
library(lattice)
library(latticeExtra)

head(SLID)
dd <- na.omit(subset(SLID, select = - language))
quantile(dd$age)
quantile(dd$education)

# stable splines:

spage <- gspline(20:70, c(25,35,45), 2, 1) 
sped <- gspline(6:20,c(12,13,14), 1, 0)

# raw splines:

spageraw <- gspline(, c(25,35,45), 2, 1)
spedraw <- gspline(,c(12,13,14), 1, 0)

fit <- lm( log(wages) ~ (spage(age) + sped(education) + sex)^2, dd)
fitraw <- lm( log(wages) ~ (spageraw(age) + spedraw(education) + sex)^2, dd)



dd$fit <- predict(fit, newdata = dd)
dd$fitraw <- predict(fitraw, newdata = dd)

# fitted values with fitting data are consistent:
xyplot(fit ~ fitraw, dd)

# with new data
pred <- expandModelData(fit, 
                        expand.grid(
                          education = c(8,12,16,20),
                          age = seq(20,65),
                          sex = levels(dd$sex)))
pred <- subset(pred, .source == 'add')

pred$fit <- predict(fit, newdata = pred)
pred$fitraw <- predict(fitraw, newdata = pred)

# fit values on new data are incorrect
dd <- dd[order(dd$age),]

xyplot(fit ~ age | sex, dd, groups = education, type = 'l')
xyplot(fit ~ age | sex, pred, groups = education, type = 'l', auto.key = T)

# fit and fitraw are not consistent

xyplot(fit ~ fitraw, pred)

# fitraw produces reasonable looking output

xyplot(fitraw ~ age | sex, dd, groups = education, type = 'l')
xyplot(fitraw ~ age | sex, pred, groups = education, type = 'l', auto.key = T)


# new December 2019
# Dspline to estimate derivative of a spline
#
#' creates a list of terms for processing by Dspline
modelList <- function(model) {
      ts <- colnames(attr(terms(model),'factors'))
      ts <- strsplit(ts,":")
      ts <- lapply(seq_along(ts), 
      			 function(i) {
      			 	lapply(seq_along(ts[[i]]),
      			 		   function(j) paste0('M(', ts[[i]][j],")"))
      			 })
      ts <- lapply(ts, paste0, collapse = ' * ')
      ts <- lapply(ts, function(s) paste0("1 * ",s))
      ts <- c('1',ts)
      ts
}
#' Add rows to a model data frame
#' 
#' Facilitates creating rows of predictor values to graph
#' fitted values and standard errors for hypothetical
#' set of predictor values.
#' 
#' @param model whose model frame is used to obtain
#'        variable names and, particularly, correct
#'        levels for factors used as predictors in the model.
#' @param add a data frame with values of predictor variables. The
#'        default, NULL, returns only the model data frame.
#' @return a data frame concatenating the rows of the 
#'        model frame with those provided in 'add' with
#'        an additional variable '.source' with value
#'        'model' or 'add' to indicate the source of the row.
#'        To get only the added rows, use \code{subset(getD(model,add), .source == 'add')}.
#' @export
expandModelData <- function(model, add = NULL) {
  data <- getModelData(model)
  data$.source <- 'model'
  if(is.null(add)) return(data)
  add$.source <- 'add'
  merge(data, add, all = T)
}
#' Derivatives of a generalized parametric spline
#' 
#' For models that use a generalized parametric spline generated
#' by \code{\link{gspline}}, this function generates the linear hypothesis
#' matrix that is then used to estimate the value and standard error of
#' derivatives of the spline function.
#' 
#' This function makes a number of assumptions:
#' 
#' \itemize{
#' \item The variable that is the argument of the spline does not appear
#'   elsewhere than as the argument of the spline. 
#' \item The spline function may interact with other variables, but
#'   only variables that are not functions of the spline variable.
#' \item The spline function may appear more than once but only additively.
#' }
#' 
#' The function can be used to estimate the value of the spline by setting
#' the parameter, D (the order of the derivative) to 0. This can be useful
#' to obtain standard errors for a fitted spline model for which the 'predict'
#' generic function does not provide standard errors. 
#' 
#' @param model a fitted model that uses a generalized parametric spline.
#' @param spline the spline function used in the model with respect to whose
#'        argument the derivative needs to be obtained.
#' @param D the order of the derivative, an integer which can be 0 to
#'        obtain the expected value of the model. Default: 1.
#' @param limit specifies whether, at discontinuities, the limit should 
#'        be taken from the right (+1), from the left (-1), or whether the
#'        size of the discontinuity should be estimated. Default: +1.
#' @param data specified the data frame over which the model, or its
#'        derivative is evaluated. The default is the data frame
#'        of the model.
#' @return a data frame whose first column, names 'coef' is the estimated
#'        value of the derivative of the spline, the second column, named 'SE',
#'        is the estimated standard error, followed by the remmaning columns
#'        in 'data'.
#' @examples
#' 
#' library(gspline)
#' library(lattice)
#' library(latticeExtra)
#' dd <- Unemployment
#' 
#' head(dd)
#' xyplot(unemployment ~ date, dd)
#' 
#' dd <- within(dd, {
#'   years <- as.numeric(date - as.Date('2000-01-01'))/365.25
#' })
#' xyplot(unemployment ~ years, dd)
#' sp2 <- gspline(
#'   -5:20,
#'   knots = c(0,3,8.8,13,18),
#'   degree = 2,
#'   smooth = c(1,1,-1,1,1))
#' sp3 <- gspline(
#'   -5:20,
#'   knots = c(0,3,8.8,13,18),
#'   degree = 3,
#'   smooth = c(2,2,-1,2,2))
#' fit2 <- lm(unemployment ~ sp2(years), dd)
#' fit3 <- lm(unemployment ~ sp3(years), dd)
#' summary(fit2)
#' summary(fit3)
#' 
#' dd$fit2 <- predict(fit2)
#' dd$fit3 <- predict(fit3)
#' xyplot(unemployment ~ years, dd) +
#'   xyplot(fit2 ~ years, dd, type = 'l', col = 'red') +
#'   xyplot(fit3 ~ years, dd, type = 'l', col = 'blue') 
#' 
#' spper <- gspline(knots = c(3,6,9,12)/12, degree = 2, smooth = 1,periodic = TRUE)  
#' 
#' fit2p <- lm(unemployment ~ sp2(years) + spper(years), dd) 
#' fit3p <- lm(unemployment ~ sp3(years) + spper(years), dd)
#' dd$fit2p <- predict(fit2p)
#' dd$fit3p <- predict(fit3p)
#' 
#' xyplot(unemployment ~ years, dd) +
#'   xyplot(fit2p ~ years, dd, type = 'l', col = 'red') +
#'   xyplot(fit3p ~ years, dd, type = 'l', col = 'blue') 
#' 
#' dd <- within(dd, {
#'   res2 <- unemployment - fit2
#'   res3 <- unemployment - fit3
#'   res2p <- unemployment - fit2p
#'   res3p <- unemployment - fit3p
#' })
#' 
#' xyplot(res2 ~ years, dd, col = 'red', type = 'l')
#' xyplot(res3p ~ years, dd, col = 'red', type = 'l')
#' 
#' dd_deriv <- Dspline(fit3, sp3)
#' head(dd_deriv)
#' 
#' xyplot(coef ~ years, dd_deriv, type = 'l') 
#' 
#' xyplot(coef ~ years, dd_deriv, type = 'l',
#'        fit = dd_deriv$coef,
#'        upper = dd_deriv$coef + dd_deriv $ se,
#'        lower = dd_deriv$coef - dd_deriv $ se,
#'        subscript = T) +
#'   layer(panel.fit(...))
#' 
#' dd_deriv2 <- Dspline(fit3, sp3, D = 2)
#' 
#' xyplot(coef ~ years, dd_deriv2, type = 'l',
#'        fit = dd_deriv2$coef,
#'        upper = dd_deriv2$coef + dd_deriv2 $ se,
#'        lower = dd_deriv2$coef - dd_deriv2 $ se,
#'        subscript = T) +
#'   layer(panel.fit(...))
#' 
#' # since the splines are additive
#' 
#' dd_per3 <- Dspline(fit3p, sp3, D = 1)
#' dd_per3p <- Dspline(fit3p, spper, D = 1)
#' 
#' # To compute the combined SEs we would need to add the Lmatrices
#' # and use the wald function with that
#' 
#' dd_per3 <- within(dd_per3, {
#'   coefper <- dd_per3p$coef
#'   coefdetrended <- coef
#'   coefcombined <- coefdetrended + coefper 
#'   date <- dd$date
#' })
#' 
#' xyplot(coefcombined ~ date, dd_per3, type = 'l')+
#' xyplot(coefper ~ date, dd_per3, type = 'l', col = 'black')+
#' xyplot(coefdetrended ~ date, dd_per3, type = 'l', col = 'red')
#' @export
Dspline <- function(fit, spline, D = 1, limit = 1, data = getModelData(fit)) {
	mlist <- modelList(fit)
	spline_name <- deparse(substitute(spline))
	arg <- paste('D = ', D, ', limit = ', limit)
	spname_match <- paste0(' M\\(', spline_name, '\\(')
	mlist <- lapply(mlist, function(s) {
		if(grepl(spname_match, s)) { 
			sub(
				paste0('(',spname_match,"[^)]+",')'),
				paste0('\\1, ', arg), s) 
		} else if(D > 0) { 
			sub("^1","0", s) 
		} else {
			s
		}
	})
	mlist <- paste(unlist(mlist), collapse = ', ')
	mlist <- paste('list(',mlist,')')
	L <- do.call(cbind, eval(parse(text=mlist), data))
	cbind(as.data.frame(wald(fit,L)), data)
}

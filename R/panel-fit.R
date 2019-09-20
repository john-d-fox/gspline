# added by G. Monette 2019-07-08
# last modified by 2019-07-08 by J. Fox



#' Panel function for predicted values and SE bands
#' 
#' Panel function to display predicted values and SE bands using
#' \code{\link[lattice]{xyplot}} in in \pkg{lattice} and
#' \code{\link[latticeExtra]{layer}} or \code{glayer} in \pkg{latticeExtra}
#' 
#' With \code{\link[latticeExtra]{layer}} and \code{glayer} in
#' \pkg{latticeExtra}, \code{panel.fit} can be used to generate fitted values
#' and confidence or prediction bands that have render correctly whether a plot
#' uses \var{groups} or not.
#' 
#' If a data frame, \code{data}, is used to fit a model and contains variables
#' for fitted values and standard error for estimation or prediction, a typical
#' use of \code{panel.fit} has the following form: \verb{
#' 
#' library(latticeExtra) xyplot( y ~ x, data, groups = g, fit = data$yhat,
#' lower = with(data, yhat - 2*se), upper = with(data, yhat + 2*se), subscripts
#' = T) + glayer(panel.fit(...)) }
#' 
#' @param x,y these are place holders for the arguments that are automatically
#' passed to panel functions from \code{\link[lattice]{xyplot}} through
#' \code{\link[latticeExtra]{layer}} or \code{glayer}. Their values are not
#' used by \code{panel.fit}
#' @param fit fitted values defined as an argument named \code{fit} in the call
#' to \code{\link[lattice]{xyplot}} and usually passed to \code{panel.fit}
#' through \code{\link[latticeExtra]{layer}} or \code{glayer}
#' @param lower,upper lower and upper limits of error bands, passed in the same
#' way as \code{fit}
#' @param subscripts logical, passed \code{\link[lattice]{xyplot}}
#' @param ... specify any additional arguments to prevent them from being
#' silently passed from \code{\link[lattice]{xyplot}} to \code{panel.fit}
#' @param group.number passed silently from \code{\link[lattice]{xyplot}}
#' @param alpha transparency of the bands, in the interval [0,1]. Provide a
#' value or it will be passed silently from \code{\link[lattice]{xyplot}}
#' @param col color, may be specified, otherwise passed from
#' \code{\link[lattice]{xyplot}}
#' @param col.line line color, if \code{group.number} isn't specified.
#' @param col.symbol color when using groups,may be specified, otherwise passed
#' from \code{\link[lattice]{xyplot}}
#' @param border if TRUE draw borders on bands. Default is FALSE
#' @param font may be specified, otherwise passed from
#' \code{\link[lattice]{xyplot}}
#' @param fontface may be specified, otherwise passed from
#' \code{\link[lattice]{xyplot}}
#' @param type currently not used.
#' @return The \code{panel.fit},is invoked for its graphical effect.
#' @author Georges Monette <georges@@yorku.ca>
#' @examples
#' 
#' \dontrun{
#'   
#'   ###
#'   ### Exploring possible discontinuity in value of post-secondary education
#'   ###
#'  	 
#' }
#' \dontrun{
#' trellis.focus()
#' panel.identify(labels = rownames(data),rot=-15,col = col.symbol, etc.)
#' trellis.unfocus()
#' }
#' 
#' @export panel.fit
panel.fit <-
  function(x, y, fit, lower, upper,
           subscripts, ..., type, group.number, alpha, col, col.line, col.symbol, border = F, font, fontface)
  {
    if( !missing(fit)) {
      if( missing(col) ) col <- 'blue'
      if( !missing(group.number)) {
        col <- col.line
      }
      if( !missing(subscripts) ) {
        fit <- fit[subscripts]
      }
      dd <- data.frame( x=x, fit = fit)
      dd <- dd[order(dd$x),]
      panel.lines( dd$x, dd$fit, ..., col = col, type = 'l')
    }
    if( !missing(lower)){
      if( missing(alpha) || alpha == 1) alpha <- .3
      if( missing(col) ) col <- 'blue'
      if( !missing(group.number)) {
        col <- col.symbol
      }
      if( !missing(subscripts) ) {
        upper <- upper[subscripts]
        lower <- lower[subscripts]
      }
      dd <- data.frame( x=x, lower = lower, upper = upper)
      dd <- dd[order(dd$x),]
      panel.polygon( c(dd$x, rev(dd$x)),c(dd$upper, rev(dd$lower)),
                     border = border, col = col, alpha = alpha,...)
    }
    #  panel.polygon(c(dd$x, rev(dd$x)), c(dd$upper, rev(dd$lower)), col = col, alpha = alpha, ...)
  }

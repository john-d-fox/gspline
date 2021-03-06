% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_gpanel.R
\name{panel.dell}
\alias{panel.dell}
\title{Draw data ellipse}
\usage{
panel.dell(x, y, radius = 1, subscripts, type = "l", ...)
}
\arguments{
\item{x, y}{points to generate data ellipse}

\item{radius}{default 1}

\item{...}{NOTE: may specify anything you don't want passed through ...}
}
\description{
This is an experiment in writing a function that can be
called via layer or glayer without further complications
e.g. \code{xyplot(......,labels = rownames(data)) + layer(panel.dell(...))}
or  \code{xyplot(....., labels = rownames(data), subscripts = T) + glayer(panel.dell(...))}.
}
\examples{
\dontrun{
library(latticeExtra)
xyplot(mathach ~ ses, hs) + layer(panel.dell(..., col='red',lwd=2))
xyplot(mathach ~ ses | school, hs) + layer(panel.dell(...))
xyplot(mathach ~ ses , hs, groups = school) + glayer(panel.dell(...,lwd=2))
}
}

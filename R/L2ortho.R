L2ortho <- function(fit, L) {
    fitted_mm <- model.matrix(fit)	
    on.exit(inwald(inwald()))
    inwald(TRUE)
    fitraw <- update(fit)
    raw_mm <- model.matrix(fitraw)
    G <- lm.fit(raw_mm, fitted_mm)$coefficients
    ret <- L %*% G
    attr(ret,'G') <- G
    ret
}
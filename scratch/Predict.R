Predict <- function(model, newdata, se.fit=FALSE,  ...){
  if (missing(newdata)){
    predict(model, se.fit=se.fit, ...)
  } else {
    wald.result <- suppressWarnings(wald(model, L.=getX(model, data=newdata))[[1]]$estimate)
    if (se.fit) wald.result[, "Estimate", "Std.Error"] else wald.result[, "Estimate"]
  }
}
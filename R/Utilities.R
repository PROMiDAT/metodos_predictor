

select_on_class <- function(.data, clases = "numeric") {
  .data[, sapply(.data, function(vec, clss) class(vec) %in% clss, clss = clases), drop  = FALSE]
}

contr.dummy <- function (n, contrasts = TRUE) {
  if (length(n) <= 1) {
    if (is.numeric(n) && length(n) == 1 && n > 1)
      levels <- 1:n
    else stop("contrasts are not defined for 0 degrees of freedom")
  }
  else levels <- n
  lenglev <- length(levels)
  cont <- array(0, c(lenglev, lenglev), list(levels, levels))
  cont[col(cont) == row(cont)] <- 1
  cont
}

contr.ordinal <- function (n, contrasts = TRUE) {
  if (length(n) <= 1) {
    if (is.numeric(n) && length(n) == 1 && n > 1)
      levels <- 1:n
    else stop("contrasts are not defined for 0 degrees of freedom")
  }
  else levels <- n
  lenglev <- length(levels)
  cont <- array(0.5, c(lenglev, lenglev - 1), list(levels,
                                                   NULL))
  cont[lower.tri(cont)] <- -0.5
  cont
}

max.col <- function(m){
  base::max.col(apply(m, 1, function(x) max(x, na.rm = TRUE)) == m)
}

numeric.to.predict <- function(predic.var = NULL){
  real <- .data$test[, .data$var.pred]
  if(is.numeric(predic.var)){
    for(nom in unique(real)) {
      nom.num <- unique(real)
      nom.num <- as.numeric(nom.num)[nom.num == nom]
      predic.var[predic.var==nom.num] <- nom
    }
  }
  predic.var
}

type.correction <- function(prediction, fix){
  var_aux <- .data$data[,.data$var.pred]
  if(!is.numeric(var_aux) && !is.integer(var_aux) && fix){
    factor(as.character(prediction), levels = levels(var_aux))
  }else{
    prediction
  }
}

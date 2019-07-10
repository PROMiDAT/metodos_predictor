
#' get_test_less_predict
#'
#' @keywords internal
#'
select_on_class <- function(.data, clases = "numeric") {
  .data[, sapply(.data, function(vec, clss) class(vec) %in% clss, clss = clases), drop  = FALSE]
}

#' contr.dummy
#'
#' @keywords internal
#'
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

#' contr.ordinal
#'
#' @keywords internal
#'
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

#' max_col
#'
#' @keywords internal
#'
max_col <- function(m){
  base::max.col(apply(m, 1, function(x) max(x, na.rm = TRUE)) == m)
}

#' numeric_to_predict
#'
#' @keywords internal
#'
numeric_to_predict <- function(real, predic.var = NULL){
  if(is.numeric(predic.var)){
    for(nom in unique(real)) {
      nom.num <- unique(real)
      nom.num <- as.numeric(nom.num)[nom.num == nom]
      predic.var[predic.var==nom.num] <- nom
    }
  }
  predic.var
}

#' type_correction
#'
#' @keywords internal
#'
type_correction <- function(model, prediction, fix){
  var_type <- model$prmdt$type
  .levels  <- model$prmdt$levels
  if(var_type != "numeric" && var_type != "integer" && fix){
    factor(as.character(prediction), levels = .levels)
  }else{
    prediction
  }
}

#' original_model
#'
#' @keywords internal
#'
original_model <- function(x){
  class(x) <- class(x)[-(1:2)]
  x$prmdt <- NULL
  return(x)
}

#' get_test_less_predict
#'
#' @keywords internal
#'
get_test_less_predict <- function(data, var.pred){
  data[,-which(colnames(data) == var.pred)]
}


#' Printing prmdt models
#'
#' @param x A prmdt models
#' @param ... optional arguments to print o format method
#'
#' @export
print.prmdt <- function(x, ...){
  print(original_model(x), ...)
}

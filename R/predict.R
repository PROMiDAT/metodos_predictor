
predict.ada <- function(object, type = "class", n.iter = NULL, ...){
  type <- ifelse(type == "class", "vector", type)
  ans <- ada:::predict.ada(object, get.test.less.predict(), type, n.iter, ...)
  type.correction(ans, type == "vector")
}

predict.naiveBayes <- function(object, type = "class", threshold = 0.001, eps = 0, ...){
  type <- ifelse(type == "prob", "raw", type)
  ans <- e1071:::predict.naiveBayes(object, get.test.less.predict(), type, threshold, eps, ...)
  type.correction(ans, type == "class")
}

predict.train.kknn <- function(object, type = "class", ...){
  type <- ifelse(type == "class", "raw", type)
  ans <- kknn:::predict.train.kknn(object, type = type, get.test.less.predict(), ...)
  type.correction(ans, type == "raw") # ojo
}

predict.nnet <- function(object, type = "class", ...){
  type <- ifelse(type == "prob", "raw", type)
  ans <- nnet:::predict.nnet(object,  get.test.less.predict(), type, ...)
  type.correction(ans, type == "class")
}

predict.randomForest <- function(object, type = "class", norm.votes = TRUE, predict.all = FALSE, proximity = FALSE, nodes = FALSE, cutoff, ...){
  type <- ifelse(type == "class", "response", type)
  ans <- randomForest:::predict.randomForest(object, get.test.less.predict(), type, norm.votes, predict.all, proximity, nodes, cutoff, ...)
  type.correction(ans, type == "response")
}

predict.rpart <- function(object, type = "class", na.action = na.pass, ...){
  ans <- rpart:::predict.rpart(object, .data$test, type, na.action, ...)
  type.correction(ans, type == "class")
}

predict.svm <- function(object, decision.values = FALSE, probability = FALSE, ..., na.action = na.omit){
  ans <- e1071:::predict.svm(object, get.test.less.predict(), decision.values, probability, ..., na.action = na.action)
  type.correction(ans,  probability == FALSE)
}

predict.xgb.Booster <- function(object, type = "class", missing = NA, outputmargin = FALSE, ntreelimit = NULL, predleaf = FALSE, predcontrib = FALSE,
                                approxcontrib = FALSE, predinteraction = FALSE, reshape = FALSE, ...){

  test_aux <- .data$test %>% select_on_class(c("numeric","integer", "factor"))
  test_aux[] <- lapply(test_aux, as.numeric)

  if(min(test_aux[,.data$var.pred]) != 0){
    test_aux[,.data$var.pred]  <- test_aux[,.data$var.pred]  - 1
  }

  selector <- which(colnames(test_aux) == .data$var.pred)
  test_aux  <- xgb.DMatrix(data = data.matrix(test_aux[,-selector]), label = data.matrix(test_aux[,selector]))

  ans <- xgboost:::predict.xgb.Booster(object, test_aux, missing, outputmargin, ntreelimit, predleaf, predcontrib, approxcontrib, predinteraction, reshape, ...)

  num.class <- length(levels(.data$data[,.data$var.pred]))

  if(type == "class"){
    if(num.class > 2){
      ans <- max.col(matrix(ans, ncol = num.class, byrow = TRUE))
    }else{
      ans <- ifelse(ans > 0.5, 2, 1)
    }
    ans <- numeric.to.predict(ans)
  }

  if(type == "prob"){
    ans <- matrix(ans, ncol = num.class, byrow = TRUE)
  }

  type.correction(ans, type == "class")
}


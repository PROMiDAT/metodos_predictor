
train.rpart <- function(formula, weights, subset, na.action = na.rpart, method, model = FALSE, x = FALSE, y = TRUE, parms, control, cost, ...){
  if(missing(weights) & missing(subset)){
    model <- rpart(formula = formula, data = .data$train, na.action = na.action, method = method, model = model,
                   x = x , y = y , parms = parms, control = control, cost = cost, ... = ...)
  }else{
    if((!missing(weights)) & (!missing(subset))){
      model <- rpart(formula = formula, data = .data$train, weights = weights, subset = subset, na.action = na.action, method = method, model = model,
                     x = x , y = y , parms = parms, control = control, cost = cost, ... = ...)
    }else{
      if(!missing(weights)){
        model <- rpart(formula = formula, data = .data$train, weights = weights, na.action = na.action, method = method, model = model,
                       x = x , y = y , parms = parms, control = control, cost = cost, ... = ...)
      }else{
        model <- rpart(formula = formula, data = .data$train, subset = subset, na.action = na.action, method = method, model = model,
                       x = x , y = y , parms = parms, control = control, cost = cost, ... = ...)
      }
    }
  }
  return(model)
}

train.bayes <- function(formula, laplace = 0, ..., subset, na.action = na.pass){
  if(missing(subset)){
    naiveBayes(formula = formula, data = .data$train, laplace = laplace, ... = ..., na.action = na.action)
  }else{
    naiveBayes(formula = formula, data = .data$train, laplace = laplace, ... = ..., subset = subset, na.action = na.action)
  }
}

train.randomForest <- function(formula, ..., subset, na.action = na.fail){
  if(missing(subset)){
    randomForest(formula = formula, data = .data$train, ... = ..., na.action = na.action)
  }else{
    randomForest(formula = formula, data = .data$train, ... = ..., subset = subset, na.action = na.action)
  }
}

train.knn <- function(formula, kmax = 11, ks = NULL, distance = 2, kernel = "optimal", ykernel = NULL,
                      scale = TRUE, contrasts = c('unordered' = "contr.dummy",ordered = "contr.ordinal"), ...){
  train.kknn(formula = formula, data = .data$train, kmax = kmax, ks = ks, distance = distance, kernel = kernel,  ykernel = ykernel,
                   scale =  scale, contrasts = contrasts, ordered = ordered, ... = ...)
}

train.ada <- function(formula, ..., subset, na.action = na.rpart){
  if(missing(subset)){
    ada(formula = formula, data = .data$train, ... = ..., na.action = na.action)
  }else{
    ada(formula = formula, data = .data$train, ... = ..., subset = subset, na.action = na.action)
  }
}

train.nnet <- function(formula, weights, ..., subset, na.action, contrasts = NULL){
  if(missing(weights) & missing(subset)){
    model <- nnet(formula = formula, data = .data$train, ... =  ..., na.action = na.action, contrasts = contrasts)
  }else{
    if((!missing(weights)) & (!missing(subset))){
      assign("sub_set_aux", subset, envir = .GlobalEnv)
      model <- nnet(formula = formula, data = .data$train, weights = weights, ... =  ..., subset =  sub_set_aux, na.action = na.action, contrasts = contrasts)
      rm("sub_set_aux", envir = .GlobalEnv)
    }else{
      if(!missing(weights)){
        model <- nnet(formula = formula, data = .data$train, weights = weights, ... =  ..., na.action = na.action, contrasts = contrasts)
      }else{
        assign("sub_set_aux", subset, envir = .GlobalEnv)
        model <- nnet(formula = formula, data = .data$train, ... =  ..., subset =  sub_set_aux, na.action = na.action, contrasts = contrasts)
        rm("sub_set_aux", envir = .GlobalEnv)
      }
    }
  }
  return(model)
}

train.svm <- function(formula, ..., subset, na.action = na.omit, scale = TRUE){
  if(missing(subset)){
    svm(formula = formula, data = .data$train, ... = ..., na.action = na.action, scale = scale)
  }else{
    svm(formula = formula, data = .data$train, ... = ..., subset = subset, na.action = na.action, scale = scale)
  }
}

train.xgboost <- function(nrounds, watchlist = list(), obj = NULL, feval = NULL,
                          verbose = 1, print_every_n = 1L, early_stopping_rounds = NULL, maximize = NULL,
                          save_period = NULL, save_name = "xgboost.model", xgb_model = NULL, callbacks = list(),
                          early_stop_round = 10, eval_metric = "mlogloss",extra_params = NULL, booster = "gbtree",
                          objective = NULL, eta = 0.3, gamma=0, max_depth = 6, min_child_weight = 1, subsample = 1,
                          colsample_bytree = 1, ...){

  train_aux <- .data$train %>% select_on_class(c("numeric","integer", "factor"))
  test_aux <- .data$test %>% select_on_class(c("numeric","integer", "factor"))

  train_aux[] <- lapply(train_aux, as.numeric)
  test_aux[] <- lapply(test_aux, as.numeric)

  if(min(train_aux[,.data$var.pred]) != 0){
    train_aux[,.data$var.pred] <- train_aux[,.data$var.pred] - 1
    test_aux[,.data$var.pred]  <- test_aux[,.data$var.pred]  - 1
  }

  selector <- which(colnames(train_aux) == .data$var.pred)

  train_aux <- xgb.DMatrix(data = data.matrix(train_aux[,-selector]), label = data.matrix(train_aux[,selector]))
  test_aux  <- xgb.DMatrix(data = data.matrix(test_aux[,-selector]), label = data.matrix(test_aux[,selector]))

  if(length(watchlist) == 0){
    watchlist <- list(train = train_aux, test = test_aux)
  }

  num.class <- length(levels(.data$data[,.data$var.pred]))

  if(is.null(extra_params)){
    if(is.null(objective)){
      objective <- ifelse(num.class == 2, "binary:logistic", "multi:softprob")
    }
    params <- list(booster = booster, objective = objective, eta = eta, gamma = gamma, max_depth = max_depth,
                   min_child_weight = min_child_weight, subsample = subsample, colsample_bytree = colsample_bytree)
  }else{
    params <- extra_params
  }

  if(num.class > 2){
    params$num_class <- num.class
    xgb.train(params = params, data = train_aux, early_stop_round = 10, eval_metric = "mlogloss", nrounds = nrounds, watchlist = watchlist,
              obj = obj, feval = feval, verbose = verbose, print_every_n = print_every_n, early_stopping_rounds = early_stopping_rounds,
              maximize = maximize, save_period = save_period, save_name = save_name, xgb_model = xgb_model, callbacks = callbacks, ... = ...)
  }else{
    xgb.train(params = params, data = train_aux, nrounds = nrounds, watchlist = watchlist, obj = obj, feval = feval, verbose = verbose,
              print_every_n = print_every_n, early_stopping_rounds = early_stopping_rounds, maximize = maximize, save_period = save_period,
              save_name = save_name, xgb_model = xgb_model, callbacks = callbacks, ... = ...)
  }

}

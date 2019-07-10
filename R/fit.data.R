#
# .data           <- new.env()
# .data$data      <- NULL
# .data$var.pred  <- NULL
# .data$train     <- NULL
# .data$test      <- NULL
# .data$data.rate <- NULL
#
# fit.data <- function(data, var.predict, rate, train, test){
#   aux_var <- data[,var.predict]
#   data[,var.predict] <- NULL
#   data[,var.predict] <- aux_var
#
#   if(!missing(rate) && is.numeric(rate)){
#     if(rate < 1 & rate > 0){
#       n <- seq_len(nrow(data))
#       .sample <- sample(n, length(n) * rate)
#       train <- data[.sample,]
#       test <- data[.sample,]
#     }else{
#       stop("The rate has to be a value between 0 and 1", call. = FALSE)
#     }
#   }else{
#     if(missing(train) || missing(test)){
#       stop("If the rate is not specified, the learning and test data have to be entered", call. = FALSE)
#     }
#   }
#   .data$data      <- data
#   .data$var.pred  <- var.predict
#   .data$train     <- train
#   .data$test      <- test
#   .data$data.rate <- rate
# }
#
# get.test.less.predict <- function(){
#   .data$test[,-ncol(.data$test)]
# }

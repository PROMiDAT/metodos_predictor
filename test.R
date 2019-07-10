library(Prueba)
options(scipen=5)

############# ================================================================== ###################
#############                           2 categorías                             ###################
############# ================================================================== ###################

datos <- na.omit(read.csv("~/Desktop/Datos/titanicLimpio.csv"))

n <- seq_len(nrow(datos))
.sample <- sample(n, length(n) * 0.75)
data.train <- datos[.sample,]
data.test <- datos[.sample,]

real <- data.test$Survived

#### --------------------------------- ADA BOOSTING

( modelo.ada <- train.ada(Survived~., data.train) )
prob <- predict(modelo.ada, data.test , type = "prob")
( prediccion <- predict(modelo.ada, data.test , type = "class") )
table(real, prediccion)

#### --------------------------------- BAYES

( modelo.bayes <- train.bayes(Survived~., data.train) )
prob <- predict(modelo.bayes, data.test , type = "prob")
( prediccion <- predict(modelo.bayes, data.test , type = "class") )
table(real, prediccion)

#### --------------------------------- KNN

( modelo.knn <- train.knn(Survived~., data.train) )
prob <- predict(modelo.knn, data.test , type = "prob")
( prediccion <- predict(modelo.knn, data.test , type = "class") )
table(real, prediccion)

#### --------------------------------- NNET

( modelo.nn <- train.nnet(Survived~., data.train, size = 20) )
prob <- predict(modelo.nn, data.test , type = "prob")
( prediccion <- predict(modelo.nn, data.test, type = "class") )
table(real, prediccion)

#### --------------------------------- NEURALNET

( modelo.neuralnet <- train.neuralnet(Survived~., data.train, hidden = c(2,2,2,2),
                                      linear.output = FALSE,threshold = 0.5, stepmax = 5000) )
prob <- predict(modelo.neuralnet, data.test, type = "prob")
( prediccion <- predict(modelo.neuralnet, data.test , type = "class") )
table(real, prediccion)

#### --------------------------------- RANDOMFOREST

( modelo.rf <- train.randomForest(Survived~., data.train) )
prob <- predict(modelo.rf, data.test , type = "prob")
( prediccion <- predict(modelo.rf, data.test , type = "class") )
table(real, prediccion)

#### --------------------------------- RPART

( modelo.rpart <- train.rpart(Survived~., data.train) )
prob <- predict(modelo.rpart, data.test , type = "prob")
( prediccion <- predict(modelo.rpart, data.test  , type = "class") )
table(real, prediccion)

#### --------------------------------- SVM

( modelo.svm <- train.svm(Survived~., data.train) )
prob <- predict(modelo.svm, data.test , type = "prob")
( prediccion <- predict(modelo.svm, data.test , type = "class") )
table(real, prediccion)

#### --------------------------------- XG BOOSTING

( modelo.xg <- train.xgboost(Survived~., data.train, nrounds = 79, eval_metric = "error") ) # Error
prob <- predict(modelo.xg, data.test, type = "prob")
( prediccion <- predict(modelo.xg, data.test, type = "class") )
table(real, prediccion)

############# ================================================================== ###################
#############                           3 categorías                             ###################
############# ================================================================== ###################

data("iris")

n <- seq_len(nrow(iris))
.sample <- sample(n, length(n) * 0.75)
data.train <- iris[.sample,]
data.test <- iris[.sample,]

real <- data.test$Species

#### --------------------------------- BAYES

( modelo.bayes <- train.bayes(Species ~., data.train) )
prob <- predict(modelo.bayes, data.test, type = "prob")
( prediccion <- predict(modelo.bayes, data.test, type = "class") )
table(real, prediccion)

#### --------------------------------- KNN

( modelo.knn <- train.knn(Species~., data.train) )
prob <- predict(modelo.knn, data.test, type = "prob")
( prediccion <- predict(modelo.knn, data.test, type = "class") )
table(real, prediccion)

#### --------------------------------- NNET

( modelo.nn <- train.nnet(Species~., data.train, size = 20) )
prob <- predict(modelo.nn, data.test, type = "prob")
( prediccion <- predict(modelo.nn, data.test, type = "class") )
table(real, prediccion)

#### --------------------------------- NEURALNET

( modelo.neuralnet <- train.neuralnet(Species~., data.train,hidden = c(10, 14, 13),
                                     linear.output = FALSE, threshold = 0.01, stepmax = 1e+06) )
prob <- predict(modelo.neuralnet, data.test, type = "prob")
( prediccion <- predict(modelo.neuralnet, data.test, type = "class") )
table(real, prediccion)

#### --------------------------------- RANDOMFOREST

( modelo.rf <- train.randomForest(Species~., data.train) )
prob <- predict(modelo.rf, data.test, type = "prob")
( prediccion <- predict(modelo.rf, data.test, type = "class") )
table(real, prediccion)

#### --------------------------------- RPART

( modelo.rpart <- train.rpart(Species~., data.train) )
prob <- predict(modelo.rpart, data.test, type = "prob")
( prediccion <- predict(modelo.rpart, data.test, type = "class") )
table(real, prediccion)

#### --------------------------------- SVM

( modelo.svm <- train.svm(Species~., data.train) )
prob <- predict(modelo.svm, data.test , type = "prob")
( prediccion <- predict(modelo.svm, data.test , type = "class") )
table(real, prediccion)

#### --------------------------------- XG BOOSTING

( modelo.xg <- train.xgboost(Species~., data.train, nrounds = 79, maximize = FALSE) )
prob <- predict(modelo.xg, data.test, type = "prob")
( prediccion <- predict(modelo.xg, data.test, type = "class") )
table(real, prediccion)


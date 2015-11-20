kfoldCV <- function(fm, data, Y, k, Ncv, family){
  # function to run a k fold CV over Ncv times
  # gives cross-validation log-likelihood for test data (sum over folds)
  # SH 07-10-2014
  
  # prepare output
  llCV <- numeric(Ncv)
  
  # prepare input
  model.data <- data
  #Y <- model.data[, as.character(fm$call[[2]][2])]
  #if (!is.numeric(model.data[, as.character(fm$call[[2]][2])])) Y <- as.numeric(model.data[, as.character(fm$call[[2]][2])]) -1
  Ndata <- NROW(model.data)
  
  # run CV
  if(family == "gaussian"){
    for(t in 1:Ncv){
      # portion data
      fold.nr <- sample(rep(1:k, length=Ndata))
      # cbind data and fold.nr
      dataplusfold <- cbind(model.data, fold.nr) 
      
      testLL <- numeric(k)
      
      for (i in 1:k){
        # update model (trainingData)
        train <- update(fm, data = dataplusfold[fold.nr != i, ])
        
        # varying process for various model classes
        # GLM, GAM or ANN
        if(as.character(fm$call[1]) == "glm" | as.character(fm$call[1]) == "gam" | as.character(fm$call[1]) == "nnet.formula"){
          # standard deviation of residuals (trainingData)
          sd.resid <- sqrt(sum((dataplusfold[fold.nr != i, as.character(fm$call[[2]][2])] - train$fitted.values)^2 )  /nrow(dataplusfold[fold.nr != i, ]))
          # predict model for testData
          test <- predict(train, newdata = dataplusfold[fold.nr == i, ])
        }
        # randomForest
        if(as.character(fm$call[1]) == "randomForest" ){
          # standard deviation of residuals (trainingData)
          sd.resid <- sqrt(sum((dataplusfold[fold.nr != i, as.character(fm$call[[2]][2])] - train$predicted)^2 )  /nrow(dataplusfold[fold.nr != i, ]))
          # predict model for testData
          test <- predict(train, newdata = dataplusfold[fold.nr == i, ], type = "response")
        }
        # BRT
        if(as.character(fm$call[1]) == "gbm" ){
          # standard deviation of residuals (trainingData)
          sd.resid <- sqrt(sum((dataplusfold[fold.nr != i, as.character(fm$call[[2]][2])] - train$fit)^2 )  /nrow(dataplusfold[fold.nr != i, ]))
          # predict model for testData
          test <- predict(train, newdata = dataplusfold[fold.nr == i, ], n.trees = gbm.perf(train, plot.it=F))
        }
        
        # compute test LL
        testLL[i] <- sum(dnorm(dataplusfold[fold.nr == i, as.character(fm$call[[2]][2])], mean = test, sd = sd.resid, log = T))        
      }
      llCV[t] <- sum(testLL)
    }
  }
  #
  if(family == "binomial"){
    for(t in 1:Ncv){
      # portion data
      # draw 1s and 0s separately:
      zeros <- which(Y == 0)
      ones <- which(Y == 1)
      fold.nr <- 1:3 # create a vector to write fold.nr in
      fold.nr[zeros] <- sample(rep(1:k, length=length(zeros)))
      fold.nr[ones] <- sample(rep(1:k, length=length(ones)))
      # cbind data and fold.nr
      dataplusfold <- cbind(model.data, fold.nr)
      
      testLL <- numeric(k)
      
      for (i in 1:k){
        # update model (trainingData)
        train <- update(fm, data = dataplusfold[fold.nr != i, ])
        
        # varying process for various model classes
        # GLM and GAM
        if(as.character(fm$call[1]) == "glm" | as.character(fm$call[1]) == "gam"){
          # predict model for test elements
          test <- predict(train, newdata = dataplusfold[fold.nr == i, ], type = "response")
        }
        # randomForest
        if(as.character(fm$call[1]) == "randomForest"){
          # predict model for test elements
          if(fm$type == "regression"){
            test <- predict(train, newdata = dataplusfold[fold.nr == i, ], type = "response")
          }else{
            test <- predict(train, newdata = dataplusfold[fold.nr == i, ], type = "prob")[,2]
          }
        }
        # ANN
        if(as.character(fm$call[1]) == "nnet.formula"){
          # predict model for test elements
          test <- predict(train, newdata = dataplusfold[fold.nr == i, ])
        }
        # BRT
        if(as.character(fm$call[1]) == "gbm"){
          # predict model for test elements
          test <- predict(train, newdata = dataplusfold[fold.nr == i, ], type = "response", n.trees = gbm.perf(train, plot.it = F))
        }
        # compute test LL
        control <- dbinom(as.numeric(Y[fold.nr == i]), size = 1, prob = test, log = T)
        control[which(is.infinite(control))] <- NA
        testLL[i] <- sum(control, na.rm = T)
      }
      llCV[t] <- sum(testLL)
    }
  }
  return(llCV)
}
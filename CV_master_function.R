library(caret)
library(MASS)
library(e1071)
CVmaster <- function(clas_func, features, labels, K, loss_func, split_opt){
    if (split_opt == 1){
      blocks <- unique(train_valid_set1$block)
      folds <- createFolds(blocks, k = K)
      currFormula <- as.formula(paste(labels, "~",
                                      paste(features[], collapse = "+"), sep = ""))
      loss = rep(0, K)
      for (i in 1:K){
        Val <- train_valid_set1[train_valid_set1$block %in% folds[[i]],]
        Train <- train_valid_set1[!train_valid_set1$block %in% folds[[i]],]
        if(clas_func == "glm"){
          model <- glm(currFormula, data = Train, family = "binomial")
        }
        if(clas_func == "lda"){
          model <- lda(currFormula, data = Train)
        }
        if(clas_func == "qda"){
          model <- qda(currFormula, data = Train)
        }
        if(clas_func == "nb"){
          model <- naiveBayes(currFormula, data = Train)
        }
        pred <- predict(model, Val)
        true <- Val$V3
        if(clas_func == "glm"){
          pred$class <- pred > 0.5
        }
        if(clas_func == "nb"){
          pred$class <- pred
        }
        loss[i] = mean(pred$class != true)
      }
      return (loss)
    }
    if (split_opt == 2){
      blocks <- unique(train_valid_set2$block)
      folds <- createFolds(blocks, k = K)
      currFormula <- as.formula(paste(labels, "~",
                                      paste(features[], collapse = "+"),
                                      sep = ""))
      loss = rep(0, K)
      for (i in 1:K){
        Val <- train_valid_set2[train_valid_set2$block %in% folds[[i]],]
        Train <- train_valid_set2[!train_valid_set2$block %in% folds[[i]],]
        if(clas_func == "glm"){
          model <- glm(currFormula, data = Train, family = "binomial")
        }
        if(clas_func == "lda"){
          model <- lda(currFormula, data = Train)
        }
        if(clas_func == "qda"){
          model <- qda(currFormula, data = Train)
        }
        pred <- predict(model, Val)
        true <- Val$V3
        if(clas_func == "glm"){
          pred$class <- pred > 0.5
        }
        if(clas_func == "nb"){
          pred$class <- pred
        }
        loss[i] = mean(pred$class != true)
      }
      return (loss)
    }
}
MSE <- function(a, b){
  return (mean((a-b)^2))
}

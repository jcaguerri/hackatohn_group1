load("datchile.RData")
library(tidyverse)
library(caret)
                  #MODELS: 1
#Preprocesado
datp <- datchile %>% select(starts_with("P5"), P2_1_1) %>% na.omit() #to select variables
datp2 <- datp %>% filter_all(all_vars( .<85)) #clean answers
datp3 <- datp2 %>% mutate(seguridad = case_when(
  .$P2_1_1 %in% c("1", "2")~"inseguro",
  .$P2_1_1 %in% c("3", "4") ~"seguro")) #mutate variables 
datp4 <- datp3%>% mutate(seguridad = as_factor(seguridad)) %>% select(-"P2_1_1") #drop transformed varaible

# TO CREATE THE PARTITION BETWEEN TEST AND VALIDATION SETS
set.seed(1992)
test_index <- createDataPartition(y = datp4$seguridad, times = 1, p = 0.2, list = FALSE)
train <- datp4[-test_index,]
test <- datp4[test_index,]

#GLM MODEL
set.seed(1992)
glm_model <- train(seguridad~., data=train, method= "glm",
                   trControl = trainControl(method = "cv", 
                                            number=5, 
                                            p=0.9)) #Choose crossvalidation with
#5 k-folds
y_hat_glm <- predict(glm_model, test, type = "raw") #Predict
confusionMatrix(y_hat_glm, test$seguridad) #Show the results

#KNN
knn_model <- train(seguridad~., data=train, method = "knn",
                   tuneGrid = data.frame(k = seq(5, 20, 2)),#test differents k
                   trControl = trainControl(method = "cv", 
                                            number=5, 
                                            p=0.9)) #5-folds crossvalidation
knn_model$bestTune #Best K
y_hat_knn <- predict(knn_model, test, type = "raw") #Predict
confusionMatrix(y_hat_knn, test$seguridad) #show results

#CLASIFICATION TREE
set.seed(1992)
rpart_model <- train(seguridad ~., data = train, method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.005)),
                     trControl = trainControl(method = "cv", 
                                              number=5, 
                                              p=0.9)) #Choose crossvalidation with
#5 k-folds)
rpart_model$bestTune #Best complexity parameter
y_hat_rpart <- predict(rpart_model, test)#Predict
confusionMatrix(y_hat_rpart, test$seguridad)#Show results
#Plotting the tree
plot(rpart_model$finalModel, margin=0.1)
text(rpart_model$finalModel, cex = 0.75)

#Ramdom Forest
set.seed(1992)
library(randomForest)
rf_fit_mod <- train(seguridad~., data= train, method= "rf",
                    trControl= trainControl(method="cv", 
                                            number=5), #CV with 5 folds
                    importance = T )
y_hat_rf <- predict(rf_fit_mod, test, type = "raw") #Predict
confusionMatrix(y_hat_rf, test$seguridad) #Show results
varImp(rf_fit_mod) #varaible importance
varImp(rf_fit_mod, scale = FALSE)

#Support Vector Machine (radial)
svm_fit_mod <- train(seguridad ~., data = train, method = "svmRadial", trControl = trainControl(method="cv", 
                                                                                                number=5),
                     preProcess = c("center","scale"), tuneLength = 10)
# Print the best tuning parameter sigma and C that maximizes model accuracy
svm_fit_mod$bestTune
y_hat_svm <- predict(svm_fit_mod, test, type = "raw") #Predict
confusionMatrix(y_hat_rf, test$seguridad)

#RESULTS
#To save as objects the confusion matrix
con_glm <- confusionMatrix(y_hat_glm, test$seguridad)
con_knn <- confusionMatrix(y_hat_knn, test$seguridad)
con_rpart <- confusionMatrix(y_hat_rpart, test$seguridad)
con_rf <- confusionMatrix(y_hat_rf, test$seguridad)
con_svm <- confusionMatrix(y_hat_svm, test$seguridad)
#To build the table with the resoults:
models <- c("glm", "knn", "rpart", "rf", "svm")
#Accuracies
accuracies <- c(con_glm$overall["Accuracy"], con_knn$overall["Accuracy"], 
                con_rpart$overall["Accuracy"], con_rf$overall["Accuracy"],
                con_svm$overall["Accuracy"])
resoults <- data.frame(models, accuracies)
resoults
#Acuracies, precision, recall and balanced accuracy.
resoults <- resoults %>% 
  mutate(sensitivity = c(con_glm$byClass["Sensitivity"], con_knn$byClass["Sensitivity"], 
                         con_rpart$byClass["Sensitivity"], con_rf$byClass["Sensitivity"],
                         con_svm$byClass["Sensitivity"]),
         specificity =  c(con_glm$byClass["Specificity"], con_knn$byClass["Specificity"], 
                          con_rpart$byClass["Specificity"], con_rf$byClass["Specificity"],
                          con_svm$byClass["Specificity"]),
         balanced_accuracy = c(con_glm$byClass["Balanced Accuracy"], con_knn$byClass["Balanced Accuracy"], 
                               con_rpart$byClass["Balanced Accuracy"], con_rf$byClass["Balanced Accuracy"],
                               con_svm$byClass["Balanced Accuracy"]))
resoults

                      #MODELS: 2
datp_2 <- datchile %>% select(starts_with("P5"), P2_1_1, VP_DC, rph_edad, rph_sexo) %>%
  na.omit() # to select variables
datp2_2 <- datp_2 %>% filter_all(all_vars( .<85)) #clean answers
datp3_2 <- datp2_2 %>% mutate(seguridad = case_when(
  .$P2_1_1 %in% c("1", "2")~"inseguro",
  .$P2_1_1 %in% c("3", "4") ~"seguro"), 
  sexo = as_factor(rph_sexo)) #transform some varaibles

datp4_2 <- datp3_2%>% mutate(seguridad = as_factor(seguridad),
                             victim = as_factor(VP_DC)) %>% select(-"P2_1_1", -rph_sexo, -VP_DC) #drop transformed varaibles
datp4_2  %>% 
  summarise("seguro" = mean(seguridad== "seguro"), 
            "inseguro" = mean(seguridad == "inseguro"),
            "total" = n())
# TO CREATE THE PARTITION BETWEEN TEST AND VALIDATION SETS
set.seed(1992)
test_index2 <- createDataPartition(y = datp4_2$seguridad, times = 1, p = 0.2, list = FALSE)
train2 <- datp4_2[-test_index2,]
test2 <- datp4_2[test_index2,]

#GLM MODEL
set.seed(1992)
glm_model2 <- train(seguridad~., data=train2, method= "glm",
                    trControl = trainControl(method = "cv", 
                                             number=5, 
                                             p=0.9)) #Choose crossvalidation with
#5 k-folds
summary(glm_model2)
y_hat_glm2 <- predict(glm_model2, test2, type = "raw") #Predict
confusionMatrix(y_hat_glm2, test2$seguridad) #Show the results

#KNN
knn_model2 <- train(seguridad~., data=train2, method = "knn",
                    tuneGrid = data.frame(k = seq(2, 100, 2)),#test differents k
                    trControl = trainControl(method = "cv", 
                                             number=5, 
                                             p=0.9)) #5-folds crossvalidation
knn_model2$bestTune #Best K
y_hat_knn2 <- predict(knn_model2, test2, type = "raw") #Predict
confusionMatrix(y_hat_knn2, test2$seguridad) #show results

#CLASIFICATION TREE
set.seed(1992)
rpart_model2 <- train(seguridad ~., data = train2, method = "rpart",
                      tuneGrid = data.frame(cp = seq(0, 1, 0.005)),
                      trControl = trainControl(method = "cv", 
                                               number=5, 
                                               p=0.9)) #Choose crossvalidation with
#5 k-folds)
rpart_model2$bestTune #Best complexity parameter
y_hat_rpart2 <- predict(rpart_model2, test2)#Predict
confusionMatrix(y_hat_rpart2, test2$seguridad)#Show results
#Plotting the tree
plot(rpart_model2$finalModel, margin=0.1)
text(rpart_model2$finalModel, cex = 0.75)


set.seed(1992)
library(randomForest)
rf_fit_mod2 <- train(seguridad~., data= train2, method= "rf",
                     trControl= trainControl(method="cv", 
                                             number=5), #CV with 5 folds
                     importance = T )
rf_fit_mod2$bestTune
rf_fit_mod2$finalModel
y_hat_rf2 <- predict(rf_fit_mod2, test2, type = "raw") #Predict
confusionMatrix(y_hat_rf2, test2$seguridad) #Show results
varImp(rf_fit_mod2) #varaible importance
varImp(rf_fit_mod2, scale = FALSE)

#SVM
svm_fit_mod2 <- train(seguridad ~., data = train2, method = "svmRadial", trControl = trainControl(method="cv", 
                                                                                                  number=5),
                      preProcess = c("center","scale"), tuneLength = 10)
# Print the best tuning parameter sigma and C that maximizes model accuracy
svm_fit_mod2$bestTune
y_hat_svm2 <- predict(svm_fit_mod2, test2, type = "raw") #Predict
confusionMatrix(y_hat_svm2, test2$seguridad)

#RESULTS
#To save as objects the confusion matrix
con_glm2 <- confusionMatrix(y_hat_glm2, test2$seguridad)
con_knn2 <- confusionMatrix(y_hat_knn2, test2$seguridad)
con_rpart2 <- confusionMatrix(y_hat_rpart2, test2$seguridad)
con_rf2 <- confusionMatrix(y_hat_rf2, test2$seguridad)
con_svm2 <- confusionMatrix(y_hat_svm2, test2$seguridad)
#To build the table with the resoults:
models <- c("glm", "knn", "rpart", "rf", "svm")
#Accuracies
accuracies2 <- c(con_glm2$overall["Accuracy"], con_knn2$overall["Accuracy"], 
                 con_rpart2$overall["Accuracy"], con_rf2$overall["Accuracy"],
                 con_svm2$overall["Accuracy"])
resoults2 <- data.frame(models, accuracies2)
resoults2
#Acuracies, precision, recall and balanced accuracy.
resoults2 <- resoults2 %>% 
  mutate(sensitivity = c(con_glm2$byClass["Sensitivity"], con_knn2$byClass["Sensitivity"], 
                         con_rpart2$byClass["Sensitivity"], con_rf2$byClass["Sensitivity"],
                         con_svm2$byClass["Sensitivity"]),
         specificity =  c(con_glm2$byClass["Specificity"], con_knn2$byClass["Specificity"], 
                          con_rpart2$byClass["Specificity"], con_rf2$byClass["Specificity"],
                          con_svm2$byClass["Specificity"]),
         balanced_accuracy = c(con_glm2$byClass["Balanced Accuracy"], con_knn2$byClass["Balanced Accuracy"], 
                               con_rpart2$byClass["Balanced Accuracy"], con_rf2$byClass["Balanced Accuracy"],
                               con_svm2$byClass["Balanced Accuracy"]))
resoults2

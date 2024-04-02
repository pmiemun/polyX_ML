rm(list=ls())																							#Remove all variables in the current session
library(readr); library(caret); library("pROC"); source("http://peterhaschke.com/Code/multiplot.R")
addTaskCallback(function(...) {set.seed(500);TRUE})
########################################################################## PolyQ ##########################################################################
#Set
  ppi_Qall <- read.csv("./polyQ.csv");                                              #Load dataset
  ppi_Qall <- ppi_Qall[, -c(1:2,25)]                                                #Delete the AC and polyX positions; not informative
  ppi_Qall$ppi <- factor(ppi_Qall$ppi,levels = c("0", "1"),labels = c("no","yes"))  #Factor PPI variable
  indexQ <- createDataPartition(ppi_Qall$ppi, p = 0.7, list = FALSE)                #Index to obtain 70% of data randomly
  Q_train <- ppi_Qall[indexQ, ];  Q_test  <- ppi_Qall[-indexQ, ]                    #Splitting training & test dataset (70/30 split)
  prop.table(table(Q_train$ppi));	prop.table(table(Q_test$ppi))		                  #Check they have a similar distribution of the PPI variable
#ML - Training
  ctrlQ <- trainControl(method = "repeatedcv", sampling='down', number = 10, repeats = 3, savePredictions = TRUE, classProbs = TRUE, selectionFunction = "best", summaryFunction = twoClassSummary) 
  rf_ppi_Q <- train(ppi ~ ., data = Q_train, method = "rf",	metric = "ROC", trControl = ctrlQ);                rf_ppi_Q       #Random Forest
  logreg_ppi_Q <- train(ppi ~ ., data = Q_train, method = "LogitBoost",	metric = "ROC", trControl = ctrlQ);    logreg_ppi_Q   #Boosted logistic regression
  knn_ppi_Q <- train(ppi ~ ., data = Q_train, method = "knn",	metric = "ROC", trControl = ctrlQ);              knn_ppi_Q      #K-nearest neighbors
  svm_ppi_Q <- train(ppi ~ ., data = Q_train, method = "svmLinear",	metric = "ROC", trControl = ctrlQ);        svm_ppi_Q      #SVM with linear kernel
  nn_ppi_Q <- train(ppi ~ ., data = Q_train, method = "nnet",	metric = "ROC", trControl = ctrlQ);              nn_ppi_Q       #Neural networks
#ML - Test
  #RF
    pred_Q_rf = predict(rf_ppi_Q,newdata=Q_test[-2],type='raw')
    cm_rf_Q = as.data.frame(table(Q_test[,2], as.numeric(pred_Q_rf)))
    cm_rf_Q<-caret::confusionMatrix(pred_Q_rf,as.factor(Q_test$ppi),mode="everything")
    cm_rf_Q
  #Boosted logistic regression
    pred_Q_logreg = predict(logreg_ppi_Q,newdata=Q_test[-2],type='raw')
    cm_logreg_Q = as.data.frame(table(Q_test[,2], as.numeric(pred_Q_logreg)))
    cm_logreg_Q<-caret::confusionMatrix(pred_Q_logreg,as.factor(Q_test$ppi),mode="everything")
    cm_logreg_Q
  #K-nearest neighbors
    pred_Q_knn = predict(knn_ppi_Q,newdata=Q_test[-2],type='raw')
    cm_knn_Q = as.data.frame(table(Q_test[,2], as.numeric(pred_Q_knn)))
    cm_knn_Q<-caret::confusionMatrix(pred_Q_knn,as.factor(Q_test$ppi),mode="everything")
    cm_knn_Q
  #SVM with linear kernel
    pred_Q_svm = predict(svm_ppi_Q,newdata=Q_test[-2],type='raw')
    cm_svm_Q = as.data.frame(table(Q_test[,2], as.numeric(pred_Q_svm)))
    cm_svm_Q<-caret::confusionMatrix(pred_Q_svm,as.factor(Q_test$ppi),mode="everything")
    cm_svm_Q
  #Neural networks
    pred_Q_nn = predict(nn_ppi_Q,newdata=Q_test[-2],type='raw')
    cm_nn_Q = as.data.frame(table(Q_test[,2], as.numeric(pred_Q_nn)))
    cm_nn_Q<-caret::confusionMatrix(pred_Q_nn, as.factor(Q_test$ppi),mode="everything")
    cm_nn_Q
  #Plot
    pred_rfQ= predict(rf_ppi_Q,newdata=Q_test[-2],type='prob');         roc_rfQ = roc(Q_test[,2], as.numeric(pred_rfQ[[2]]));          auc_rfQ <- roc_rfQ$auc;         auc_rfQ
    pred_logregQ= predict(logreg_ppi_Q,newdata=Q_test[-2],type='prob'); roc_logregQ = roc(Q_test[,2], as.numeric(pred_logregQ[[2]]));  auc_logregQ <- roc_logregQ$auc; auc_logregQ
    pred_knnQ= predict(knn_ppi_Q,newdata=Q_test[-2],type='prob');       roc_knnQ = roc(Q_test[,2], as.numeric(pred_knnQ[[2]]));        auc_knnQ <- roc_knnQ$auc;       auc_knnQ
    pred_svmQ= predict(svm_ppi_Q,newdata=Q_test[-2],type='prob');       roc_svmQ = roc(Q_test[,2], as.numeric(pred_svmQ[[2]]));        auc_svmQ <- roc_svmQ$auc;       auc_svmQ
    pred_nnQ= predict(nn_ppi_Q,newdata=Q_test[-2],type='prob');         roc_nnQ = roc(Q_test[,2], as.numeric(pred_nnQ[[2]]));          auc_nnQ <- roc_nnQ$auc;         auc_nnQ
    q<- ggroc(list(RF = roc_rfQ, LogReg = roc_logregQ, knn = roc_knnQ, SVM = roc_svmQ, NN = roc_nnQ),legacy.axes = T) +  theme_bw() + geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),color="darkgrey") +  ggtitle(paste0('A'))  + theme(legend.position = c(0.8, 0.3),legend.title = element_blank())  + geom_text(x=0.15, y=0.95, label="polyQ\nInteractome3D",color="black")
########################################################################## PolyA ########################################################################## 
#Set
  ppi_Aall <- read.csv("./polyA.csv");                                              #Load dataset
  ppi_Aall <- ppi_Aall[, -c(1:2,25)]                                                #Delete the AC and polyX positions; not informative
  ppi_Aall$ppi <- factor(ppi_Aall$ppi,levels = c("0", "1"),labels = c("no","yes"))  #Factor PPI variable
  indexA <- createDataPartition(ppi_Aall$ppi, p = 0.7, list = FALSE)                #Index to obtain 70% of data randomly
  A_train <- ppi_Aall[indexA, ];  A_test  <- ppi_Aall[-indexA, ]                    #Splitting training & test dataset (70/30 split)
  prop.table(table(A_train$ppi));	prop.table(table(A_test$ppi))		                  #Check they have a similar distribution of the PPI variable
#ML - Training
  ctrlA <- trainControl(method = "repeatedcv", sampling='down', number = 10, repeats = 3, savePredictions = TRUE, classProbs = TRUE, selectionFunction = "best", summaryFunction = twoClassSummary) 
  rf_ppi_A <- train(ppi ~ ., data = A_train, method = "rf",	metric = "ROC", trControl = ctrlA);                rf_ppi_A       #Random Forest
  logreg_ppi_A <- train(ppi ~ ., data = A_train, method = "LogitBoost",	metric = "ROC", trControl = ctrlA);    logreg_ppi_A   #Boosted logistic regression
  knn_ppi_A <- train(ppi ~ ., data = A_train, method = "knn",	metric = "ROC", trControl = ctrlA);              knn_ppi_A      #K-nearest neighbors
  svm_ppi_A <- train(ppi ~ ., data = A_train, method = "svmLinear",	metric = "ROC", trControl = ctrlA);        svm_ppi_A      #SVM with linear kernel
  nn_ppi_A <- train(ppi ~ ., data = A_train, method = "nnet",	metric = "ROC", trControl = ctrlA);              nn_ppi_A       #Neural networks
#ML - Test
  #RF
  pred_A_rf = predict(rf_ppi_A,newdata=A_test[-2],type='raw')
  cm_rf_A = as.data.frame(table(A_test[,2], as.numeric(pred_A_rf)))
  cm_rf_A<-caret::confusionMatrix(pred_A_rf,as.factor(A_test$ppi),mode="everything")
  cm_rf_A
  #Boosted logistic regression
  pred_A_logreg = predict(logreg_ppi_A,newdata=A_test[-2],type='raw')
  cm_logreg_A = as.data.frame(table(A_test[,2], as.numeric(pred_A_logreg)))
  cm_logreg_A<-caret::confusionMatrix(pred_A_logreg,as.factor(A_test$ppi),mode="everything")
  cm_logreg_A
  #K-nearest neighbors
  pred_A_knn = predict(knn_ppi_A,newdata=A_test[-2],type='raw')
  cm_knn_A = as.data.frame(table(A_test[,2], as.numeric(pred_A_knn)))
  cm_knn_A<-caret::confusionMatrix(pred_A_knn,as.factor(A_test$ppi),mode="everything")
  cm_knn_A
  #SVM with linear kernel
  pred_A_svm = predict(svm_ppi_A,newdata=A_test[-2],type='raw')
  cm_svm_A = as.data.frame(table(A_test[,2], as.numeric(pred_A_svm)))
  cm_svm_A<-caret::confusionMatrix(pred_A_svm,as.factor(A_test$ppi),mode="everything")
  cm_svm_A
  #Neural networks
  pred_A_nn = predict(nn_ppi_A,newdata=A_test[-2],type='raw')
  cm_nn_A = as.data.frame(table(A_test[,2], as.numeric(pred_A_nn)))
  cm_nn_A<-caret::confusionMatrix(pred_A_nn, as.factor(A_test$ppi),mode="everything")
  cm_nn_A
#Plot
  pred_rfA= predict(rf_ppi_A,newdata=A_test[-2],type='prob');         roc_rfA = roc(A_test[,2], as.numeric(pred_rfA[[2]]));          auc_rfA <- roc_rfA$auc;         auc_rfA
  pred_logregA= predict(logreg_ppi_A,newdata=A_test[-2],type='prob'); roc_logregA = roc(A_test[,2], as.numeric(pred_logregA[[2]]));  auc_logregA <- roc_logregA$auc; auc_logregA
  pred_knnA= predict(knn_ppi_A,newdata=A_test[-2],type='prob');       roc_knnA = roc(A_test[,2], as.numeric(pred_knnA[[2]]));        auc_knnA <- roc_knnA$auc;       auc_knnA
  pred_svmA= predict(svm_ppi_A,newdata=A_test[-2],type='prob');       roc_svmA = roc(A_test[,2], as.numeric(pred_svmA[[2]]));        auc_svmA <- roc_svmA$auc;       auc_svmA
  pred_nnA= predict(nn_ppi_A,newdata=A_test[-2],type='prob');         roc_nnA = roc(A_test[,2], as.numeric(pred_nnA[[2]]));          auc_nnA <- roc_nnA$auc;         auc_nnA
  a<- ggroc(list(RF = roc_rfA, LogReg = roc_logregA, knn = roc_knnA, SVM = roc_svmA, NN = roc_nnA),legacy.axes = T) +  theme_bw() + geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),color="darkgrey") + ggtitle(paste0('B'))  + theme(legend.position = c(0.8, 0.3), legend.title = element_blank())  + geom_text(x=0.15, y=0.95, label="polyA\nInteractome3D",color="black")
########################################################################## Plot Both ###################################################################
multiplot(q,a, cols=2)
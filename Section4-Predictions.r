rm(list=ls())																							#Remove all variables in the current session
library(readr); library(caret); library("pROC"); library(dplyr); library(ggplot2); source("http://peterhaschke.com/Code/multiplot.R"); library(ggpubr)
addTaskCallback(function(...) {set.seed(500);TRUE})
ctrl <- trainControl(method = "repeatedcv", sampling='down', number = 10, repeats = 3, savePredictions = TRUE, classProbs = TRUE, selectionFunction = "best", summaryFunction = twoClassSummary) 
########################################################################## PolyQ ##########################################################################
#Just the context
  #Prepare dataset
  ppi_Qall <- read.csv("./polyQ.csv"); ppi_RAWQ <- ppi_Qall                           #Load dataset
  ppi_Qall$ppi <- factor(ppi_Qall$ppi,levels = c("0", "1"),labels = c("no","yes"))    #Factor PPI variable
  #Prepare train and test 
  indexQ <- createDataPartition(ppi_Qall$ppi, p = 0.7, list = FALSE);   Q_train <- ppi_Qall[indexQ, ];  Q_test  <- ppi_Qall[-indexQ, ] #Index to obtain 70% of data randomly & split 70/30
  Q_train <- Q_train[, -c(1:2,5:8,21:44)]                                             #Delete the AC and polyX positions
  Q_test <- Q_test[, -c(1:2,5:8,21:44)]                                               #Copy the test dataset & #Delete the AC and polyX positions
  #Train and predict on the sets
  rf_ppi_Q <- train(ppi ~ ., data = Q_train, method = "rf",	metric = "ROC", trControl = ctrl)                                                               #Training
  pred_Q_rf = predict(rf_ppi_Q,newdata=Q_test[-2],type='raw');                          cm_rf_Q = as.data.frame(table(Q_test[,2], as.numeric(pred_Q_rf)))   #Test
  cm_rf_Q<-caret::confusionMatrix(pred_Q_rf,as.factor(Q_test$ppi),mode="everything");   cm_rf_Q
  pred_rfQ= predict(rf_ppi_Q,newdata=Q_test[-2],type='prob');         roc_rfQ = roc(Q_test[,2], as.numeric(pred_rfQ[[2]]));       auc_rfQ <- roc_rfQ$auc;         auc_rfQ
  #Predictions on the complete dataset
  ppi_Qall <- ppi_Qall[, -c(1:2,5:8,21:44)]
  pred_completeQ= predict(rf_ppi_Q,newdata=ppi_Qall[-2],type='prob')
  pred_completeQ$ac <- ppi_RAWQ$ac; pred_completeQ$polyQ <- ppi_RAWQ$polyQ; pred_completeQ$ppi <- ppi_RAWQ$ppi
  predictionsQ <- pred_completeQ[order(pred_completeQ$no),];  write.csv(predictionsQ, "predictionsQ.csv")     #Save the predictions
########################################################################## PolyA ########################################################################## 
#Context & CC  
  #Prepare dataset
  ppi_A10ccall <- read.csv("./polyA_cc.csv"); ppi_RAWA <- ppi_A10ccall;                     #Load dataset     
  ppi_A10ccall$ppi <- factor(ppi_A10ccall$ppi,levels = c("0", "1"),labels = c("no","yes"))  #Factor PPI variable
  #Prepare train and test 
  indexA10cc <- createDataPartition(ppi_A10ccall$ppi, p = 0.7, list = FALSE);   A10cc_train <- ppi_A10ccall[indexA10cc, ];  A10cc_test  <- ppi_A10ccall[-indexA10cc, ] #Index to obtain 70% of data randomly & split 70/30           
  A10cc_train <- A10cc_train[, -c(1:2)]                                                     #Delete the AC and polyX positions
  A10cc_test <- A10cc_test[, -c(1:2)]                                                       #Copy the test dataset & #Delete the AC and polyX positions
  #Train and predict on the sets
  rf_ppi_A10cc <- train(ppi ~ ., data = A10cc_train, method = "rf",	metric = "ROC", trControl = ctrl)                                                                             #Training
  pred_A10cc_rf = predict(rf_ppi_A10cc,newdata=A10cc_test[-2],type='raw');                          cm_rf_A10cc = as.data.frame(table(A10cc_test[,2], as.numeric(pred_A10cc_rf))) #Test
  cm_rf_A10cc<-caret::confusionMatrix(pred_A10cc_rf,as.factor(A10cc_test$ppi),mode="everything");   cm_rf_A10cc
  pred_rfA10cc= predict(rf_ppi_A10cc,newdata=A10cc_test[-2],type='prob');         roc_rfA10cc = roc(A10cc_test[,2], as.numeric(pred_rfA10cc[[2]]));          auc_rfA10cc <- roc_rfA10cc$auc;         auc_rfA10cc
  #Predictions on the complete dataset
  ppi_A10ccall <- ppi_A10ccall[, -c(1:2)]
  pred_completeA= predict(rf_ppi_A10cc,newdata=ppi_A10ccall[-2],type='prob')
  pred_completeA$ac <- ppi_RAWA$ac; pred_completeA$polyA <- ppi_RAWA$polyA; pred_completeA$ppi <- ppi_RAWA$ppi
  predictionsA <- pred_completeA[order(pred_completeA$no),];  write.csv(predictionsA, "predictionsA.csv")     #Save the predictions
########################################################################## Plot ########################################################################## 
  #ROC on test sets
    ggroc(list(polyQ = roc_knnQ, polyA = roc_rfA10cc), legacy.axes = T) +  theme_bw() + geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),color="darkgrey") + ggtitle(paste0('Best predictions on test set'))
  #Score distribution on complete datasets
    q <- ggplot(pred_completeQ, aes(x=yes)) + geom_histogram(binwidth=0.05) + theme_classic() + xlim(0,1) +  labs(title="polyQ",x="Score", y = "Count")
    a <- ggplot(pred_completeA, aes(x=yes)) + geom_histogram(binwidth=0.05) + theme_classic() + xlim(0,1) +  labs(title="polyA",x="Score", y = "Count")
    multiplot(q,a, cols=1)
  #Feature importance
    i_scores_caretQ <- varImp(rf_ppi_Q,scale = TRUE);       feat_impQ<-plot(i_scores_caretQ, top = 20)
    i_scores_caretA <- varImp(rf_ppi_A10cc,scale = TRUE);   feat_impA<-plot(i_scores_caretA, top = 20)
    ggarrange(feat_impQ, q, feat_impA, a + rremove("x.text"),labels = c("A", "B", "C","D"),ncol = 2, nrow = 2)
  #Score vs length
    pred_completeQ2 <- pred_completeQ;  pred_completeQ2$length <- ppi_RAWQ$length
    pred_completeA2 <- pred_completeA;  pred_completeA2$length <- ppi_RAWA$length
    qs <- ggplot(pred_completeQ2, aes(x=length, y=yes, fill=ppi)) + geom_point(aes(colour = ppi)) + theme_classic()  +  labs(x="PolyQ length", y = "Score") + theme(legend.position = "none")+ xlim(0,50)
    as <- ggplot(pred_completeA2, aes(x=length, y=yes, fill=ppi)) + geom_point(aes(colour = ppi)) + theme_classic()  +  labs(x="PolyA length", y = "Score") + theme(legend.position = "none")+ xlim(0,50) 
    
    ggarrange(q, feat_impQ, qs, a, feat_impA, as + rremove("x.text"),labels = c("A", "B", "C","D", "E","F"),ncol = 3, nrow = 2)
    
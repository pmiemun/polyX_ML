rm(list=ls())																							#Remove all variables in the current session
library(readr); library(caret); library("pROC"); library(dplyr); source("http://peterhaschke.com/Code/multiplot.R")
addTaskCallback(function(...) {set.seed(500);TRUE})
ctrl <- trainControl(method = "repeatedcv", sampling='down', number = 10, repeats = 3, savePredictions = TRUE, classProbs = TRUE, selectionFunction = "best", summaryFunction = twoClassSummary) 
########################################################################## PolyQ ##########################################################################
#Just the context
  ppi_Qall <- read.csv("./polyQ_cc.csv");      ppi_Qall <- ppi_Qall[, -c(1:2,5:8,21:44)]                           #Load dataset    &   #Delete the AC and polyX positions
  ppi_Qall$ppi <- factor(ppi_Qall$ppi,levels = c("0", "1"),labels = c("no","yes"))                                          #Factor PPI variable
  indexQ <- createDataPartition(ppi_Qall$ppi, p = 0.7, list = FALSE);   Q_train <- ppi_Qall[indexQ, ];  Q_test  <- ppi_Qall[-indexQ, ] #Index to obtain 70% of data randomly & split 70/30           
  rf_ppi_Q <- train(ppi ~ ., data = Q_train, method = "rf",	metric = "ROC", trControl = ctrl)                             #Training
  pred_Q_rf = predict(rf_ppi_Q,newdata=Q_test[-2],type='raw');                          cm_rf_Q = as.data.frame(table(Q_test[,2], as.numeric(pred_Q_rf))) #Test
  cm_rf_Q<-caret::confusionMatrix(pred_Q_rf,as.factor(Q_test$ppi),mode="everything");   cm_rf_Q
  pred_rfQ= predict(rf_ppi_Q,newdata=Q_test[-2],type='prob');         roc_rfQ = roc(Q_test[,2], as.numeric(pred_rfQ[[2]]));          auc_rfQ <- roc_rfQ$auc;         auc_rfQ
#Context + CC  
  ppi_Qccall <- read.csv("./polyQ_cc.csv");      ppi_Qccall <- ppi_Qccall[, -c(1:2,5:8,21:28,41:44)]                           #Load dataset    &   #Delete the AC and polyX positions
  ppi_Qccall$ppi <- factor(ppi_Qccall$ppi,levels = c("0", "1"),labels = c("no","yes"))                                          #Factor PPI variable
  indexQcc <- createDataPartition(ppi_Qccall$ppi, p = 0.7, list = FALSE);   Qcc_train <- ppi_Qccall[indexQcc, ];  Qcc_test  <- ppi_Qccall[-indexQcc, ] #Index to obtain 70% of data randomly & split 70/30           
  rf_ppi_Qcc <- train(ppi ~ ., data = Qcc_train, method = "rf",	metric = "ROC", trControl = ctrl)                             #Training
  pred_Qcc_rf = predict(rf_ppi_Qcc,newdata=Qcc_test[-2],type='raw');                          cm_rf_Qcc = as.data.frame(table(Qcc_test[,2], as.numeric(pred_Qcc_rf))) #Test
   cm_rf_Qcc<-caret::confusionMatrix(pred_Qcc_rf,as.factor(Qcc_test$ppi),mode="everything");   cm_rf_Qcc
  pred_rfQcc= predict(rf_ppi_Qcc,newdata=Qcc_test[-2],type='prob');         roc_rfQcc = roc(Qcc_test[,2], as.numeric(pred_rfQcc[[2]]));          auc_rfQcc <- roc_rfQcc$auc;         auc_rfQcc
#Plot
 q <- ggroc(list(M6P6 = roc_rfQ, M6P6_CC = roc_rfQcc), legacy.axes = T) +  theme_bw() + geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),color="darkgrey") + ggtitle(paste0('A')) + theme(legend.position = c(0.8, 0.3),legend.title = element_blank())  + geom_text(x=0.1, y=0.95, label="polyQ",color="black")
########################################################################## PolyA ########################################################################## 
#Just the context
  ppi_A10all <- read.csv("./polyA_cc.csv");      ppi_A10all <- ppi_A10all[, -c(1:2,25:44)]                                            #Load dataset    &   #Delete the AC and polyX positions
  ppi_A10all$ppi <- factor(ppi_A10all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                          #Factor PPI variable
  indexA10 <- createDataPartition(ppi_A10all$ppi, p = 0.7, list = FALSE);   A10_train <- ppi_A10all[indexA10, ];  A10_test  <- ppi_A10all[-indexA10, ] #Index to obtain 70% of data randomly & split 70/30           
  rf_ppi_A10 <- train(ppi ~ ., data = A10_train, method = "rf",	metric = "ROC", trControl = ctrl)                             #Training
  pred_A10_rf = predict(rf_ppi_A10,newdata=A10_test[-2],type='raw');                          cm_rf_A10 = as.data.frame(table(A10_test[,2], as.numeric(pred_A10_rf))) #Test
  cm_rf_A10<-caret::confusionMatrix(pred_A10_rf,as.factor(A10_test$ppi),mode="everything");   cm_rf_A10
  pred_rfA10= predict(rf_ppi_A10,newdata=A10_test[-2],type='prob');         roc_rfA10 = roc(A10_test[,2], as.numeric(pred_rfA10[[2]]));          auc_rfA10 <- roc_rfA10$auc;         auc_rfA10
#Context + CC  
  ppi_A10ccall <- read.csv("./polyA_cc.csv");      ppi_A10ccall <- ppi_A10ccall[, -c(1:2)]                                            #Load dataset    &   #Delete the AC and polyX positions
  ppi_A10ccall$ppi <- factor(ppi_A10ccall$ppi,levels = c("0", "1"),labels = c("no","yes"))                                          #Factor PPI variable
  indexA10cc <- createDataPartition(ppi_A10ccall$ppi, p = 0.7, list = FALSE);   A10cc_train <- ppi_A10ccall[indexA10cc, ];  A10cc_test  <- ppi_A10ccall[-indexA10cc, ] #Index to obtain 70% of data randomly & split 70/30           
  rf_ppi_A10cc <- train(ppi ~ ., data = A10cc_train, method = "rf",	metric = "ROC", trControl = ctrl)                             #Training
  pred_A10cc_rf = predict(rf_ppi_A10cc,newdata=A10cc_test[-2],type='raw');                          cm_rf_A10cc = as.data.frame(table(A10cc_test[,2], as.numeric(pred_A10cc_rf))) #Test
  cm_rf_A10cc<-caret::confusionMatrix(pred_A10cc_rf,as.factor(A10cc_test$ppi),mode="everything");   cm_rf_A10cc
  pred_rfA10cc= predict(rf_ppi_A10cc,newdata=A10cc_test[-2],type='prob');         roc_rfA10cc = roc(A10cc_test[,2], as.numeric(pred_rfA10cc[[2]]));          auc_rfA10cc <- roc_rfA10cc$auc;         auc_rfA10cc
#Plot
  a <- ggroc(list(M10P10 = roc_rfA10, M10P10_CC = roc_rfA10cc), legacy.axes = T) +  theme_bw() + geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),color="darkgrey") + ggtitle(paste0('B')) + theme(legend.position = c(0.8, 0.3),legend.title = element_blank())  + geom_text(x=0.1, y=0.95, label="polyA",color="black")
########################################################################## Plot both ########################################################################## 
  multiplot(q,a, cols=2)
rm(list=ls())																							#Remove all variables in the current session
library(readr); library(caret); library("pROC"); source("http://peterhaschke.com/Code/multiplot.R")
addTaskCallback(function(...) {set.seed(500);TRUE})
ctrl <- trainControl(method = "repeatedcv", sampling='down', number = 10, repeats = 3, savePredictions = TRUE, classProbs = TRUE, selectionFunction = "best", summaryFunction = twoClassSummary) 
########################################################################## PolyQ ##########################################################################
#-10 -> +10
ppi_Q10all <- read.csv("./polyQ.csv");      ppi_Q10all <- ppi_Q10all[, -c(1:2)]                                            #Load dataset    &   #Delete the AC and polyX positions
ppi_Q10all$ppi <- factor(ppi_Q10all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                          #Factor PPI variable
indexQ10 <- createDataPartition(ppi_Q10all$ppi, p = 0.7, list = FALSE);   Q10_train <- ppi_Q10all[indexQ10, ];  Q10_test  <- ppi_Q10all[-indexQ10, ] #Index to obtain 70% of data randomly & split 70/30           
rf_ppi_Q10 <- train(ppi ~ ., data = Q10_train, method = "rf",	metric = "ROC", trControl = ctrl)                             #Training
pred_Q10_rf = predict(rf_ppi_Q10,newdata=Q10_test[-2],type='raw');                          cm_rf_Q10 = as.data.frame(table(Q10_test[,2], as.numeric(pred_Q10_rf))) #Test
cm_rf_Q10<-caret::confusionMatrix(pred_Q10_rf,as.factor(Q10_test$ppi),mode="everything");   cm_rf_Q10
pred_rfQ10= predict(rf_ppi_Q10,newdata=Q10_test[-2],type='prob');         roc_rfQ10 = roc(Q10_test[,2], as.numeric(pred_rfQ10[[2]]));          auc_rfQ10 <- roc_rfQ10$auc;         auc_rfQ10
#-9 -> +9
ppi_Q9all <- read.csv("./polyQ.csv");      ppi_Q9all <- ppi_Q9all[, -c(1:2,5,24)]                                               #Load dataset    &   #Delete the AC and polyX positions
ppi_Q9all$ppi <- factor(ppi_Q9all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
indexQ9 <- createDataPartition(ppi_Q9all$ppi, p = 0.7, list = FALSE);  Q9_train <- ppi_Q9all[indexQ9, ];  Q9_test  <- ppi_Q9all[-indexQ9, ]   #Index to obtain 70% of data randomly & split 70/30
rf_ppi_Q9 <- train(ppi ~ ., data = Q9_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
pred_Q9_rf = predict(rf_ppi_Q9,newdata=Q9_test[-2],type='raw');                          cm_rf_Q9 = as.data.frame(table(Q9_test[,2], as.numeric(pred_Q9_rf))) #Test
cm_rf_Q9<-caret::confusionMatrix(pred_Q9_rf,as.factor(Q9_test$ppi),mode="everything");   cm_rf_Q9
pred_rfQ9= predict(rf_ppi_Q9,newdata=Q9_test[-2],type='prob');         roc_rfQ9 = roc(Q9_test[,2], as.numeric(pred_rfQ9[[2]]));          auc_rfQ9 <- roc_rfQ9$auc;         auc_rfQ9
#-8 -> +8
ppi_Q8all <- read.csv("./polyQ.csv");      ppi_Q8all <- ppi_Q8all[, -c(1:2,5:6,23:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
ppi_Q8all$ppi <- factor(ppi_Q8all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
indexQ8 <- createDataPartition(ppi_Q8all$ppi, p = 0.7, list = FALSE);  Q8_train <- ppi_Q8all[indexQ8, ];  Q8_test  <- ppi_Q8all[-indexQ8, ]   #Index to obtain 70% of data randomly & split 70/30
rf_ppi_Q8 <- train(ppi ~ ., data = Q8_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
pred_Q8_rf = predict(rf_ppi_Q8,newdata=Q8_test[-2],type='raw');                          cm_rf_Q8 = as.data.frame(table(Q8_test[,2], as.numeric(pred_Q8_rf))) #Test
cm_rf_Q8<-caret::confusionMatrix(pred_Q8_rf,as.factor(Q8_test$ppi),mode="everything");   cm_rf_Q8
pred_rfQ8= predict(rf_ppi_Q8,newdata=Q8_test[-2],type='prob');         roc_rfQ8 = roc(Q8_test[,2], as.numeric(pred_rfQ8[[2]]));          auc_rfQ8 <- roc_rfQ8$auc;         auc_rfQ8
#-7 -> +7
ppi_Q7all <- read.csv("./polyQ.csv");      ppi_Q7all <- ppi_Q7all[, -c(1:2,5:7,22:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
ppi_Q7all$ppi <- factor(ppi_Q7all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
indexQ7 <- createDataPartition(ppi_Q7all$ppi, p = 0.7, list = FALSE);  Q7_train <- ppi_Q7all[indexQ7, ];  Q7_test  <- ppi_Q7all[-indexQ7, ]   #Index to obtain 70% of data randomly & split 70/30
rf_ppi_Q7 <- train(ppi ~ ., data = Q7_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
pred_Q7_rf = predict(rf_ppi_Q7,newdata=Q7_test[-2],type='raw');                          cm_rf_Q7 = as.data.frame(table(Q7_test[,2], as.numeric(pred_Q7_rf))) #Test
cm_rf_Q7<-caret::confusionMatrix(pred_Q7_rf,as.factor(Q7_test$ppi),mode="everything");   cm_rf_Q7
pred_rfQ7= predict(rf_ppi_Q7,newdata=Q7_test[-2],type='prob');         roc_rfQ7 = roc(Q7_test[,2], as.numeric(pred_rfQ7[[2]]));          auc_rfQ7 <- roc_rfQ7$auc;         auc_rfQ7
#-6 -> +6
ppi_Q6all <- read.csv("./polyQ.csv");      ppi_Q6all <- ppi_Q6all[, -c(1:2,5:8,21:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
ppi_Q6all$ppi <- factor(ppi_Q6all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
indexQ6 <- createDataPartition(ppi_Q6all$ppi, p = 0.7, list = FALSE);  Q6_train <- ppi_Q6all[indexQ6, ];  Q6_test  <- ppi_Q6all[-indexQ6, ]   #Index to obtain 70% of data randomly & split 70/30
rf_ppi_Q6 <- train(ppi ~ ., data = Q6_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
pred_Q6_rf = predict(rf_ppi_Q6,newdata=Q6_test[-2],type='raw');                          cm_rf_Q6 = as.data.frame(table(Q6_test[,2], as.numeric(pred_Q6_rf))) #Test
cm_rf_Q6<-caret::confusionMatrix(pred_Q6_rf,as.factor(Q6_test$ppi),mode="everything");   cm_rf_Q6
pred_rfQ6= predict(rf_ppi_Q6,newdata=Q6_test[-2],type='prob');         roc_rfQ6 = roc(Q6_test[,2], as.numeric(pred_rfQ6[[2]]));          auc_rfQ6 <- roc_rfQ6$auc;         auc_rfQ6
#-5 -> +5
ppi_Q5all <- read.csv("./polyQ.csv");      ppi_Q5all <- ppi_Q5all[, -c(1:2,5:9,20:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
ppi_Q5all$ppi <- factor(ppi_Q5all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
indexQ5 <- createDataPartition(ppi_Q5all$ppi, p = 0.7, list = FALSE);  Q5_train <- ppi_Q5all[indexQ5, ];  Q5_test  <- ppi_Q5all[-indexQ5, ]   #Index to obtain 70% of data randomly & split 70/30
rf_ppi_Q5 <- train(ppi ~ ., data = Q5_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
pred_Q5_rf = predict(rf_ppi_Q5,newdata=Q5_test[-2],type='raw');                          cm_rf_Q5 = as.data.frame(table(Q5_test[,2], as.numeric(pred_Q5_rf))) #Test
cm_rf_Q5<-caret::confusionMatrix(pred_Q5_rf,as.factor(Q5_test$ppi),mode="everything");   cm_rf_Q5
pred_rfQ5= predict(rf_ppi_Q5,newdata=Q5_test[-2],type='prob');         roc_rfQ5 = roc(Q5_test[,2], as.numeric(pred_rfQ5[[2]]));          auc_rfQ5 <- roc_rfQ5$auc;         auc_rfQ5
#-4 -> +4
ppi_Q4all <- read.csv("./polyQ.csv");      ppi_Q4all <- ppi_Q4all[, -c(1:2,5:10,19:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
ppi_Q4all$ppi <- factor(ppi_Q4all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
indexQ4 <- createDataPartition(ppi_Q4all$ppi, p = 0.7, list = FALSE);  Q4_train <- ppi_Q4all[indexQ4, ];  Q4_test  <- ppi_Q4all[-indexQ4, ]   #Index to obtain 70% of data randomly & split 70/30
rf_ppi_Q4 <- train(ppi ~ ., data = Q4_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
pred_Q4_rf = predict(rf_ppi_Q4,newdata=Q4_test[-2],type='raw');                          cm_rf_Q4 = as.data.frame(table(Q4_test[,2], as.numeric(pred_Q4_rf))) #Test
cm_rf_Q4<-caret::confusionMatrix(pred_Q4_rf,as.factor(Q4_test$ppi),mode="everything");   cm_rf_Q4
pred_rfQ4= predict(rf_ppi_Q4,newdata=Q4_test[-2],type='prob');         roc_rfQ4 = roc(Q4_test[,2], as.numeric(pred_rfQ4[[2]]));          auc_rfQ4 <- roc_rfQ4$auc;         auc_rfQ4
#-3 -> +3
ppi_Q3all <- read.csv("./polyQ.csv");      ppi_Q3all <- ppi_Q3all[, -c(1:2,5:11,18:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
ppi_Q3all$ppi <- factor(ppi_Q3all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
indexQ3 <- createDataPartition(ppi_Q3all$ppi, p = 0.7, list = FALSE);  Q3_train <- ppi_Q3all[indexQ3, ];  Q3_test  <- ppi_Q3all[-indexQ3, ]   #Index to obtain 70% of data randomly & split 70/30
rf_ppi_Q3 <- train(ppi ~ ., data = Q3_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
pred_Q3_rf = predict(rf_ppi_Q3,newdata=Q3_test[-2],type='raw');                          cm_rf_Q3 = as.data.frame(table(Q3_test[,2], as.numeric(pred_Q3_rf))) #Test
cm_rf_Q3<-caret::confusionMatrix(pred_Q3_rf,as.factor(Q3_test$ppi),mode="everything");   cm_rf_Q3
pred_rfQ3= predict(rf_ppi_Q3,newdata=Q3_test[-2],type='prob');         roc_rfQ3 = roc(Q3_test[,2], as.numeric(pred_rfQ3[[2]]));          auc_rfQ3 <- roc_rfQ3$auc;         auc_rfQ3
#-2 -> +2
ppi_Q2all <- read.csv("./polyQ.csv");      ppi_Q2all <- ppi_Q2all[, -c(1:2,5:12,17:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
ppi_Q2all$ppi <- factor(ppi_Q2all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
indexQ2 <- createDataPartition(ppi_Q2all$ppi, p = 0.7, list = FALSE);  Q2_train <- ppi_Q2all[indexQ2, ];  Q2_test  <- ppi_Q2all[-indexQ2, ]   #Index to obtain 70% of data randomly & split 70/30
rf_ppi_Q2 <- train(ppi ~ ., data = Q2_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
pred_Q2_rf = predict(rf_ppi_Q2,newdata=Q2_test[-2],type='raw');                          cm_rf_Q2 = as.data.frame(table(Q2_test[,2], as.numeric(pred_Q2_rf))) #Test
cm_rf_Q2<-caret::confusionMatrix(pred_Q2_rf,as.factor(Q2_test$ppi),mode="everything");   cm_rf_Q2
pred_rfQ2= predict(rf_ppi_Q2,newdata=Q2_test[-2],type='prob');         roc_rfQ2 = roc(Q2_test[,2], as.numeric(pred_rfQ2[[2]]));          auc_rfQ2 <- roc_rfQ2$auc;         auc_rfQ2
#-1 -> +1
ppi_Q1all <- read.csv("./polyQ.csv");      ppi_Q1all <- ppi_Q1all[, -c(1:2,5:13,16:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
ppi_Q1all$ppi <- factor(ppi_Q1all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
indexQ1 <- createDataPartition(ppi_Q1all$ppi, p = 0.7, list = FALSE);  Q1_train <- ppi_Q1all[indexQ1, ];  Q1_test  <- ppi_Q1all[-indexQ1, ]   #Index to obtain 70% of data randomly & split 70/30
rf_ppi_Q1 <- train(ppi ~ ., data = Q1_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
pred_Q1_rf = predict(rf_ppi_Q1,newdata=Q1_test[-2],type='raw');                          cm_rf_Q1 = as.data.frame(table(Q1_test[,2], as.numeric(pred_Q1_rf))) #Test
cm_rf_Q1<-caret::confusionMatrix(pred_Q1_rf,as.factor(Q1_test$ppi),mode="everything");   cm_rf_Q1
pred_rfQ1= predict(rf_ppi_Q1,newdata=Q1_test[-2],type='prob');         roc_rfQ1 = roc(Q1_test[,2], as.numeric(pred_rfQ1[[2]]));          auc_rfQ1 <- roc_rfQ1$auc;         auc_rfQ1

#-6 -> +7
ppi_Q67all <- read.csv("./polyQ.csv");      ppi_Q67all <- ppi_Q67all[, -c(1:2,5:8,22:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
ppi_Q67all$ppi <- factor(ppi_Q67all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
indexQ67 <- createDataPartition(ppi_Q67all$ppi, p = 0.7, list = FALSE);  Q67_train <- ppi_Q67all[indexQ67, ];  Q67_test  <- ppi_Q67all[-indexQ67, ]   #Index to obtain 70% of data randomly & split 70/30
rf_ppi_Q67 <- train(ppi ~ ., data = Q67_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
pred_Q67_rf = predict(rf_ppi_Q67,newdata=Q67_test[-2],type='raw');                          cm_rf_Q67 = as.data.frame(table(Q67_test[,2], as.numeric(pred_Q67_rf))) #Test
cm_rf_Q67<-caret::confusionMatrix(pred_Q67_rf,as.factor(Q67_test$ppi),mode="everything");   cm_rf_Q67
pred_rfQ67= predict(rf_ppi_Q67,newdata=Q67_test[-2],type='prob');         roc_rfQ67 = roc(Q67_test[,2], as.numeric(pred_rfQ67[[2]]));          auc_rfQ67 <- roc_rfQ67$auc;         auc_rfQ67
#-6 -> +5
ppi_Q65all <- read.csv("./polyQ.csv");      ppi_Q65all <- ppi_Q65all[, -c(1:2,5:8,20:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
ppi_Q65all$ppi <- factor(ppi_Q65all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
indexQ65 <- createDataPartition(ppi_Q65all$ppi, p = 0.7, list = FALSE);  Q65_train <- ppi_Q65all[indexQ65, ];  Q65_test  <- ppi_Q65all[-indexQ65, ]   #Index to obtain 70% of data randomly & split 70/30
rf_ppi_Q65 <- train(ppi ~ ., data = Q65_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
pred_Q65_rf = predict(rf_ppi_Q65,newdata=Q65_test[-2],type='raw');                          cm_rf_Q65 = as.data.frame(table(Q65_test[,2], as.numeric(pred_Q65_rf))) #Test
cm_rf_Q65<-caret::confusionMatrix(pred_Q65_rf,as.factor(Q65_test$ppi),mode="everything");   cm_rf_Q65
pred_rfQ65= predict(rf_ppi_Q65,newdata=Q65_test[-2],type='prob');         roc_rfQ65 = roc(Q65_test[,2], as.numeric(pred_rfQ65[[2]]));          auc_rfQ65 <- roc_rfQ65$auc;         auc_rfQ65

#Plot best symmetrical and best assymetrical per side
q <-ggroc(list(M10P10 = roc_rfQ10, M9P9 = roc_rfQ9, M8P8 = roc_rfQ8, M7P7 = roc_rfQ7, M6P6 = roc_rfQ6, M5P5 = roc_rfQ5, M4P4 = roc_rfQ4, M3P3 = roc_rfQ3, M2P2 = roc_rfQ2, M1P1 = roc_rfQ1), legacy.axes = T) +  theme_bw() + geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),color="darkgrey") + ggtitle(paste0('A'))  + theme(legend.position = c(0.87, 0.37),legend.title = element_blank()) + geom_text(x=0.10, y=0.95, label="polyQ\nContext",color="black")
########################################################################## PolyA ########################################################################## 
#-10 -> +10
  ppi_A10all <- read.csv("./polyA.csv");      ppi_A10all <- ppi_A10all[, -c(1:2)]                                            #Load dataset    &   #Delete the AC and polyX positions
  ppi_A10all$ppi <- factor(ppi_A10all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                          #Factor PPI variable
  indexA10 <- createDataPartition(ppi_A10all$ppi, p = 0.7, list = FALSE);   A10_train <- ppi_A10all[indexA10, ];  A10_test  <- ppi_A10all[-indexA10, ] #Index to obtain 70% of data randomly & split 70/30           
  rf_ppi_A10 <- train(ppi ~ ., data = A10_train, method = "rf",	metric = "ROC", trControl = ctrl)                             #Training
  pred_A10_rf = predict(rf_ppi_A10,newdata=A10_test[-2],type='raw');                          cm_rf_A10 = as.data.frame(table(A10_test[,2], as.numeric(pred_A10_rf))) #Test
  cm_rf_A10<-caret::confusionMatrix(pred_A10_rf,as.factor(A10_test$ppi),mode="everything");   cm_rf_A10
  pred_rfA10= predict(rf_ppi_A10,newdata=A10_test[-2],type='prob');         roc_rfA10 = roc(A10_test[,2], as.numeric(pred_rfA10[[2]]));          auc_rfA10 <- roc_rfA10$auc;         auc_rfA10
#-9 -> +9
  ppi_A9all <- read.csv("./polyA.csv");      ppi_A9all <- ppi_A9all[, -c(1:2,5,24)]                                               #Load dataset    &   #Delete the AC and polyX positions
  ppi_A9all$ppi <- factor(ppi_A9all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
  indexA9 <- createDataPartition(ppi_A9all$ppi, p = 0.7, list = FALSE);  A9_train <- ppi_A9all[indexA9, ];  A9_test  <- ppi_A9all[-indexA9, ]   #Index to obtain 70% of data randomly & split 70/30
  rf_ppi_A9 <- train(ppi ~ ., data = A9_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
  pred_A9_rf = predict(rf_ppi_A9,newdata=A9_test[-2],type='raw');                          cm_rf_A9 = as.data.frame(table(A9_test[,2], as.numeric(pred_A9_rf))) #Test
  cm_rf_A9<-caret::confusionMatrix(pred_A9_rf,as.factor(A9_test$ppi),mode="everything");   cm_rf_A9
  pred_rfA9= predict(rf_ppi_A9,newdata=A9_test[-2],type='prob');         roc_rfA9 = roc(A9_test[,2], as.numeric(pred_rfA9[[2]]));          auc_rfA9 <- roc_rfA9$auc;         auc_rfA9
#-8 -> +8
  ppi_A8all <- read.csv("./polyA.csv");      ppi_A8all <- ppi_A8all[, -c(1:2,5:6,23:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
  ppi_A8all$ppi <- factor(ppi_A8all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
  indexA8 <- createDataPartition(ppi_A8all$ppi, p = 0.7, list = FALSE);  A8_train <- ppi_A8all[indexA8, ];  A8_test  <- ppi_A8all[-indexA8, ]   #Index to obtain 70% of data randomly & split 70/30
  rf_ppi_A8 <- train(ppi ~ ., data = A8_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
  pred_A8_rf = predict(rf_ppi_A8,newdata=A8_test[-2],type='raw');                          cm_rf_A8 = as.data.frame(table(A8_test[,2], as.numeric(pred_A8_rf))) #Test
  cm_rf_A8<-caret::confusionMatrix(pred_A8_rf,as.factor(A8_test$ppi),mode="everything");   cm_rf_A8
  pred_rfA8= predict(rf_ppi_A8,newdata=A8_test[-2],type='prob');         roc_rfA8 = roc(A8_test[,2], as.numeric(pred_rfA8[[2]]));          auc_rfA8 <- roc_rfA8$auc;         auc_rfA8
#-7 -> +7
  ppi_A7all <- read.csv("./polyA.csv");      ppi_A7all <- ppi_A7all[, -c(1:2,5:7,22:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
  ppi_A7all$ppi <- factor(ppi_A7all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
  indexA7 <- createDataPartition(ppi_A7all$ppi, p = 0.7, list = FALSE);  A7_train <- ppi_A7all[indexA7, ];  A7_test  <- ppi_A7all[-indexA7, ]   #Index to obtain 70% of data randomly & split 70/30
  rf_ppi_A7 <- train(ppi ~ ., data = A7_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
  pred_A7_rf = predict(rf_ppi_A7,newdata=A7_test[-2],type='raw');                          cm_rf_A7 = as.data.frame(table(A7_test[,2], as.numeric(pred_A7_rf))) #Test
  cm_rf_A7<-caret::confusionMatrix(pred_A7_rf,as.factor(A7_test$ppi),mode="everything");   cm_rf_A7
  pred_rfA7= predict(rf_ppi_A7,newdata=A7_test[-2],type='prob');         roc_rfA7 = roc(A7_test[,2], as.numeric(pred_rfA7[[2]]));          auc_rfA7 <- roc_rfA7$auc;         auc_rfA7
#-6 -> +6
  ppi_A6all <- read.csv("./polyA.csv");      ppi_A6all <- ppi_A6all[, -c(1:2,5:8,21:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
  ppi_A6all$ppi <- factor(ppi_A6all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
  indexA6 <- createDataPartition(ppi_A6all$ppi, p = 0.7, list = FALSE);  A6_train <- ppi_A6all[indexA6, ];  A6_test  <- ppi_A6all[-indexA6, ]   #Index to obtain 70% of data randomly & split 70/30
  rf_ppi_A6 <- train(ppi ~ ., data = A6_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
  pred_A6_rf = predict(rf_ppi_A6,newdata=A6_test[-2],type='raw');                          cm_rf_A6 = as.data.frame(table(A6_test[,2], as.numeric(pred_A6_rf))) #Test
  cm_rf_A6<-caret::confusionMatrix(pred_A6_rf,as.factor(A6_test$ppi),mode="everything");   cm_rf_A6
  pred_rfA6= predict(rf_ppi_A6,newdata=A6_test[-2],type='prob');         roc_rfA6 = roc(A6_test[,2], as.numeric(pred_rfA6[[2]]));          auc_rfA6 <- roc_rfA6$auc;         auc_rfA6
#-5 -> +5
  ppi_A5all <- read.csv("./polyA.csv");      ppi_A5all <- ppi_A5all[, -c(1:2,5:9,20:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
  ppi_A5all$ppi <- factor(ppi_A5all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
  indexA5 <- createDataPartition(ppi_A5all$ppi, p = 0.7, list = FALSE);  A5_train <- ppi_A5all[indexA5, ];  A5_test  <- ppi_A5all[-indexA5, ]   #Index to obtain 70% of data randomly & split 70/30
  rf_ppi_A5 <- train(ppi ~ ., data = A5_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
  pred_A5_rf = predict(rf_ppi_A5,newdata=A5_test[-2],type='raw');                          cm_rf_A5 = as.data.frame(table(A5_test[,2], as.numeric(pred_A5_rf))) #Test
  cm_rf_A5<-caret::confusionMatrix(pred_A5_rf,as.factor(A5_test$ppi),mode="everything");   cm_rf_A5
  pred_rfA5= predict(rf_ppi_A5,newdata=A5_test[-2],type='prob');         roc_rfA5 = roc(A5_test[,2], as.numeric(pred_rfA5[[2]]));          auc_rfA5 <- roc_rfA5$auc;         auc_rfA5
#-4 -> +4
  ppi_A4all <- read.csv("./polyA.csv");      ppi_A4all <- ppi_A4all[, -c(1:2,5:10,19:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
  ppi_A4all$ppi <- factor(ppi_A4all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
  indexA4 <- createDataPartition(ppi_A4all$ppi, p = 0.7, list = FALSE);  A4_train <- ppi_A4all[indexA4, ];  A4_test  <- ppi_A4all[-indexA4, ]   #Index to obtain 70% of data randomly & split 70/30
  rf_ppi_A4 <- train(ppi ~ ., data = A4_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
  pred_A4_rf = predict(rf_ppi_A4,newdata=A4_test[-2],type='raw');                          cm_rf_A4 = as.data.frame(table(A4_test[,2], as.numeric(pred_A4_rf))) #Test
  cm_rf_A4<-caret::confusionMatrix(pred_A4_rf,as.factor(A4_test$ppi),mode="everything");   cm_rf_A4
  pred_rfA4= predict(rf_ppi_A4,newdata=A4_test[-2],type='prob');         roc_rfA4 = roc(A4_test[,2], as.numeric(pred_rfA4[[2]]));          auc_rfA4 <- roc_rfA4$auc;         auc_rfA4
#-3 -> +3
  ppi_A3all <- read.csv("./polyA.csv");      ppi_A3all <- ppi_A3all[, -c(1:2,5:11,18:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
  ppi_A3all$ppi <- factor(ppi_A3all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
  indexA3 <- createDataPartition(ppi_A3all$ppi, p = 0.7, list = FALSE);  A3_train <- ppi_A3all[indexA3, ];  A3_test  <- ppi_A3all[-indexA3, ]   #Index to obtain 70% of data randomly & split 70/30
  rf_ppi_A3 <- train(ppi ~ ., data = A3_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
  pred_A3_rf = predict(rf_ppi_A3,newdata=A3_test[-2],type='raw');                          cm_rf_A3 = as.data.frame(table(A3_test[,2], as.numeric(pred_A3_rf))) #Test
  cm_rf_A3<-caret::confusionMatrix(pred_A3_rf,as.factor(A3_test$ppi),mode="everything");   cm_rf_A3
  pred_rfA3= predict(rf_ppi_A3,newdata=A3_test[-2],type='prob');         roc_rfA3 = roc(A3_test[,2], as.numeric(pred_rfA3[[2]]));          auc_rfA3 <- roc_rfA3$auc;         auc_rfA3
#-2 -> +2
  ppi_A2all <- read.csv("./polyA.csv");      ppi_A2all <- ppi_A2all[, -c(1:2,5:12,17:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
  ppi_A2all$ppi <- factor(ppi_A2all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
  indexA2 <- createDataPartition(ppi_A2all$ppi, p = 0.7, list = FALSE);  A2_train <- ppi_A2all[indexA2, ];  A2_test  <- ppi_A2all[-indexA2, ]   #Index to obtain 70% of data randomly & split 70/30
  rf_ppi_A2 <- train(ppi ~ ., data = A2_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
  pred_A2_rf = predict(rf_ppi_A2,newdata=A2_test[-2],type='raw');                          cm_rf_A2 = as.data.frame(table(A2_test[,2], as.numeric(pred_A2_rf))) #Test
  cm_rf_A2<-caret::confusionMatrix(pred_A2_rf,as.factor(A2_test$ppi),mode="everything");   cm_rf_A2
  pred_rfA2= predict(rf_ppi_A2,newdata=A2_test[-2],type='prob');         roc_rfA2 = roc(A2_test[,2], as.numeric(pred_rfA2[[2]]));          auc_rfA2 <- roc_rfA2$auc;         auc_rfA2
#-1 -> +1
  ppi_A1all <- read.csv("./polyA.csv");      ppi_A1all <- ppi_A1all[, -c(1:2,5:13,16:24)]                                               #Load dataset    &   #Delete the AC and polyX positions
  ppi_A1all$ppi <- factor(ppi_A1all$ppi,levels = c("0", "1"),labels = c("no","yes"))                                            #Factor PPI variable
  indexA1 <- createDataPartition(ppi_A1all$ppi, p = 0.7, list = FALSE);  A1_train <- ppi_A1all[indexA1, ];  A1_test  <- ppi_A1all[-indexA1, ]   #Index to obtain 70% of data randomly & split 70/30
  rf_ppi_A1 <- train(ppi ~ ., data = A1_train, method = "rf",	metric = "ROC", trControl = ctrl)                               #Training
  pred_A1_rf = predict(rf_ppi_A1,newdata=A1_test[-2],type='raw');                          cm_rf_A1 = as.data.frame(table(A1_test[,2], as.numeric(pred_A1_rf))) #Test
  cm_rf_A1<-caret::confusionMatrix(pred_A1_rf,as.factor(A1_test$ppi),mode="everything");   cm_rf_A1
  pred_rfA1= predict(rf_ppi_A1,newdata=A1_test[-2],type='prob');         roc_rfA1 = roc(A1_test[,2], as.numeric(pred_rfA1[[2]]));          auc_rfA1 <- roc_rfA1$auc;         auc_rfA1
#Plot all symmetricals
  a <- ggroc(list(M10P10 = roc_rfA10, M9P9 = roc_rfA9, M8P8 = roc_rfA8, M7P7 = roc_rfA7, M6P6 = roc_rfA6, M5P5 = roc_rfA5, M4P4 = roc_rfA4, M3P3 = roc_rfA3, M2P2 = roc_rfA2, M1P1 = roc_rfA1), legacy.axes = T) +  theme_bw() + geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),color="darkgrey") + ggtitle(paste0('B')) + theme(legend.position = c(0.87, 0.37),legend.title = element_blank())  + geom_text(x=0.10, y=0.95, label="polyA\nContext",color="black")
########################################################################## Plot both ########################################################################## 
  multiplot(q,a, cols=2)
library(caret)
library(rsample)
library(ranger)
library(tidyverse)
library(vip)

# 2022 
lb2022 <- read.csv("Z:/Late_blight/MRCfieldtest/planet22_infxviscaledrev.csv")
lb2022$year <- "2022"

# 2023 
lb2023 <- read.csv("Z:/Late_blight/MRCfieldtest/planet23_infxviscaledrev.csv")
lb2023$year <- "2023"

# Combine the years
lball <- rbind(lb2022, lb2023)

# mean_sev_wp is the response
lball$mean_sev_wp <- as.factor(lball$mean_sev_wp)

# Split for training/testing
set.seed(42)
trainindex <- createDataPartition(lball$mean_sev_wp, p = 0.70, list = F, times = 5)

# Splits 
training1 <- lball[trainindex[, 1], ]
testing1 <- lball[-trainindex[, 1], ]

# Call for random forest
carrf1 <- train(mean_sev_wp ~ ., data = training1, method = "ordinalRF",
                importance = T, trControl = trainControl(method = "cv", #classProbs = T
                                                         savePredictions = "final"))

# Predict
prrd <- predict(carrf1, testing1)
testing1$pred <-prrd
testing1$obs <- testing1$mean_sev_wp
testing1$diff <- testing1$pred == testing1$obs

# Draw confusion matrix
# Train 1
# Next line actually makes a simple text version of confusion matrix. I use it to tell me how to adjust the "annotate" lines of the ggplot.
tbl1 <- table(carrf1$pred$obs, carrf1$pred$pred)

# Save the simple table, otherwise, you have to run the model in every new session of R if you need to revise the ggplot.
write.csv(tbl1, "Z:/Late_blight/relatedtables/confusmetrix/rf_train_red_pred_1_conmat.csv", row.names = F)

tblcm1 <- confusionMatrix(tbl1)
tcmtmt <- as.matrix(tblcm1$table)
tcmdf <- as.data.frame(tcmtmt)
cmtdf <- data.frame(x = rep(c(0,1,2,3,4,5,7,8,9,10), 10), 
                    y = rep(c(0,1,2,3,4,5,7,8,9,10), each = 10), vals = tcmdf$Freq)
cmtdf$id <- paste0(cmtdf$x,"_",cmtdf$y)
cmtdf2 <- data.frame(y = rep(seq(0,11), 12), x = rep(seq(0,11), each=12 ), vals = 0)
cmtdf2$id <- paste0(cmtdf2$x,"_",cmtdf2$y)
cmtdf2$newval <- NA
cmtdf2$newval <- cmtdf[match(cmtdf2$id, cmtdf$id), "vals"]
cmtdf2$newval <- ifelse(is.na(cmtdf2$newval), 0, cmtdf2$newval)

ggplot(cmtdf2) + geom_tile(aes(x, y, fill = newval)) + 
  scale_fill_continuous(name = "") + 
  labs(title = "Confusion Matrix for Random Forest Training Reduced Predictor Set", x = "True", y = "Predicted") +
  scale_x_continuous(breaks = seq(0,11), expand = c(0,0),labels = c("zero",
                                                                    "one","two","three","four","five","six","seven","eight","nine","ten","eleven")) + 
  scale_y_continuous(breaks = seq(0,11), expand = c(0,0), 
                     labels = c("zero","one","two","three","four","five","six","seven","eight","nine",
                                "ten","eleven")) +
  theme(text = element_text(face = "bold"), panel.grid = element_blank()) +
  # These all have to be adjusted according to "tbl1" 
  annotate("text", label = "87", x = 0, y = 0, size = 6, color = "black") +
  annotate("text", label = "7", x = 1, y = 1, size = 6, color = "white") +
  annotate("text", label = "1", x = 1, y = 0, size = 6, color = "white") +
  annotate("text", label = "2", x = 2, y = 0, size = 6, color = "white") +
  annotate("text", label = "", x = 3, y = 0, size = 6, color = "white")+
  annotate("text", label = "1", x = 4, y = 0, size = 6, color = "white") +
  annotate("text", label = "2", x = 5, y = 0, size = 6, color = "white") +
  annotate("text", label = "1", x = 0, y = 8, size = 6, color = "white") +
  annotate("text", label = "2", x = 8, y = 0, size = 6, color = "white") +
  annotate("text", label = "1", x = 0, y = 9, size = 6, color = "white") +
  annotate("text", label = "1", x = 1, y = 2, size = 6, color = "white")+
  annotate("text", label = "1", x = 2, y = 1, size = 6, color = "white")+
  annotate("text", label = "1", x = 8, y = 10, size = 6, color = "white") +
  annotate("text", label = "13", x = 2, y = 2, size = 6, color = "white")+
  annotate("text", label = "5", x = 3, y = 3, size = 6, color = "white")+
  annotate("text", label = "8", x = 4, y = 4, size = 6, color = "white") +
  annotate("text", label = "", x = 5, y = 5, size = 6, color = "white") +
  annotate("text", label = "2", x = 7, y = 7, size = 6, color = "white")+
  annotate("text", label = "18", x = 8, y = 8, size =6, color = "white")+
  annotate("text", label = "4", x = 9, y = 9, size = 6, color = "white")+
  annotate("text", label = "7", x = 10, y = 10, size = 6, color = "white")+
  annotate("text", label = "1", x = 1, y = 10, size = 6, color = "white")+
  annotate("text", label = "1", x = 4, y = 8, size = 6, color = "white")+
  annotate("text", label = "1", x = 5, y = 8, size = 6, color = "white")+
  annotate("text", label = "1", x = 9, y = 0, size = 6, color = "white")+
  annotate("text", label = "1", x = 9, y = 2, size = 6, color = "white") +
  annotate("text", label = "1", x = 3, y = 4, size = 6, color = "white") +
  annotate("text", label = "1", x = 2, y = 9, size = 6, color = "white") +
  annotate("text", label = "1", x = 0, y = 9, size = 6, color = "white") +
  annotate("text", label = "1", x = 2, y = 9, size = 6, color = "white") +
  annotate("text", label = "1", x = 8, y = 10, size = 6, color = "white") 

# Produce and user accuracies. This has to be adjusted according to number of categories in response.
prod_user_ac_tr_1 <- data.frame(cat = c(0,1,2,3,4,5,7,8,9,10), 
  prod_a = diag(tbl1) / rowSums(tbl1) * 100,
  user_a = diag(tbl1) / colSums(tbl1) * 100)
# Save these tables
write.csv(prod_user_ac_tr_1, "Z:/Late_blight/relatedtables/confusmetrix/prod_user_acc_REDPRD_trn_1.csv", row.names = F)

# Prediction plot, same a previous basically
tbl2 <- table(testing1$obs, testing1$pred)
write.csv(tbl2, "Z:/Late_blight/relatedtables/confmat_prd_REDPRD_1.csv", row.names = F)
tblcm2 <- confusionMatrix(tbl2)
tcmtmt2 <- as.matrix(tblcm2$table)
tcmdf2 <- as.data.frame(tcmtmt2)
cmtdf3 <- data.frame(x = rep(c(0,1,2,3,4,5,7,8,9,10),), 
                    y = rep(c(0,1,2,3,4,5,7,8,9,10), each = 10), vals = tcmdf2$Freq)
cmtdf3$id <- paste0(cmtdf3$x,"_",cmtdf3$y)
cmtdf4 <- data.frame(y = rep(seq(0,11), 12), x = rep(seq(0,11), each=12 ))
cmtdf4$id <- paste0(cmtdf4$x,"_",cmtdf4$y)
cmtdf4$newval <- NA
cmtdf4$newval <- cmtdf3[match(cmtdf4$id, cmtdf3$id), "vals"]
cmtdf4$newval <- ifelse(is.na(cmtdf4$newval), 0, cmtdf4$newval)

ggplot(cmtdf4) + geom_tile(aes(x, y, fill = newval)) + 
  scale_fill_continuous(name = "") + 
  labs(title = "Confusion Matrix for Random Forest Prediction Reduced Predictor Set", x = "True", y = "Predicted") +
  scale_x_continuous(breaks = seq(0,11), expand = c(0,0),labels = c("zero",
  "one","two","three","four","five","six","seven","eight","nine","ten","eleven")) + 
  scale_y_continuous(breaks = seq(0,11), expand = c(0,0), 
                     labels = c("zero","one","two","three","four","five","six","seven","eight","nine",
                                "ten","eleven")) +
  theme(text = element_text(face = "bold"), panel.grid = element_blank()) + 
  annotate("text", label = "36", x = 0, y = 0, size = 6, color = "black") +
  annotate("text", label = "4", x = 1, y = 1, size = 6, color = "white") +
  annotate("text", label = "7", x = 2, y = 2, size = 6, color = "white")+
  annotate("text", label = "1", x = 5, y = 5, size = 6, color = "white")+
  annotate("text", label = "3", x = 4, y = 4, size = 6, color = "white") +
  annotate("text", label = "5", x = 8, y = 8, size =6, color = "white")+
  annotate("text", label = "2", x = 9, y = 9, size = 6, color = "white")+
  annotate("text", label = "3", x = 10, y = 10, size = 6, color = "white") +
  annotate("text", label = "", x = 1, y = 0, size = 6, color = "white") +
  annotate("text", label = "3", x = 8, y = 0, size = 6, color = "white") +
  annotate("text", label = "1", x = 4, y = 0, size = 6, color = "white")+
  annotate("text", label = "1", x = 0, y = 1, size = 6, color = "white") +
  annotate("text", label = "1", x = 0, y = 8, size = 6, color = "white")+
  annotate("text", label = "1", x = 3, y = 4, size = 6, color = "white")+
  annotate("text", label = "1", x = 3, y = 0, size = 6, color = "white")+
  annotate("text", label = "", x = 1, y = 10, size = 6, color = "white")

# Produce and user accuracies. This has to be adjusted according to number of categories in response.
prod_user_ac_prd_1 <- data.frame(cat = c(0,1,2,3,4,5,7,8,9,10), 
                           prod_a = diag(tbl2) / rowSums(tbl2) * 100,
                           user_a = diag(tbl2) / colSums(tbl2) * 100)
write.csv(prod_user_ac_prd_1, "Z:/Late_blight/relatedtables/confusmetrix/prod_user_acc_pred_REDPRD_1.csv", row.names = F)

# Uses the MLmetrics pacakge. 
MLmetrics::F1_Score(carrf1$pred$obs, carrf1$pred$pred)
MLmetrics::Recall(carrf1$pred$obs, carrf1$pred$pred)
MLmetrics::Sensitivity(carrf1$pred$obs, carrf1$pred$pred)
# The package doesn't include specificity. Specificity is calcuated by taking the values on diagonal and dividing by 
# the sum of the diagonal and the false positives (values above the diagonal).
# Specificity training
(88+7+14+5+9+2+2+18+6+6) / (88+7+14+5+9+2+2+18+6+6+1+1+1+1+2+1+1+1)
0.95

# Prediction (or 'testing')
MLmetrics::F1_Score(testing1$obs, testing1$pred)
MLmetrics::Recall(testing1$obs, testing1$pred)
MLmetrics::Sensitivity(testing1$obs, testing1$pred)
# Specificity prediction
(36+4+7+3+1+5+2+3) / (36+4+7+3+1+5+2+3+1+1+1)
0.95




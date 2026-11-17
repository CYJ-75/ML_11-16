#載入套件
#install.packages(c("rpart", "rpart.plot", "caTools","ggplot2"))
library(rpart)        #決策樹
library(rpart.plot)   #決策樹視覺化
library(ggplot2)      #繪圖
library(randomForest) #隨機森林
library(gridExtra)    #圖表排列

#讀取與清理棒球資料
Baseball <- read.table("C:/Users/Jenny/Desktop/機器學習/ball.txt", header = TRUE, sep = "", quote = "\"", dec = ".")
head(Baseball)
dim(Baseball)

#移除Id、Name與Salary，保留Logsalary作為目標變數
Baseball_clean <- subset(Baseball, select = -c(Id, Name, Salary))
Baseball_clean <- na.omit(Baseball_clean) #移除遺漏值

#將文字欄位轉為因子(factor)，確保rpart與randomForest正常運作
Baseball_clean[sapply(Baseball_clean, is.character)] <- lapply(Baseball_clean[sapply(Baseball_clean, is.character)], as.factor)

#切分訓練集(80%)與測試集(20%)
set.seed(123)
train_idx <- sample(1:nrow(Baseball_clean), size = 0.8 * nrow(Baseball_clean))

train_data <- Baseball_clean[train_idx, ]
test_data <- Baseball_clean[-train_idx, ]

dim(train_data)
dim(test_data)

#決策樹模型一(fit1)：排除多類別變數(Team86,Pos86,Team87)
set.seed(123)
fit1 <- rpart(Logsalary ~ Bat86 + Hit86 + Hr86 + Run86 + 
                Rb86 + Wlk86 + Yrs + Batcr + 
                Hitcr + Hrcr + Runcr + Rbcr +
                Wlkcr + League86 + Div86 + 
                Puto86 + Asst86 + Err86 + League87,
              method = "anova",
              data = train_data)

windows()
rpart.plot(fit1, main = "Regression Tree 1 (Without Team & Pos)")

#變數重要性
fit1$variable.importance

#預測與計算誤差
pred.fit1.tree <- predict(fit1, newdata = test_data)
mse.tree1 <- mean((pred.fit1.tree - test_data$Logsalary)^2)
rmse.tree1 <- sqrt(mse.tree1)

#QQ-Plot比較圖
windows()
qqplot(pred.fit1.tree, test_data$Logsalary,
       xlim = c(4, 8), ylim = c(4, 8),
       xlab = "Predicted Values", ylab = "Observed Values",
       main = "Regression Tree 1 (Without Team & Pos)", pch = 19)
abline(a = 0, b = 1, col = "red", lty = 2)

#決策樹模型二(fit2)：包含所有變數(含Team86,Pos86,Team87)
set.seed(123)
fit2 <- rpart(Logsalary ~ .,
              method = "anova",
              data = train_data)

windows()
rpart.plot(fit2, main = "Regression Tree 2 (With All Variables)")

#變數重要性
fit2$variable.importance

#預測與計算誤差
pred.fit2.tree <- predict(fit2, newdata = test_data)
mse.tree2  <- mean((pred.fit2.tree - test_data$Logsalary)^2)
rmse.tree2 <- sqrt(mse.tree2)

#QQ-Plot比較圖
windows()
qqplot(pred.fit2.tree, test_data$Logsalary,
       xlim = c(4, 8), ylim = c(4, 8),
       xlab = "Predicted Values", ylab = "Observed Values",
       main = "Regression Tree 2 (With All Variables)", pch = 19)
abline(a = 0, b = 1, col = "red", lty = 2)

#隨機森林模型一(rf.fit1)：排除多類別變數(對應fit1)
set.seed(123)
rf.fit1 <- randomForest(Logsalary ~ Bat86 + Hit86 + Hr86 + Run86 + 
                          Rb86 + Wlk86 + Yrs + Batcr + 
                          Hitcr + Hrcr + Runcr + Rbcr +
                          Wlkcr + League86 + Div86 + 
                          Puto86 + Asst86 + Err86 + League87,
                        data = train_data,
                        ntree = 500,
                        mtry = 6,
                        nodesize = 5,
                        importance = TRUE)

print(rf.fit1)
windows()
varImpPlot(rf.fit1, main = "Variable Importance: RF 1 (Without Team & Pos)")

pred.rf1 <- predict(rf.fit1, newdata = test_data)
mse.rf1  <- mean((pred.rf1 - test_data$Logsalary)^2)
rmse.rf1 <- sqrt(mse.rf1)

#隨機森林模型二(rf.fit2)：包含所有變數(對應fit2)
set.seed(123)
rf.fit2 <- randomForest(Logsalary ~ .,
                        data = train_data,
                        ntree = 500,
                        mtry = 7,
                        nodesize = 5,
                        importance = TRUE)

print(rf.fit2)
windows()
plot(rf.fit2, main = "Random Forest 2: OOB MSE")
windows()
varImpPlot(rf.fit2, main = "Variable Importance: RF 2 (With All Variables)")

pred.rf2 <- predict(rf.fit2, newdata = test_data)
mse.rf2  <- mean((pred.rf2 - test_data$Logsalary)^2)
rmse.rf2 <- sqrt(mse.rf2)


#四種模型績效總比較(MSE與RMSE越小越好)
results <- data.frame(
  Model = c("Tree 1 (No Team/Pos)", 
            "Tree 2 (All Vars)", 
            "Random Forest 1 (No Team/Pos)", 
            "Random Forest 2 (All Vars)"),
  MSE   = c(mse.tree1, mse.tree2, mse.rf1, mse.rf2),
  RMSE  = c(rmse.tree1, rmse.tree2, rmse.rf1, rmse.rf2)
)
print(results)

#視覺化比較：預測值 vs. 實際 Logsalary
p1 <- ggplot(data.frame(Predicted = pred.fit1.tree, Observed = test_data$Logsalary), aes(x = Predicted, y = Observed)) +
  geom_point(color = "orange") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = 2) +
  coord_fixed(xlim = c(4, 8), ylim = c(4, 8)) +
  labs(title = "Tree 1 (No Team/Pos)") +
  theme_bw()

p2 <- ggplot(data.frame(Predicted = pred.fit2.tree, Observed = test_data$Logsalary), aes(x = Predicted, y = Observed)) +
  geom_point(color = "darkorange3") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = 2) +
  coord_fixed(xlim = c(4, 8), ylim = c(4, 8)) +
  labs(title = "Tree 2 (All Vars)") +
  theme_bw()

p3 <- ggplot(data.frame(Predicted = pred.rf1, Observed = test_data$Logsalary), aes(x = Predicted, y = Observed)) +
  geom_point(color = "mediumseagreen") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = 2) +
  coord_fixed(xlim = c(4, 8), ylim = c(4, 8)) +
  labs(title = "RF 1 (No Team/Pos)") +
  theme_bw()

p4 <- ggplot(data.frame(Predicted = pred.rf2, Observed = test_data$Logsalary), aes(x = Predicted, y = Observed)) +
  geom_point(color = "darkgreen") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = 2) +
  coord_fixed(xlim = c(4, 8), ylim = c(4, 8)) +
  labs(title = "RF 2 (All Vars)") +
  theme_bw()

windows()
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

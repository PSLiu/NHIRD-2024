### 初始化 ----



# ~ 使用套件 ----

# 下載
pack_need <- c("data.table", "ggplot2", "fastDummies", "pROC", "class")
install.packages(pack_need, dependencies = TRUE)
rm(pack_need)

# library
library(data.table) # 資料操作套件
library(fastDummies) # 欄位轉譯套件
library(pROC) # ROC分析
library(class) # knn
library(ggplot2) # 視覺化
library(rpart) # 決策樹
library(rpart.plot)

# 路徑物件
path_temp <- "C:/Users/liu/Dropbox/Teaching/0-HDMRP-2024/prediction-model"



# ~ 課程資料 ----

# 讀取資料
setwd(path_temp)
dt <- fread("pneumonia.csv")

# 整體摘要性統計
summary(dt)

# 前20筆資料
head(dt, 20)

# 數值型資料描述性統計分析
summary(dt$age) # 連續變數 age

summary(dt$cci) # 連續變數 cci

summary(dt$bed_day) # 連續變數 bed_day

# 類別型資料描述性統計分析
table(dt$sex) # 類別變數 sex，1 = 男性；2 = 女性

table(dt$agegp) # 類別變數 agegp，0 = 65歲以下；1 = 65歲(含)以上

table(dt$incgp) # 類別變數 incgp，依據承保檔投保金額區分為0 = 15,840以下；1 = 15,840-30,000；2 = 30,000以上

table(dt$med_center) # 類別變數 med_center，0 = 在非醫學中心接受治療；1 = 在醫學中心接受治療

table(dt$pneumonia_pre) # 類別變數 pneumonia_pre，0 = 過去半年沒有曾經因為肺炎住院；1 = 過去半年曾經因為肺炎住院

table(dt$htn) # 類別變數 htn ，0 = 過去半年沒有曾經因為HTN就診；1 = 過去半年曾經因為HTN就診

table(dt$dm) # 類別變數 dm ，0 = 過去半年沒有曾經因為DM就診；1 = 過去半年曾經因為DM就診

table(dt$hyperlipidemia) # 類別變數 hyperlipidemia，0 = 過去半年沒有曾經因為hyperlipidemia就診；1 = 過去半年曾經因為hyperlipidemia就診



# ~ 設計dummy variables ----

# 有幾個欄位非以0/1這類one-hot-code表示
table(dt$sex)
table(dt$incgp)

# 使用fastDummies::dummy_cols進行轉碼
dt <- dummy_cols(dt, select_columns = c("sex", "incgp"))

# 確認incgp的編碼轉譯
with(dt, table(incgp_0, incgp))
with(dt, table(incgp_1, incgp))
with(dt, table(incgp_2, incgp))

# 確認sex的編碼轉譯
with(dt, table(sex_1, sex))
with(dt, table(sex_2, sex))





### 迴歸模型預測(linear) ----

# 用於預測結果為連續變數時

# 命題：建構模型來預測病患住院天數

# ~ 瞭解資料 ----

# 新物件
dt_1 <- dt

# outcome
summary(dt_1$bed_day)



# ~ 分割資料 ----

# 原始樣本總數
totn <- nrow(dt_1)

# 設定亂數種子
set.seed(2188)

# 使用70%作為train dataset，另外30%作為valid dataset
prop <- 0.7
gplist <- sample(x = c("train", "valid"), size = totn, replace = T, prob = c(prop, 1 - prop))
table(gplist)

# 切割出訓練資料集
dt_1_train <- dt_1[gplist == "train"]

# 切割出驗證資料集
dt_1_valid <- dt_1[gplist == "valid"]



# ~ 建立模型 ----

# 模型架構可以獨立成為一個物件
fm <- bed_day ~ age + sex_2 + incgp_1 + incgp_2 + cci + pneumonia_pre + htn + dm + hyperlipidemia


# ~ 訓練模型 ----

# glm
model <- glm(fm, data = dt_1_train)

# 檢視
summary(model)

# 把預測值加入data中
dt_1_train$bed_day_pred <- predict(model, dt_1_train)
head(dt_1_train[, .(id, sex, age, bed_day, bed_day_pred)])

# 計算殘差
dt_1_train <- dt_1_train[, bed_day_resd := bed_day_pred - bed_day]

# 訓練資料集產生模型之解釋力(R-square)，越接近1越好
print(with(summary(model), 1 - deviance/null.deviance))

# 計算RMSE，越接近0越好
sqrt(mean(dt_1_train$bed_day_resd^2))

# 計算相關係數 Pearson's r，預測值與實際值的相關係數，越接近1越好
with(dt_1_train, cor(bed_day_pred, bed_day))

# 繪圖視覺化
ggplot(dt_1_train, aes(x = bed_day, y = bed_day_pred)) + geom_point(color = "red", size = 3) + geom_abline()

# 模型診斷
ggplot(dt_1_train, aes(x = bed_day_pred, y = bed_day_resd)) + geom_point(color = "black", size = 3) + geom_hline(yintercept = 0)



# ~ 驗證模型 ----

# 套用模型，建立預測結果的欄位
dt_1_valid$bed_day_pred <- predict(model, newdata = dt_1_valid)
head(dt_1_valid[, .(id, sex, age, bed_day, bed_day_pred)])

# 計算殘差
dt_1_valid <- dt_1_valid[, bed_day_resd := bed_day_pred - bed_day]

# 計算RMSE，越接近0越好
sqrt(mean(dt_1_valid$bed_day_resd^2))

# 計算相關係數 Pearson's r，預測值與實際值的相關係數，越接近1越好
with(dt_1_valid, cor(bed_day_pred, bed_day))

# 繪圖視覺化
ggplot(dt_1_valid, aes(x = bed_day, y = bed_day_pred)) + geom_point(color = "blue", size = 3) + geom_abline()

# 模型診斷
ggplot(dt_1_valid, aes(x = bed_day_pred, y = bed_day_resd)) + geom_point(color = "black", size = 3) + geom_hline(yintercept = 0)



# 整理記憶體
rm(dt_1, dt_1_train, dt_1_valid)
rm(totn, prop, gplist, fm, model)





### 迴歸模型預測(logistic) ----

# 預測結果為二元變數

# 命題：建構模型來預測病患6個月內的存活狀況

# ~ 瞭解資料 ----

# 新物件
dt_2 <- dt

# outcome
table(dt_2$death)



# ~ 分割資料 ----

# 原始樣本總數
totn <- nrow(dt_2)

# 設定亂數種子
set.seed(5050)

# 使用60%作為train dataset，另外40%作為valid dataset
prop <- 0.6
gplist <- sample(x = c("train", "valid"), size = totn, replace = T, prob = c(prop, 1 - prop))
table(gplist)

# 切割出訓練資料集
dt_2_train <- dt_2[gplist == "train"]

# 切割出驗證資料集
dt_2_valid <- dt_2[gplist == "valid"]



# ~ 建立模型 ----

# 模型架構可以獨立成為一個物件
fm <- death ~ age + sex_2 + incgp_1 + incgp_2 + med_center + cci + pneumonia_pre + htn + dm + hyperlipidemia


# ~ 訓練模型 ----

# glm
model <- glm(fm, data = dt_2_train, family = "binomial")

# 檢視
summary(model)

# 把預測值加入data中
dt_2_train$death_pred <- predict(model, dt_2_train, type = "response")

# 建構ROC物件
dt_2_train_roc <- roc(dt_2_train$death, dt_2_train$death_pred)

dt_2_train_cutoff <- coords(dt_2_train_roc, "best", ret = "threshold", best.method = "youden")

# 統計ROC曲線下面積，越接近1越好
auc(dt_2_train_roc)

# 繪製ROC曲線
plot(dt_2_train_roc, col = "red", lwd = 3)
text(x = 0.2, y = 0.2, paste0("AUROC = ", round(auc(dt_2_train_roc), 4)))



# ~ 驗證模型 ----

# 套用模型，建立預測結果的欄位
dt_2_valid$death_prob <- predict(model, dt_2_valid, type = "response")

# 建構ROC物件
dt_2_valid_roc <- roc(dt_2_valid$death, dt_2_valid$death_prob)

# 統計ROC曲線下面積，越接近1越好
auc(dt_2_valid_roc)

# 繪製ROC曲線
plot(dt_2_valid_roc, col = "blue", lwd = 3)
text(x = 0.2, y = 0.2, paste0("AUROC = ", round(auc(dt_2_valid_roc), 4)))

# 若預測機率大於訓練模型的cutoff則預測事件會發生
dt_2_valid <- dt_2_valid[, death_pred := 0][death_prob >= dt_2_train_cutoff$threshold, death_pred := 1]

print(dt_2_train_cutoff$threshold)
head(dt_2_valid[, .(id, death, death_pred, death_prob)])

# 交叉表
with(dt_2_valid, table(death, death_pred))




# 整理記憶體
rm(dt_2, dt_2_train, dt_2_valid)
rm(dt_2_train_roc, dt_2_valid_roc, dt_2_train_cutoff)
rm(totn, prop, gplist, fm, model)





### 分類預測KNN ----

# 用於outcome為多分類

# 命題：建構模型來預測病患住院天數會落在哪一個段落

# ~ 瞭解資料 ----

# 新物件
dt_3 <- dt

# outcome
summary(dt_3$bed_day)
table(dt_3$bed_day)

# 分組
dt_3 <- dt_3[, daygp := cut(x = bed_day, breaks = c(1, 5, 11, 31), labels = c(1:3), include.lowest = T)]
with(dt_3, table(bed_day, daygp))



# ~ 分割資料 ----

# 原始樣本總數
totn <- nrow(dt_3)

# 設定亂數種子
set.seed(8764)

# 使用60%作為train dataset，另外40%作為valid dataset
prop <- 0.6
gplist <- sample(x = c("train", "valid"), size = totn, replace = T, prob = c(prop, 1 - prop))
table(gplist)

# 切割出訓練資料集
dt_3_train <- dt_3[gplist == "train"]

# 切割出驗證資料集
dt_3_valid <- dt_3[gplist == "valid"]



# ~ 建立模型 ----

# 擷取變數，KNN比較特別，把資料為進去之前要用as.matrix轉換為矩陣物件
# *_iv代表independent variable，模型的因子，自變項
# *_dv代表dependent variable，模型的結果，依變項
dt_3_train_iv <- as.matrix(dt_3_train[, c("agegp", "sex_2", "incgp_1", "incgp_2", "med_center", "cci", "pneumonia_pre", "htn", "dm", "hyperlipidemia")])
dt_3_train_dv <- as.matrix(dt_3_train[, c("daygp")])
dt_3_valid_iv <- as.matrix(dt_3_valid[, c("agegp", "sex_2", "incgp_1", "incgp_2", "med_center", "cci", "pneumonia_pre", "htn", "dm", "hyperlipidemia")])
dt_3_valid_dv <- as.matrix(dt_3_valid[, c("daygp")])



# ~ 訓練模型 ----

# knn
dt_3_valid$daygp_pred <- knn(
  train = dt_3_train_iv,
  test = dt_3_valid_iv,
  cl = dt_3_train_dv,
  k = 3
)

head(dt_3_valid[, .(id, daygp, daygp_pred)])



# ~ 驗證模型 ----

# 確認
with(dt_3_valid, table(daygp, daygp_pred))

# 相同的比率
with(dt_3_valid, mean(daygp_pred == daygp))




# 整理記憶體
rm(dt_3, dt_3_train, dt_3_valid)
rm(dt_3_train_iv, dt_3_train_dv, dt_3_valid_iv, dt_3_valid_dv)
rm(totn, prop, gplist)





### 決策樹 ----

# 用於預測結果為連續變數/二元變數/分類變數的好用工具

# 命題：建構模型來預測病患6個月內的存活狀況

# ~ 瞭解資料 ----

# 新物件
dt_4 <- dt

# outcome
table(dt_4$death)



# ~ 分割資料 ----

# 原始樣本總數
totn <- nrow(dt_4)

# 設定亂數種子
set.seed(2188)

# 使用70%作為train dataset，另外30%作為valid dataset
prop <- 0.7
gplist <- sample(x = c("train", "valid"), size = totn, replace = T, prob = c(prop, 1 - prop))
table(gplist)

# 切割出訓練資料集
dt_4_train <- dt_4[gplist == "train"]

# 切割出驗證資料集
dt_4_valid <- dt_4[gplist == "valid"]



# ~ 建立模型 ----

# 模型架構可以獨立成為一個物件
fm <- death ~ age + sex_2 + incgp_1 + incgp_2 + cci + pneumonia_pre + htn + dm + hyperlipidemia


# ~ 訓練模型 ----

# 決策樹建模
tree_model <- rpart(fm, data = dt_4_train, method = "class")

# 繪製決策樹
rpart.plot(tree_model)

# 檢視
summary(tree_model)

# 把預測值加入data中
dt_4_train$death_pred <- predict(tree_model, dt_4_train, type = "prob")[, "1"]

# 建構ROC物件
dt_4_train_roc <- roc(dt_4_train$death, dt_4_train$death_pred)

dt_4_train_cutoff <- coords(dt_4_train_roc, "best", ret = "threshold", best.method = "youden")

# 統計ROC曲線下面積，越接近1越好
auc(dt_4_train_roc)

# 繪製ROC曲線
plot(dt_4_train_roc, col = "red", lwd = 3)
text(x = 0.2, y = 0.2, paste0("AUROC = ", round(auc(dt_4_train_roc), 4)))



# ~ 驗證模型 ----

# 套用模型，建立預測結果的欄位
dt_4_valid$death_prob <- predict(tree_model, dt_4_valid, type = "prob")[, "1"]

# 建構ROC物件
dt_4_valid_roc <- roc(dt_4_valid$death, dt_4_valid$death_prob)

# 統計ROC曲線下面積，越接近1越好
auc(dt_4_valid_roc)

# 繪製ROC曲線
plot(dt_4_valid_roc, col = "blue", lwd = 3)
text(x = 0.2, y = 0.2, paste0("AUROC = ", round(auc(dt_4_valid_roc), 4)))

# 若預測機率大於訓練模型的cutoff則預測事件會發生
dt_4_valid <- dt_4_valid[, death_pred := 0][death_prob >= dt_4_train_cutoff$threshold, death_pred := 1]

print(dt_4_train_cutoff$threshold)
head(dt_4_valid[, .(id, death, death_pred, death_prob)])

# 交叉表
with(dt_4_valid, table(death, death_pred))




# 整理記憶體
rm(dt_4, dt_4_train, dt_4_valid)
rm(dt_4_train_roc, dt_4_valid_roc, dt_4_train_cutoff)
rm(totn, prop, gplist, fm, tree_model)





### 想知道更多 ----

#  Supervised learning
#    Continuous outcome
#      Linear regression
#    Count outcome
#      Poisson regression
#    Binary outcome
#      Logistic regression
#    Categorical outcome
#      kNN
#      Bayes
#      Classification Trees
#    Enhance Algorithm
#      Random forest
#      SVM, Support Vector Machine
#      GBM, Gradient Boosting Machines
#      GAM, Generalized Additive Model
#      xgboost
# Unsupervised learning
#    k-means
#    Hierarchical clustering
#    PCA, Principle Component Analysis





### END ###
### Author: Peter Liu 劉品崧 ###
### Contact: psliu520@gmail.com ###
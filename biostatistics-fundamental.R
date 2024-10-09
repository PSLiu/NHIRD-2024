# ~ 專案設定 ----

# 套件安裝
install.packages(c("data.table", "ggplot2", "pROC", "tableone", "MatchIt", "survminer"), dependencies = T)

# 資料路徑
path_temp <- "C:/Users/liu/Dropbox/Teaching/HWDC-2024/submit/20240930-NCKU-biostatistics-fundamental/temp"

# 套件載入
library(data.table)
library(ggplot2)
library(pROC)
library(tableone)
library(MatchIt)
library(survival)
library(survminer)



# y = 連續變數 ----

# 資料讀取
setwd(path_temp)
trial <- fread("bp-rct.csv")
head(trial)
nrow(trial)
table(trial$txgp)

# 描述性統計分析
# 年齡、收縮壓前測、舒張壓前測的數值分布
summary(trial$age)
quantile(trial$sbp_pre)
quantile(trial$dbp_pre, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1))
hist(trial$sbp_pre, breaks = seq(120, 140, 2), probability = T)

# 皮爾森相關係數 Pearson’s r correlation coefficient
# 年齡、收縮壓前測、舒張壓前測、收縮壓後測、舒張壓後測的相關係數
cor(trial[, .(age, sbp_pre, dbp_pre, sbp_post, dbp_post)])

# 獨立樣本 t 檢定 Independent t-test 
# 檢定兩個介入組別在收縮壓前測、舒張壓前測是否有差異
var.test(sbp_pre ~ txgp, data = trial)

t.test(sbp_pre ~ txgp, data = trial, var.equal = TRUE)

# 成對樣本 t 檢定 Paired t-test 
# 檢定介入組的收縮壓前測與收縮壓後測是否有差異
with(trial[txgp == 1], t.test(sbp_pre, sbp_post, paired = T))

# 檢定控制組的收縮壓前測與收縮壓後測是否有差異
with(trial[txgp == 0], t.test(sbp_pre, sbp_post, paired = T))

# 變異數分析 ANOVA (Analysis of variance) 事後檢定 post-hoc test
# 檢定三個年齡層之間在收縮壓後測是否有差異
trial_anova <- aov(sbp_post ~ factor(agegp), data = trial)
summary(trial_anova)
TukeyHSD(trial_anova, "factor(agegp)")

# 線性迴歸模型 - univariable analysis
# y = 收縮壓後測 x = 介入組別
trial_reg_univariable <- glm(sbp_post ~ txgp, data = trial)
summary(trial_reg_univariable)

# 線性迴歸模型 - multivariable analysis
# y = 收縮壓後測 x = 介入組別 + 性別 + 年齡 + 生活品質量表分數前測
trial_reg_multivariable <- glm(sbp_post ~ txgp + male + age + qol_scale_pre, data = trial)
summary(trial_reg_multivariable)



# y = 類別變數 ----

# 描述性統計分析
# 生活品質量表分數前測的次數分析
table(trial$qol_scale_pre)

# 長條圖 bar chart
barplot(table(trial$qol_scale_pre))

# 交叉分析表 cross table 卡方檢定 χ2 test
# 組別與前測生活品質不佳(分數 <= 5分)的交叉列聯表
table(trial$qol_bad_pre, trial$txgp)
with(trial, table(qol_bad_pre, txgp))
chisq.test(trial$qol_bad_pre, trial$txgp)

# 費氏精確檢定 Fisher exact test
fisher.test(trial$qol_bad_pre, trial$txgp)

# Logistic模型 - Logistic model - univariable analysis
# y = 生活品質不佳(分數 <= 5分)後測 x = 介入組別
trial_logistic_univariable <- glm(qol_bad_post ~ txgp, data = trial, family = binomial(link = "logit"))
summary(trial_logistic_univariable)

# Logistic模型 - Logistic model - multivariable analysis
# y = 生活品質不佳(分數 <= 5分)後測 x = 介入組別 + 性別 + 年齡
trial_logistic_multivariable <- glm(qol_bad_post ~ txgp + male + age, data = trial, family = binomial(link = "logit"))
summary(trial_logistic_multivariable)

# 將迴歸係數轉換成OR
tab_or_coefs <- summary(trial_logistic_multivariable)$coefficients
tab_or_names <- rownames(tab_or_coefs)
tab_or <- data.table(
  or_names = tab_or_names,
  or_point = exp(tab_or_coefs[, "Estimate"]),
  or_lower = exp(tab_or_coefs[, "Estimate"] - 1.96 * tab_or_coefs[, "Std. Error"]),
  or_upper = exp(tab_or_coefs[, "Estimate"] + 1.96 * tab_or_coefs[, "Std. Error"]),
  p_value  = tab_or_coefs[, "Pr(>|z|)"]
)
print(tab_or)
print(tab_or[or_names == "txgp"])

# Logistic模型 - Logistic model - multivariable analysis and ROC plot
trial$ps <- predict(trial_logistic_multivariable, data = trial, type = "response")
summary(trial$ps)
trial_logistic_multivariable_roc <- roc(trial$qol_bad_post, trial$ps)
auc(trial_logistic_multivariable_roc)
plot(trial_logistic_multivariable_roc, col = "red", lwd = 3)
text(x = 0.2, y = 0.2, paste0("AUROC = ", round(auc(trial_logistic_multivariable_roc), 4)))



# 干擾因子處理 ----

# 資料讀取
setwd(path_temp)
emulation <- fread("target-trial-emulation.csv")
head(emulation)

# 背景特質比較
emulation_tab1_original <- CreateTableOne(
  vars =       c("age", "male", "c2vs", "hyperlipidemia", "ckd", "cancer", "index_year_gp"),
  factorVars = c(       "male",         "hyperlipidemia", "ckd", "cancer", "index_year_gp"),
  strata = "oacs",
  data = emulation, test = F
)
print(emulation_tab1_original, smd = T)

# Logistic模型
emulation_psmodel <- glm(noac ~ age + male + c2vs + hyperlipidemia + ckd + cancer + year_2014_2015 + year_2016_2017, data = emulation, family = binomial(link = "logit"))
summary(emulation_psmodel)

# 傾向分數計算
emulation$ps <- predict(emulation_psmodel, data = emulation, type = "response")
summary(emulation$ps)

# 建立一個方便繪圖用的因素變項(factor variable)
emulation$noac_facvar <- factor(emulation$noac, c("1", "0"), c("NOAC", "Warfarin")) 

# 傾向分數分布(配對前)
ggplot(emulation, aes(x = ps, fill = noac_facvar)) +
  geom_histogram(
    position = "identity", alpha = 0.5, 
    breaks = seq(from = 0, to = 1, by = 0.05), binwidth = 0.05, color = "black") +
  scale_x_continuous("Propensity score", limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.25)) +
  scale_y_continuous("Frequency", limits = c(0, 1500), breaks = seq(from = 0, to = 1500, by = 300)) +
  labs(fill = "Treatment") +
  theme(
    legend.justification = c("left", "top"), 
    legend.position = c(0.05, .95))

# 傾向分數配對
emulation_matched <- matchit(noac ~ age + male + c2vs + hyperlipidemia + ckd + cancer + year_2014_2015 + year_2016_2017, data = emulation, caliper = 0.2, ratio = 1, discard = "treated")
emulation_matched <- match.data(emulation_matched)

emulation_matched <- emulation_matched[order(subclass, noac)]
head(emulation_matched[, .(subclass, id, ps, noac)], 10)

table(emulation_matched$noac)

# 背景特質比較(配對後)
emulation_tab1_matched <- CreateTableOne(
  vars = c("age", "male", "c2vs", "hyperlipidemia", "ckd", "cancer", "index_year_gp"),
  factorVars = c("male", "hyperlipidemia", "ckd", "cancer", "index_year_gp"),
  strata = "oacs",
  data = emulation_matched, test = F
)
print(emulation_tab1_matched, smd = T)

# 傾向分數分布(配對後)
ggplot(emulation_matched, aes(x = ps, fill = noac_facvar)) +
  geom_histogram(
    position = "identity", alpha = 0.5, 
    breaks = seq(from = 0, to = 1, by = 0.05), binwidth = 0.05, color = "black") +
  scale_x_continuous("Propensity score", limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.25)) +
  scale_y_continuous("Frequency", limits = c(0, 500), breaks = seq(from = 0, to = 500, by = 100)) +
  labs(fill = "Treatment") +
  theme(
    legend.justification = c("left", "top"), 
    legend.position = c(0.05, .95))



# 存活分析 ----

# 發生事件
with(emulation_matched, table(event_occur, noac))

# 觀察時間
summary(emulation_matched[noac == 0]$event_ft)
summary(emulation_matched[noac == 1]$event_ft)



# 計算分組觀察期間內結果事件發生率(每千人年)
emulation_tab2_ir_original <- emulation[, .(totN = .N, totEVENT = sum(event_occur), totFT = sum(event_ft), totIR = (sum(event_occur) / sum(event_ft)) * 1000), by = .(noac)]
emulation_tab2_ir_original <- emulation_tab2_ir_original[order(noac)]
print(emulation_tab2_ir_original)

emulation_tab2_ir_matched <- emulation_matched[, .(totN = .N, totEVENT = sum(event_occur), totFT = sum(event_ft), totIR = (sum(event_occur) / sum(event_ft)) * 1000), by = .(noac)]
emulation_tab2_ir_matched <- emulation_tab2_ir_matched[order(noac)]
print(emulation_tab2_ir_matched)



# 存活曲線 KM Curves 風險集人數 Number at risk
emulation_fig1_matched_model <- survfit(Surv(event_ft, event_occur) ~ noac, data = emulation_matched)

emulation_fig1_matched <- ggsurvplot(
  fit = emulation_fig1_matched_model, # 模型參數
  data = emulation_matched,         # 代入資料
  ylim = c(0, 1), break.y.by = 0.2,             # y軸的範圍與間距
  xlim = c(0, 7), break.x.by = 2,               # x軸的範圍與間距
  pval = TRUE, pval.coord = c(5, 0.8),          # 呈現log-rank test及座標
  palette = c("blue", "red"),                   # 線條的顏色
  censor = F,                                   # 不要呈現censor的符號
  legend.title = "Treatment",                   # 標籤標題
  legend.labs = c("Warfarin", "NOAC"),          # 標籤文字
  risk.table = T,                               # 風險集人數
  risk.table.height = 0.3)                      # 風險集佔據畫面的高度比例

print(emulation_fig1_matched)



# Cox迴歸模型 Cox proportional hazard regression
emulation_tab2_cox_original <- coxph(Surv(event_ft, event_occur) ~ noac + age + male + c2vs + hyperlipidemia + ckd + cancer + year_2014_2015 + year_2016_2017, data = emulation)
summary(emulation_tab2_cox_original)

emulation_tab2_cox_matched <- coxph(Surv(event_ft, event_occur) ~ noac + strata(subclass), data = emulation_matched)  # 加上strata
summary(emulation_tab2_cox_matched)

# 等比風險假設檢定 Test for proportional hazard assumption
cox.zph(emulation_tab2_cox_original)

cox.zph(emulation_tab2_cox_matched)





### END ###
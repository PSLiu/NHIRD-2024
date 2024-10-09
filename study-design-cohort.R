### 專案設定 ----

# 如果尚未安裝以下套件請進行下載
install.packages(c("data.table", "lubridate", "tableone", "survminer"))

# 載入套件
library(data.table)
library(lubridate)
library(tableone)
library(survival)
library(survminer)

# 路徑設定
path_base <- "D:/laboratory/project/HDMRP-2024/study-design-cohort/base"
path_temp <- "D:/laboratory/project/HDMRP-2024/study-design-cohort/temp"

# 物件命名系統
# ptot = population total

# vrit = variables for intervention / exposure
# vrot = variables for outcome
# vrcf = variables for confounding factors(demographics, disease history, medication history ... etc)

# hxit = history for intervention / control
# hxot = history for outcome

# dtmg = data merge
# dtan = data for analysis (fulfill including criteria)
# dtsg = data for subgroup (subgroup or sensitivity analysis)

# rsmx = result model  1, 2, ... n
# rstx = result table  1, 2, ... n
# rsfx = result figure 1, 2, ... n





### 資料撈取 ----



# ~ 暴露變項 ----



# 讀取門診費用檔
setwd(path_base)
vrit_1_opdte <- fread("h_nhi_opdte2014.csv", select = c("id", "func_date", "drug_day", "hosp_id", "fee_ym", "appl_type", "appl_date", "case_type", "seq_no"), colClasses = "character")
head(vrit_1_opdte)

# 就醫日期及用藥天數之文字轉日期
vrit_1_opdte <- vrit_1_opdte[, `:=`(func_date = ymd(func_date), drug_day = as.numeric(drug_day))]
head(vrit_1_opdte)



# 讀取藥品清單檔
setwd(path_base)
vrit_1_drugs <- fread("nhi_drug_list_20240229.csv", select = c("drug_no", "atc_code", "chem_name"), colClasses = "character")
head(vrit_1_drugs)

# 篩選出目標藥物健保代碼清單
vrit_1_drugs <- vrit_1_drugs[, `:=`(drug_cate = "none")]
vrit_1_drugs <- vrit_1_drugs[grepl("^B01AE07|^B01AF0[1-3]", atc_code), `:=`(drug_cate = "noac")]
vrit_1_drugs <- vrit_1_drugs[grepl("^B01AA03", atc_code), `:=`(drug_cate = "warf")]
table(vrit_1_drugs$drug_cate)

vrit_1_drugs <- vrit_1_drugs[drug_cate %in% c("noac", "warf")]
table(vrit_1_drugs$drug_cate)
head(vrit_1_drugs)

# 藥品碼去重複
vrit_1_drugs <- vrit_1_drugs[, .(drug_cate, drug_no)]
vrit_1_drugs <- unique(vrit_1_drugs)
table(vrit_1_drugs$drug_cate)
head(vrit_1_drugs)



# 讀取門診醫令檔
setwd(path_base)
vrit_1_opdto <- fread("h_nhi_opdto2014.csv", select = c("drug_no", "hosp_id", "fee_ym", "appl_type", "appl_date", "case_type", "seq_no"), colClasses = "character")
head(vrit_1_opdto)

# 篩選出目標藥物健保申報處方
vrit_1_opdto <- merge.data.table(vrit_1_opdto, vrit_1_drugs, by = c("drug_no"))
nrow(vrit_1_opdto)
head(vrit_1_opdto)



#  合併門診費用檔申報紀錄
vrit_1 <- merge.data.table(vrit_1_opdte, vrit_1_opdto, by = c("hosp_id", "fee_ym", "appl_type", "appl_date", "case_type", "seq_no"))
nrow(vrit_1)
head(vrit_1)

# 單一處方日數7天以上
vrit_2 <- vrit_1[7 <= drug_day]
summary(vrit_2)
nrow(vrit_2)

# 選出每人的第一次用藥
vrit_3 <- vrit_2
vrit_3 <- vrit_3[order(id, func_date)]
vrit_3 <- vrit_3[, .SD[1], by = .(id)]
vrit_3 <- vrit_3[, .(id, index_date = func_date, drug_cate)]
table(vrit_3$drug_cate)
head(vrit_3)
nrow(vrit_3)

# 創造虛擬變項
vrit_4 <- vrit_3
vrit_4 <- vrit_3[, `:=`(noac = 0)][drug_cate == "noac", `:=`(noac = 1)]

# 整理環境
vrit <- vrit_4
head(vrit)
summary(vrit)
table(vrit_3$drug_cate)

rm(list = ls(pattern = "^vrit_"))
gc()



# ~ 對應診斷 ----

# 讀取住院檔
setwd(path_base)
indx_1_ip <- fread("h_nhi_ipdte2014.csv", select = c("id", "in_date", "icd9cm_1", "icd9cm_2", "icd9cm_3", "icd9cm_4", "icd9cm_5"), colClasses = "character")
head(indx_1_ip)
setnames(indx_1_ip, "in_date", "func_date")
head(indx_1_ip)

indx_1_op <- fread("h_nhi_opdte2014.csv", select = c("id", "func_date", "icd9cm_1", "icd9cm_2", "icd9cm_3"), colClasses = "character")
head(indx_1_op)

# 找尋omdex date之前的紀錄
indx_1_ip <- indx_1_ip[vrit[, .(id, index_date)], on = .(id), nomatch = 0][ymd(func_date) <= index_date]
head(indx_1_ip)

indx_1_op <- indx_1_op[vrit[, .(id, index_date)], on = .(id), nomatch = 0][ymd(func_date) <= index_date]
head(indx_1_op)

# 資料轉置
indx_1_ip <- melt.data.table(indx_1_ip, c("id", "func_date"), c("icd9cm_1", "icd9cm_2", "icd9cm_3", "icd9cm_4", "icd9cm_5"), "icd", "code")
head(indx_1_ip)

indx_1_op <- melt.data.table(indx_1_op, c("id", "func_date"), c("icd9cm_1", "icd9cm_2", "icd9cm_3"), "icd", "code")
head(indx_1_op)

# 資料堆疊
indx_1 <- rbind(indx_1_ip, indx_1_op)

# 找出目標族群住院患者
indx_2 <- indx_1[grepl("^43[0-8]|^427", code)]
head(indx_2)

# 資料歸戶
indx_3 <- unique(indx_2[, .(id, indx = 1)])
head(indx_3)

# 整理環境
indx <- indx_3
head(indx)

rm(list = ls(pattern = "^indx_"))
gc()



# ~ 結果變項 ----

# 讀取死因檔
setwd(path_base)
vrot_1 <- fread("h_ost_death2014.csv", select = c("id", "d_date"), colClasses = "character")
head(vrot_1)

# 死亡日期之文字轉日期
vrot_2 <- vrot_1[, .(id, death_date = ymd(d_date))]
head(vrot_2)

# 主檔合併死亡紀錄
vrot_3_index <- vrit[, .(id, index_date)]
vrot_3 <- merge.data.table(vrot_3_index, vrot_2, by = c("id"))
head(vrot_3)

# 確認死亡登記日期要在index date(含)之後
vrot_3 <- vrot_3[index_date <= death_date]
head(vrot_3)

# 標註死亡變項
vrot_4 <- vrot_3[, .(id, death_occur = 1, death_date)]
head(vrot_4)

# 遺漏值補值
vrot_5_index <- vrit[, .(id)]
vrot_5 <- merge.data.table(vrot_5_index, vrot_4, by = c("id"), all.x = T)
head(vrot_5)
summary(vrot_5)

vrot_5 <- vrot_5[is.na(death_occur), `:=`(death_date = ymd("20141231"), death_occur = 0)]
head(vrot_5)
summary(vrot_5)

# 整理環境
vrot <- vrot_5
head(vrot)

rm(list = ls(pattern = "^vrot_"))
gc()



# ~ 基本資料 ----

# 讀取承保檔
setwd(path_base)
vrcf_insurance_1 <- fread("h_nhi_enrol2014.csv", select = c("id", "prem_ym", "id_s", "id_birth_y"), colClasses = "character")
head(vrcf_insurance_1)

# 留下ID, 投保日期(日期之文字轉日期), 出生年, 性別
vrcf_insurance_2 <- vrcf_insurance_1[, .(id, insurance_date = ymd(paste0(prem_ym, "01")), id_s, id_birth_y = as.numeric(id_birth_y))]
vrcf_insurance_2 <- vrcf_insurance_2[order(id, insurance_date)]
head(vrcf_insurance_2)

# 留下index date之前的投保紀錄
vrcf_insurance_3_index <- vrit[, .(id, index_date)]
vrcf_insurance_3 <- merge.data.table(vrcf_insurance_3_index, vrcf_insurance_2, by = c("id"))
vrcf_insurance_3 <- vrcf_insurance_3[insurance_date < index_date]
head(vrcf_insurance_3)

# 留下每人一筆距離index date最近的投保紀錄
vrcf_insurance_3 <- vrcf_insurance_3[order(id, -insurance_date)]
head(vrcf_insurance_3)
vrcf_insurance_3 <- vrcf_insurance_3[, .SD[1], by = .(id)]
head(vrcf_insurance_3)
nrow(vrcf_insurance_3)

# 性別
vrcf_insurance_4 <- vrcf_insurance_3[, .(id, id_s, id_birth_y, index_date, male = 0, female = 0, age = 0)]
head(vrcf_insurance_4)

vrcf_insurance_4 <- vrcf_insurance_4[id_s == "1", `:=`(male = 1)]
vrcf_insurance_4 <- vrcf_insurance_4[id_s == "2", `:=`(female = 1)]
head(vrcf_insurance_4)
with(vrcf_insurance_4, table(id_s, male))
with(vrcf_insurance_4, table(id_s, female))

# 年齡
vrcf_insurance_4 <- vrcf_insurance_4[, `:=`(age = year(index_date) - id_birth_y)]
head(vrcf_insurance_4)
summary(vrcf_insurance_4$age)

vrcf_insurance_4 <- vrcf_insurance_4[, .(id, id_s, id_birth_y, male, female, age)]
head(vrcf_insurance_4)

# 整理環境
vrcf_insurance <- vrcf_insurance_4
head(vrcf_insurance)

rm(list = ls(pattern = "^vrcf_insurance_"))
gc()



# ~ 疾病病史 ----



# 先處理門診檔



# 讀取門診檔
setwd(path_base)
vrcf_disease_1_op <- fread("h_nhi_opdte2014.csv", select = c("id", "func_date", "icd9cm_1", "icd9cm_2", "icd9cm_3"), colClasses = "character")
head(vrcf_disease_1_op)

# 就醫日期之文字轉日期
vrcf_disease_2_op <- vrcf_disease_1_op[, `:=`(func_date = ymd(func_date))]
head(vrcf_disease_2_op)

# 留下index date之前的就醫紀錄
vrcf_disease_3_op_index <- vrit[, .(id, index_date)]
vrcf_disease_3_op <- merge.data.table(vrcf_disease_3_op_index, vrcf_disease_2_op, by = c("id"))
head(vrcf_disease_3_op)

vrcf_disease_3_op <- vrcf_disease_3_op[func_date < index_date]
head(vrcf_disease_3_op)

# 將診斷碼轉置
vrcf_disease_4_op <- melt.data.table(
  data = vrcf_disease_3_op,
  id.vars = c("id", "func_date"),
  measure.vars = c("icd9cm_1", "icd9cm_2", "icd9cm_3"),
  variable.name = "icd", 
  value.name = "code"
)
head(vrcf_disease_4_op)
table(vrcf_disease_4_op$icd)

# 去除code為空白的觀察值
vrcf_disease_4_op <- vrcf_disease_4_op[!(code == "")]
head(vrcf_disease_4_op)
table(vrcf_disease_4_op$icd)

# 依照病人, 日期, 診斷碼去重複
vrcf_disease_4_op <- vrcf_disease_4_op[, .(id, func_date, code)]
vrcf_disease_4_op <- unique(vrcf_disease_4_op)
head(vrcf_disease_4_op)



# 再處理住院檔



# 讀取住院檔
setwd(path_base)
vrcf_disease_1_ip <- fread("h_nhi_ipdte2014.csv", select = c("id", "in_date", "icd9cm_1", "icd9cm_2", "icd9cm_3", "icd9cm_4", "icd9cm_5"), colClasses = "character")
head(vrcf_disease_1_ip)

# 就醫日期之文字轉日期
vrcf_disease_2_ip <- vrcf_disease_1_ip[, `:=`(func_date = ymd(in_date))]
head(vrcf_disease_2_ip)

# 留下index date之前的就醫紀錄
vrcf_disease_3_ip_index <- vrit[, .(id, index_date)]
vrcf_disease_3_ip <- merge.data.table(vrcf_disease_3_ip_index, vrcf_disease_2_ip, by = c("id"))
nrow(vrcf_disease_3_ip)

vrcf_disease_3_ip <- vrcf_disease_3_ip[func_date < index_date]
nrow(vrcf_disease_3_ip)
head(vrcf_disease_3_ip)

# 將診斷碼轉置
vrcf_disease_4_ip <- melt.data.table(
  data = vrcf_disease_3_ip,
  id.vars = c("id", "func_date"),
  measure.vars = c("icd9cm_1", "icd9cm_2", "icd9cm_3", "icd9cm_4", "icd9cm_5"),
  variable.name = "icd", 
  value.name = "code"
)
head(vrcf_disease_4_ip)
table(vrcf_disease_4_ip$icd)

# 去除code為空白的觀察值
vrcf_disease_4_ip <- vrcf_disease_4_ip[!(code == "")]
head(vrcf_disease_4_ip)
table(vrcf_disease_4_ip$icd)

# 依照病人, 日期, 診斷碼去重複
vrcf_disease_4_ip <- vrcf_disease_4_ip[, .(id, func_date, code)]
vrcf_disease_4_ip <- unique(vrcf_disease_4_ip)
head(vrcf_disease_4_ip)



# 疊加門診住院資料並給予權重
vrcf_disease_5 <- rbind(
  vrcf_disease_4_op[, .(id, func_date, code, weight = 1)],
  vrcf_disease_4_ip[, .(id, func_date, code, weight = 2)]
)



# 標記目標疾病
vrcf_disease_6 <- vrcf_disease_5[, .(id, func_date, code, weight, category = "none")]
vrcf_disease_6 <- vrcf_disease_6[grepl("^250", code), `:=`(category = "dm")]
vrcf_disease_6 <- vrcf_disease_6[grepl("^585", code), `:=`(category = "ckd")]
vrcf_disease_6 <- vrcf_disease_6[grepl("^48[0-6]", code), `:=`(category = "pneumonia")]
table(vrcf_disease_6$category)

# 去除無關紀錄
vrcf_disease_6 <- vrcf_disease_6[category != "none"]
head(vrcf_disease_6)
with(vrcf_disease_6, table(weight, category))

# 同人同日同類別，權重以較大者(住院)為主
vrcf_disease_6 <- vrcf_disease_6[, .(weight = max(weight)), by = .(id, func_date, category)]
head(vrcf_disease_6)
with(vrcf_disease_6, table(weight, category))

# 計算每人, 疾病的總計就診次數
vrcf_disease_7 <- vrcf_disease_6[, .(disease_count = sum(weight)), by = .(id, category)]
head(vrcf_disease_7)
with(vrcf_disease_7, table(category, disease_count))

# 超過兩次門診或一次住院的紀錄定義為有疾病病史
vrcf_disease_7 <- vrcf_disease_7[2 <= disease_count]
vrcf_disease_7 <- vrcf_disease_7[, .(id, category, disease_hx = 1)]
head(vrcf_disease_7)

# 病史轉置
vrcf_disease_8 <- dcast.data.table(
  data = vrcf_disease_7,
  formula = id ~ category,
  value.var = c("disease_hx")
)
head(vrcf_disease_8)
summary(vrcf_disease_8)

# 遺漏值補0
vrcf_disease_9_index <- vrit[, .(id)]
vrcf_disease_9 <- merge.data.table(vrcf_disease_9_index, vrcf_disease_8, by = c("id"), all.x = T)
head(vrcf_disease_9)
summary(vrcf_disease_9)

vrcf_disease_9[is.na(vrcf_disease_9)] <- 0
head(vrcf_disease_9)
summary(vrcf_disease_9)

# 整理環境
vrcf_disease <- vrcf_disease_9
head(vrcf_disease)

rm(list = ls(pattern = "^vrcf_disease_"))
gc()





### 資料整合 ----



# ~ 變數合併 ----

# 主檔資料
dtmg <- vrit
head(dtmg)

dtmg <- merge.data.table(dtmg, indx, by = c("id"), all.x = T)
head(dtmg)

dtmg <- merge.data.table(dtmg, vrot, by = c("id"), all.x = T)
head(dtmg)

dtmg <- merge.data.table(dtmg, vrcf_disease, by = c("id"), all.x = T)
head(dtmg)

dtmg <- merge.data.table(dtmg, vrcf_insurance, by = c("id"), all.x = T)
head(dtmg)

summary(dtmg)



# ~ 資訊處理 ----

# 住院日起的追蹤時間
dtmg <- dtmg[, `:=`(death_fu = as.numeric(death_date - index_date))]
head(dtmg)
summary(dtmg)

# 儲存資料
setwd(path_temp)
fwrite(dtmg, "dtmg.csv")



# ~ 樣本篩選 ----

# 進行選樣流程
dtan <- dtmg
summary(dtan)
nrow(dtan)

# 排除基本資料不全
table(dtan$id_s)
dtan <- dtan[id_s %in% c("1", "2")]
nrow(dtan)
table(dtan$id_s)

# 排除20歲(不含)以下
dtan <- dtan[!(age < 20)]
nrow(dtan)

# 排除沒有藥物對應適應症診斷者
summary(dtan$indx)
dtan <- dtan[indx == 1]
summary(dtan$indx)
nrow(dtan)

# 排除1-3或10-12月
dtan <- dtan[!(1 <= month(index_date) & month(index_date) <= 3)]
dtan <- dtan[!(10 <= month(index_date) & month(index_date) <= 12)]
nrow(dtan)

# 排除當天死亡的
dtan <- dtan[!(death_occur == 1 & death_fu == 0)]
nrow(dtan)

# 儲存資料
setwd(path_temp)
fwrite(dtan, "dtan.csv")

# 整理環境
rm(list = ls(pattern = "^ptot"))
rm(list = ls(pattern = "^indx"))
rm(list = ls(pattern = "^vr"))
gc()





### 統計分析 ----



# ~ 描述統計 ----

# 變項列表
all_var <- c("male", "age", "dm", "ckd", "pneumonia")
num_var <- c("age")

# 建立表格
rst1 <- CreateTableOne(
  data = dtan,
  strata = "drug_cate",
  vars = all_var,
  factorVars = all_var[!(all_var %in% num_var)],
  test = F
)

# 列印表格
print(rst1, smd = T, showAllLevels = T)



# ~ 推論統計 ----

# 發生率比較
rst2 <- dtan[, .(totN = .N, totEvent = sum(death_occur), totFU = sum(death_fu), totIR = (sum(death_occur) / sum(death_fu)) * 10000), by = .(drug_cate)]
rst2 <- rst2[order(drug_cate)]
print(rst2)

# 生存模型
rsm1 <- survfit(Surv(death_fu, death_occur) ~ noac, data = dtan)

# 生命表
summary(rsm1, times = seq(from = 0, to = 270, by = 30))

# KM curves - raw
ggsurvplot(rsm1, data = dtan)

# log-rank test
survdiff(Surv(death_fu, death_occur) ~ noac, data = dtan)

# KM curves - with appropriate mark
dtan$noac_mark <- factor(dtan$noac, c(1, 0))
rsm1_mark <- survfit(Surv(death_fu, death_occur) ~ noac_mark, data = dtan)

ggsurvplot(
  rsm1_mark, data = dtan, censor = F,
  palette = c("blue", "red"), 
  risk.table = T, risk.table.height = 0.3, 
  ylim = c(0.5, 1), break.y.by = 0.05,
  xlim = c(0, 270), break.x.by = 30,
  pval = TRUE, pval.coord = c(225, 0.70), 
  legend = c(0.85, 0.15), legend.title = "NOAC user", legend.labs = c("Yes", "No")
)

# 迴歸模型分析 Cox proportional hazard regression model - crude
rsm2 <- coxph(Surv(death_fu, death_occur) ~ noac, data = dtan)
summary(rsm2)

# 迴歸模型分析 Cox proportional hazard regression model - adjusted
rsm3 <- coxph(Surv(death_fu, death_occur) ~ noac + male + age + dm + ckd + pneumonia, data = dtan)
summary(rsm3)

# Proportional hazard assumption test - expected be non-significant
cox.zph(rsm3)

# Scaled Schoenfeld Residuals - expected be parallel
plot(cox.zph(rsm3))





### END ###
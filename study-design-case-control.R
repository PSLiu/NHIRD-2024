### 專案設定 ----

# 如果尚未安裝以下套件請進行下載
install.packages(c("data.table", "lubridate", "tableone"))

# 載入套件
library(data.table)
library(lubridate)
library(tableone)

# 路徑設定
path_base <- "D:/laboratory/project/HDMRP-2024/study-design-case-control/base"
path_temp <- "D:/laboratory/project/HDMRP-2024/study-design-case-control/temp"

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



# ~ 目標族群 ----

# 讀取住院檔
setwd(path_base)
ptot_1 <- fread("h_nhi_ipdte2014.csv", select = c("id", "in_date", "icd9cm_1"), colClasses = "character")
head(ptot_1)

# 找出目標族群住院患者
ptot_2 <- ptot_1[grepl("^48[0-6]", icd9cm_1)]
head(ptot_2)

# 住院日期之文字轉日期
ptot_3 <- ptot_2[, .(id, index_date = ymd(in_date))]
head(ptot_3)

# 找出2014年首次住院日index date
ptot_4 <- ptot_3[order(id, index_date)]
ptot_4 <- ptot_4[, .SD[1], by = .(id)]
head(ptot_4)

# 檢視住院月份
table(month(ptot_4$index_date))

# 整理環境
ptot <- ptot_4
head(ptot)

rm(list = ls(pattern = "^ptot_"))
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
vrot_3_index <- ptot[, .(id, index_date)]
vrot_3 <- merge.data.table(vrot_3_index, vrot_2, by = c("id"))
head(vrot_3)

# 確認死亡登記日期要在index date(含)之後
vrot_3 <- vrot_3[index_date <= death_date]
head(vrot_3)

# 標註死亡變項
vrot_4 <- vrot_3[, .(id, death_occur = 1, death_date)]
head(vrot_4)

# 遺漏值補值
vrot_5_index <- ptot[, .(id)]
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



# ~ 暴露變項 ----

# 讀取門診檔
setwd(path_base)
vrit_1 <- fread("h_nhi_opdte2014.csv", select = c("id", "func_date", "icd9cm_1", "icd9cm_2", "icd9cm_3"), colClasses = "character")
head(vrit_1)

# 就醫日期之文字轉日期
vrit_2 <- vrit_1[, `:=`(func_date = ymd(func_date))]
head(vrit_2)

# 留下index date之前的就醫紀錄
vrit_3_index <- ptot[, .(id, index_date)]
vrit_3 <- merge.data.table(vrit_3_index, vrit_2, by = c("id"))
vrit_3 <- vrit_3[func_date < index_date]
head(vrit_3)

# 找出DM診斷碼
vrit_4 <- vrit_3[grepl("^250", icd9cm_1) | grepl("^250", icd9cm_2) | grepl("^250", icd9cm_3)]
head(vrit_4)

# 依照門診日去除重複
vrit_5 <- vrit_4[, .(id, func_date)]
vrit_5 <- unique(vrit_5)
head(vrit_5)

# 確認日期
vrit_6 <- vrit_5[ymd("20140101") <= func_date & func_date <= ymd("20141231")]

# 計算今年DM門診就醫次數
vrit_7 <- vrit_6[, .(dm_count = .N), by = .(id)]
head(vrit_7)
summary(vrit_7)
table(vrit_7$dm_count)

# 留下至少2次門診就醫者
vrit_8 <- vrit_7[2 <= dm_count]
table(vrit_8$dm_count)

vrit_8 <- vrit_8[, .(id, dm_hx = 1)]
head(vrit_8)
summary(vrit_8)

# 遺漏值補0
vrit_9_index <- ptot[, .(id)]
vrit_9 <- merge.data.table(vrit_9_index, vrit_8, by = c("id"), all.x = T)
head(vrit_9)
summary(vrit_9)

vrit_9 <- vrit_9[is.na(dm_hx), `:=`(dm_hx = 0)]
head(vrit_9)
summary(vrit_9)

# 整理環境
vrit <- vrit_9
head(vrit)

rm(list = ls(pattern = "^vrit_"))
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
vrcf_insurance_3_index <- ptot[, .(id, index_date)]
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
vrcf_insurance_4 <- vrcf_insurance_4[, `:=`(age = year(index_date) - as.numeric(id_birth_y))]
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
vrcf_disease_3_op_index <- ptot[, .(id, index_date)]
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
vrcf_disease_3_ip_index <- ptot[, .(id, index_date)]
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
vrcf_disease_6 <- vrcf_disease_6[grepl("^40[1-5]", code), `:=`(category = "htn")]
vrcf_disease_6 <- vrcf_disease_6[grepl("^585", code), `:=`(category = "ckd")]
vrcf_disease_6 <- vrcf_disease_6[grepl("^43[0-8]", code), `:=`(category = "cva")]
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
vrcf_disease_9_index <- ptot[, .(id)]
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
dtmg <- ptot
head(dtmg)

dtmg <- merge.data.table(dtmg, vrit, by = c("id"))
head(dtmg)

dtmg <- merge.data.table(dtmg, vrot, by = c("id"))
head(dtmg)

dtmg <- merge.data.table(dtmg, vrcf_disease, by = c("id"))
head(dtmg)

dtmg <- merge.data.table(dtmg, vrcf_insurance, by = c("id"))
head(dtmg)

summary(dtmg)



# ~ 資訊處理 ----

# 確認是否在90天內死亡
dtmg <- dtmg[, `:=`(death_90day = 0)]
dtmg <- dtmg[death_occur == 1 & as.numeric(death_date - index_date) <= 90, `:=`(death_90day = 1)]
head(dtmg)
summary(dtmg)

# 儲存資料
setwd(path_temp)
fwrite(dtmg, "dtmg.csv")



# ~ 樣本篩選 ----

# 進行選樣流程
dtan <- dtmg
nrow(dtan)

# 排除基本資料不全
table(dtan$id_s)
dtan <- dtan[id_s %in% c("1", "2")]
table(dtan$id_s)
nrow(dtan)

# 排除20歲(不含)以下
summary(dtan$age)
dtan <- dtan[!(age < 20)]
summary(dtan$age)
nrow(dtan)

# 排除1-3或10-12月
with(dtan, table(month(index_date), year(index_date)))
dtan <- dtan[!(1 <= month(index_date) & month(index_date) <= 3)]
dtan <- dtan[!(10 <= month(index_date) & month(index_date) <= 12)]
with(dtan, table(month(index_date), year(index_date)))
nrow(dtan)

# 儲存資料
setwd(path_temp)
fwrite(dtan, "dtan.csv")

# 整理環境
rm(list = ls(pattern = "^ptot"))
rm(list = ls(pattern = "^vr"))
gc()





### 統計分析 ----



# ~ 描述統計 ----

# 變項列表
all_var <- c("male", "age", "dm_hx", "ckd", "htn", "cva")
num_var <- c("age")

# 建立表格
rst1 <- CreateTableOne(
  data = dtan,
  strata = "death_90day",
  vars = all_var,
  factorVars = all_var[!(all_var %in% num_var)],
  test = F
)

# 列印表格
print(rst1, smd = T, showAllLevels = T)



# ~ 推論統計 ----

# 迴歸模型分析 Logistic regression model - crude
rsm1 <- glm(death_occur ~ dm_hx, data = dtan, family = binomial("logit"))
summary(rsm1)

# 迴歸模型分析 Logistic regression model - adjusted
rsm2 <- glm(death_occur ~ dm_hx + male + age + ckd + htn + cva, data = dtan, family = binomial("logit"))
summary(rsm2)

# 整理報表數據
print(data.table(
  OR_point = exp(coef(rsm2)["dm_hx"]), 
  OR_lower = exp(confint(rsm2)["dm_hx", 1]), 
  OR_upper = exp(confint(rsm2)["dm_hx", 2]),
  p_value  = summary(rsm2)$coefficients["dm_hx", "Pr(>|z|)"]))





### END ###
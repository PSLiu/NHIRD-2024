# 專案設定 ----

# 套件載入
library(data.table)
library(lubridate)
library(tableone)

# 路徑建立
path_base <- "D:/laboratory/project/HDMRP-2024/data-management-technical-programming/r/base"
path_temp <- "D:/laboratory/project/HDMRP-2024/data-management-technical-programming/r/temp"



# 中風住院合併內出血 ----

# 讀取住院費用檔
setwd(path_base)
stroke_1 <- fread("h_nhi_ipdte2014.csv", colClasses = "character")
head(stroke_1)

# 選取必要欄位
stroke_1 <- stroke_1[, .(id, in_date = ymd(in_date), hosp_id, icd9cm_1, icd9cm_2, icd9cm_3, icd9cm_4, icd9cm_5)]
head(stroke_1)

# 撈出主診斷欄位為缺血性中風患者
stroke_2 <- copy(stroke_1)

stroke_2 <- stroke_2[grepl("^43[34]", icd9cm_1)]
head(stroke_2)

# 標記其他診斷具有出血性疾病診斷碼
stroke_3 <- copy(stroke_2)

stroke_3 <- stroke_3[, `:=`(bleeding = 0)]
stroke_3_code <- "^4230|^43[0-2]|^455[258]|^456[02]|^4590|^5307|^53082|^531[0–6]|^532[0-6]|^533[0-6]|^534[0–6]|^535[0-6]|^53783|^5620[23]|^5621[23]|^56881|^5693|^56985|^578[019]|^56881|^59381 |^5997|^6238|^62632|^6266|^7191|^784[78]|^7863|^852|^853"
stroke_3 <- stroke_3[grepl(stroke_3_code, icd9cm_2) | grepl(stroke_3_code, icd9cm_3) | grepl(stroke_3_code, icd9cm_4) | grepl(stroke_3_code, icd9cm_5), `:=`(bleeding = 1)]
head(stroke_3)
table(stroke_3$bleeding)

# 因為住院檔常有切帳申報問題，此處進行歸戶
stroke_4 <- copy(stroke_3)
stroke_4 <- stroke_4[, .(bleeding = max(bleeding)), by = .(id, in_date, hosp_id)]
head(stroke_4)

# 依據樣本ID及住院日期排序後，選取每個樣本今年的首次住院
stroke_5 <- copy(stroke_4)
head(stroke_5)

stroke_5 <- stroke_5[order(id, in_date)][, .SD[1], by = .(id)]
head(stroke_5)
summary(stroke_5)

# 整理環境
stroke <- copy(stroke_5)

rm(list = ls(pattern = "^stroke_"))
gc()





# 研究族群基本資料 ----

# 承保檔資料清單
setwd(path_base)
idfile_1_list <- list.files(pattern = "^h_nhi_enrol")

print(idfile_1_list)

# 建立處理函數
idfile_1_fn <- function(indt){
  
  # 測試用指標
  # indt <- idfile_1_list[[1]]
  
  # 讀取資料
  otdt_1 <- fread(indt, colClasses = "character")
  
  # 過濾資料
  otdt_1 <- otdt_1[stroke[, .(id)], on = .(id), nomatch = 0]
  
  # 保留欄位
  otdt_1 <- otdt_1[, .(id, id_birth_y = as.numeric(id_birth_y), id_s)]
  
  return(otdt_1)
}

# 運用lapply處理清單資料
idfile_1 <- lapply(idfile_1_list, idfile_1_fn)
class(idfile_1)
head(idfile_1)
str(idfile_1)

# 將清單資料堆疊後去重複
idfile_2 <- rbindlist(idfile_1)
idfile_2 <- unique(idfile_2)
head(idfile_2)

# 計算年齡，若有年齡小於20歲(不含)或大於100歲者(不含)者則刪除
idfile_3 <- copy(idfile_2)
idfile_3 <- idfile_3[, `:=`(age = 2014 - id_birth_y)]
summary(idfile_3$age)

# 確認性別，若有性別編碼非男女者則刪除
idfile_4 <- copy(idfile_3)

table(idfile_4$id_s)
idfile_4 <- idfile_4[id_s %in% c("1", "2")]
table(idfile_4$id_s)

idfile_4 <- idfile_4[, `:=`(male = 0)][id_s == "1", `:=`(male = 1)]
head(idfile_4)

# 整理環境
idfile <- copy(idfile_4)

rm(list = ls(pattern = "^idfile_"))
gc()



# 中風住院前藥物病史 ----

# 目標藥物清單
setwd(path_temp)
drug_1_list <- fread("druglist.csv", colClasses = "character")
head(drug_1_list)
table(drug_1_list$drug_type)

# 門診資料清單
setwd(path_base)
drug_1_file_opdte <- list.files(pattern = "^h_nhi_opdte")
drug_1_file_opdto <- list.files(pattern = "^h_nhi_opdto")

print(drug_1_file_opdte)
print(drug_1_file_opdto)

# 建立迴圈參數
drug_1_file_length <- length(drug_1_file_opdte)

# 建立輸出資料儲存清單
drug_1 <- vector("list", length(drug_1_file_opdte))

# 利用迴圈讀取資料及整理資料
for (ii in 1:drug_1_file_length) {
  
  # 測試用參數
  # ii <- 1
  
  # log on
  log_t1 <- Sys.time()
  
  # 讀取門診費用檔(opdte)
  setwd(path_base)
  indt_1_a <- fread(drug_1_file_opdte[[ii]], colClasses = "character")
  
  # 擷取必要欄位
  indt_1_a <- indt_1_a[, .(id, func_date = ymd(func_date), hosp_id, fee_ym, appl_date, appl_type, case_type, seq_no)]
  
  # 篩選研究樣本在住院日之前的資料
  indt_1_a <- merge.data.table(stroke[, .(id, in_date)], indt_1_a, by = c("id"))
  indt_1_a <- indt_1_a[func_date < in_date]
  
  # 讀取門診醫令檔(opdto)
  setwd(path_base)
  indt_1_b <- fread(drug_1_file_opdto[[ii]], colClasses = "character")
  
  # 擷取必要欄位
  indt_1_b <- indt_1_b[, .(drug_no, hosp_id, fee_ym, appl_date, appl_type, case_type, seq_no)]
  
  # 篩選目標藥物資料
  indt_1_b <- merge.data.table(drug_1_list, indt_1_b, by = c("drug_no"))
  
  # 合併門診費用檔
  indt_2 <- merge.data.table(indt_1_a, indt_1_b, by = c("hosp_id", "fee_ym", "appl_date", "appl_type", "case_type", "seq_no"))
  
  # 擷取必要欄位
  indt_2 <- unique(indt_2[, .(id, drug_use = 1)])
  
  # 輸出資料
  drug_1[[ii]] <- indt_2
  
  # log off
  log_t2 <- Sys.time()
  
  # log loop - time
  print(paste("overall", drug_1_file_length, "loop", ii, "from", log_t1, "to", log_t2))
  
  # 整理環境
  rm(list = ls(pattern = "^indt_"))
  gc()
}

rm(ii)
gc()

# 清單資料堆疊
drug_2 <- rbindlist(drug_1, use.names = T, fill = T)  
drug_2 <- unique(drug_2)

head(drug_2)

# 整理環境
drug <- copy(drug_2)

rm(list = ls(pattern = "^drug_"))
gc()



# 資料合併 ----

masterfile_1 <- stroke
masterfile_1 <- merge.data.table(masterfile_1, idfile, by = c("id"), all.x = T)
masterfile_1 <- merge.data.table(masterfile_1, drug,   by = c("id"), all.x = T)

masterfile_1 <- masterfile_1[is.na(drug_use), `:=`(drug_use = 0)]

head(masterfile_1)
summary(masterfile_1)
with(masterfile_1, table(drug_use, bleeding))




# 樣本篩選 ----

masterfile_2 <- copy(masterfile_1)
summary(masterfile_2)

# 排除承保基本資料缺失的樣本
masterfile_2 <- masterfile_2[!(is.na(age))]
summary(masterfile_2)

# 排除今年4月(不含)以前的樣本(無法有足夠時間確認抗凝血藥物使用)
masterfile_2 <- masterfile_2[!(in_date < ymd("20140401"))]
summary(masterfile_2)

# 整理環境
masterfile <- copy(masterfile_2)

rm(list = ls(pattern = "^masterfile_"))
rm(list = ls(pattern = "^stroke"))
rm(list = ls(pattern = "^idfile"))
rm(list = ls(pattern = "^drug"))
gc()



# 背景特性表格 ----

tab_1_var <- c("drug_use", "male", "age")
tab_1_fac <- c("drug_use", "male")

tab_1 <- CreateTableOne(
  data = masterfile,
  strata = "bleeding",
  vars = tab_1_var,
  factorVars = tab_1_fac,
  test = F
)

print(tab_1, smd = T, showAllLevels = TRUE)



# 統計分析 ----

# 兩組人數及用藥比例
tab_2_ratio <- masterfile[, .(totN = .N, totDRUG = sum(drug_use), pctDRUG = round(mean(drug_use) * 100, 2)), by = .(bleeding)][order(bleeding)]
print(tab_2_ratio)

# logistic model
tab_2_model <- glm(bleeding ~ drug_use + age + male, data = masterfile, family = binomial("logit"))
summary(tab_2_model)

# 確認報表物件的架構
str(summary(tab_2_model))

# 擷取係數、標準誤和p值
tab_2_coefs <- summary(tab_2_model)$coefficients
print(tab_2_coefs)

# 擷取變項名稱
tab_2_names <- rownames(tab_2_coefs)
print(tab_2_names)

# 組合成為報表
tab_2 <- data.table(
  vars = tab_2_names,
  or_point = exp(tab_2_coefs[, "Estimate"]),
  or_lower = exp(tab_2_coefs[, "Estimate"] - 1.96 * tab_2_coefs[, "Std. Error"]),
  or_upper = exp(tab_2_coefs[, "Estimate"] + 1.96 * tab_2_coefs[, "Std. Error"]),
  p_value  = tab_2_coefs[, "Pr(>|z|)"]
)

print(tab_2)

# 整理環境
rm(list = ls(pattern = "^tab_2_"))
gc()



# 分層分析 ----

# 自訂分析function
tab_3_fn <- function(indt){
  # 測試用資料
  # indt <- masterfile[65 <= age]
  
  # 兩組人數及用藥比例
  tab_ratio <- indt[, .(totN = .N, totDRUG = sum(drug_use), pctDRUG = round(mean(drug_use) * 100, 2)), by = .(bleeding)][order(bleeding)]
  
  # logistic model
  tab_model <- glm(bleeding ~ drug_use + age + male, data = indt, family = binomial("logit"))
  tab_coefs <- summary(tab_model)$coefficients
  tab_names <- rownames(tab_coefs)
  
  # 摘錄報表
  tab_paras <- data.table(
    vars = tab_names,
    or_point = exp(tab_coefs[, "Estimate"]),
    or_lower = exp(tab_coefs[, "Estimate"] - 1.96 * tab_coefs[, "Std. Error"]),
    or_upper = exp(tab_coefs[, "Estimate"] + 1.96 * tab_coefs[, "Std. Error"]),
    p_value  = tab_coefs[, "Pr(>|z|)"]
  )
  
  tab_paras <- tab_paras[vars == "drug_use"]
  tab_paras <- tab_paras[, .(bleeding = 1, or_point = round(or_point, 2), or_lower = round(or_lower, 2), or_upper = round(or_upper, 2), p_value = round(p_value, 4))]
  
  # 報表合併
  tab_merge <- merge.data.table(tab_ratio, tab_paras, by = c("bleeding"), all.x = T)
  tab_merge <- tab_merge[order(bleeding)]
  
  # 資料輸出
  return(tab_merge)
}

# 將分層分析所需要的資料以清單的方式疊在一起
tab_3_list <- list(
  masterfile[male == 1],
  masterfile[male == 0],
  masterfile[age < 65],
  masterfile[age >= 65]
)

# 將分層分析結果以清單的方式疊在一起進行批次清單處理
tab_3 <- lapply(tab_3_list, tab_3_fn)

# 將分析結果整理成為清單當中的data.table並使用rbindlist進行堆疊
tab_3 <- rbindlist(tab_3, use.names = T, fill = T)

print(tab_3)

# 整理環境
rm(list = ls(pattern = "^tab_3_"))
gc()



# 資料儲存 ----

# 指定儲存位置
setwd(path_temp)

# 寫出分析主檔
fwrite(masterfile, "masterfile.csv")

# 寫出table 3
fwrite(tab_3, "table_3.csv")




### END ###
# ~ 專案設定 ----

# 讀取原始資料路徑資料路徑 data-management-nhird/base
path_base <- "D:/laboratory/project/HDMRP-2024/data-management-nhird/base"

# 寫出整理完成之後資料路徑 data-management-nhird/temp
path_temp <- "D:/laboratory/project/HDMRP-2024/data-management-nhird/temp"

# 載入套件 data.table
library(data.table)



### 資料管理 ----



# ~ 高齡者 ----

# 讀取健保承保檔
setwd(path_base)
dt_pop_1 <- fread("h_nhi_enrol2014.csv")

# 使用head函數確認資料讀取樣貌
head(dt_pop_1)

# 保留身分證號(id), 性別(id_s), 出生年(id_birth_y)三個欄位
dt_pop_2 <- dt_pop_1
dt_pop_2 <- dt_pop_2[, .(id, id_s, id_birth_y)]
head(dt_pop_2)
nrow(dt_pop_2)

# 將上面的檔案進行去重複
dt_pop_2 <- unique(dt_pop_2)
head(dt_pop_2)
nrow(dt_pop_2)

# 確認性別欄位編碼一定要是1或2
dt_pop_3 <- dt_pop_2
table(dt_pop_3$id_s)
dt_pop_3 <- dt_pop_3[id_s %in% c(1, 2)]
table(dt_pop_3$id_s)
nrow(dt_pop_3)

# 確認年齡(以2014年計算)是否有異常值
dt_pop_3 <- dt_pop_3[, `:=`(age = 2014 - id_birth_y)]
summary(dt_pop_3$age)

# 只保留65歲(含)以上的高齡族群
dt_pop_3 <- dt_pop_3[65 <= age]
summary(dt_pop_3$age)
nrow(dt_pop_3)

# 整理物件
dt_pop <- dt_pop_3

# 刪除不需要的物件並使用gc()釋放記憶體空間
rm(list = ls(pattern = "^dt_pop_"))
gc()



# ~ 糖尿病診斷 ----

# 讀取門診費用檔所有變項以文字型態進行讀取(使用colClasses參數)
setwd(path_base)
dt_dm_1 <- fread("h_nhi_opdte2014.csv", colClasses = "character")

# 使用head函數確認資料讀取樣貌
head(dt_dm_1)

# 保留身分證號(id), 就醫日期(func_date), 三個診斷代碼(icd9cm_1~3)欄位
dt_dm_2 <- dt_dm_1
dt_dm_2 <- dt_dm_2[, .(id, func_date, icd9cm_1, icd9cm_2, icd9cm_3)]
head(dt_dm_2)

# 篩選具有糖尿病診斷的就醫紀錄
dt_dm_2 <- dt_dm_2[grepl("^250", icd9cm_1) | grepl("^250", icd9cm_2) | grepl("^250", icd9cm_3)]
nrow(dt_dm_2)
head(dt_dm_2)

# 保留身分證號(id), 就醫日期(func_date)欄位，新增一個計次欄位(dm_count = 1)，並進行去重複
dt_dm_2 <- dt_dm_2[, .(id, func_date, dm_count = 1)]
head(dt_dm_2)
nrow(dt_dm_2)
dt_dm_2 <- unique(dt_dm_2)
nrow(dt_dm_2)

# 計算每人總共有幾次具有糖尿病診斷的就診並命名為dm_freq
dt_dm_3 <- dt_dm_2[, .(dm_freq = sum(dm_count)), by = .(id)]
head(dt_dm_3)
nrow(dt_dm_3)

# 使用summary函數描述診斷次數的分布
summary(dt_dm_3$dm_freq)

# 只保留至少2次門診就醫紀錄的樣本
dt_dm_3 <- dt_dm_3[2 <= dm_freq]
nrow(dt_dm_3)
summary(dt_dm_3$dm_freq)

# 僅保留DM病人的身分證號(id)
dt_dm_4 <- dt_dm_3[, .(id)]
head(dt_dm_4)

# 加上一個欄位dm_diagnosis = 1
dt_dm_4 <- dt_dm_4[, `:=`(dm_diagnosis = 1)]
head(dt_dm_4)

# 整理物件
dt_dm <- dt_dm_4

# 刪除不需要的物件並使用gc()釋放記憶體空間
rm(list = ls(pattern = "^dt_dm_"))
gc()



# ~ 合併族群總檔 ----

# 以65歲以上的高齡族群為主(all.x = T)合併糖尿病診斷檔
nrow(dt_pop)
dt_merge <- merge.data.table(dt_pop, dt_dm, by = c("id"), all.x = T)
nrow(dt_merge)

# 使用head和summary確認現在資料的型態
head(dt_merge)
summary(dt_merge)

# 若DM就醫紀錄是NA的則把內容修改為0(補0 = 沒有糖尿就醫紀錄)
dt_merge <- dt_merge[is.na(dm_diagnosis), `:=`(dm_diagnosis  = 0)]

# 使用head和summary確認修改後資料的型態
head(dt_merge)
summary(dt_merge)



### 統計分析 ----

# 高齡者總人數
nrow(dt_merge)

# DM人數次數分析
table(dt_merge$dm_diagnosis)

# DM盛行率
mean(dt_merge$dm_diagnosis)

# 將整理好的資料寫到path_temp裡面儲存
setwd(path_temp)
fwrite(dt_merge, "elder_dm.csv")





### END ###
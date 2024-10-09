### 專案設定 ----

# 載入套件
library(data.table)
library(lubridate)

# 路徑設定
path_base <- "D:/laboratory/project/HDMRP-2024/data-management-basic/base"
path_temp <- "D:/laboratory/project/HDMRP-2024/data-management-basic/temp"

# 命名系統
# dt_x = raw data of x department
# ot_m_n = n output result in m section





### 資料管理 ----



# ~ 資料讀取：從硬碟讀取資料 ----

# 指定資料處理的硬碟路徑
setwd(path_base)

# 讀取門診檔(data_opd.csv)成為物件dt_opd
# 讀取住院檔(data_ipd.csv)成為物件dt_ipd
# fread是data.table套件用來讀取csv檔案的函數
dt_opd <- fread("data_opd.csv", colClasses = "character")
dt_ipd <- fread("data_ipd.csv", colClasses = "character")



# ~ 資料型態：確認大致樣貌及描述性統計 ----

# head 印出資料的前6列(預設)
# summary 摘要統計
# nrow 資料集觀察值筆數
# ncol 資料集欄位數量
# dim 資料集列數和欄數

# 門診資料
head(dt_opd)
summary(dt_opd)
nrow(dt_opd)
ncol(dt_opd)
dim(dt_opd)

# 住院資料
head(dt_ipd)
summary(dt_ipd)
nrow(dt_ipd)
ncol(dt_ipd)
dim(dt_ipd)



# ~ 資料型態：文字轉日期或數值 ----

# class 確認資料類型
# ymd 將資料轉日期格式
# as.numeric 將資料轉數值格式
# dt$v1 代表dt資料集裡面的v1變項

# 門診日期,費用轉換類型
class(dt_opd$opd_date)
class(dt_opd$fee)

dt_opd$opd_date <- ymd(dt_opd$opd_date)
dt_opd$fee <- as.numeric(dt_opd$fee)

class(dt_opd$opd_date)
class(dt_opd$fee)

head(dt_opd)
summary(dt_opd)

# A醫院住院日期,費用,天數轉換類型
dt_ipd$ipd_date <- ymd(dt_ipd$ipd_date)
dt_ipd$fee <- as.numeric(dt_ipd$fee)
dt_ipd$bed_day <- as.numeric(dt_ipd$bed_day)

head(dt_ipd)
summary(dt_ipd)



# ~ 資料篩選：取得想要的觀察值《遺漏值》 ----

# 文字的空值是 ""
# 數值的空值是 NA

# 找出文字欄位(icd_code)的空值
nrow(dt_opd[icd_code == ""])
head(dt_opd[icd_code == ""])

# 找出數值欄位(fee)的空值
nrow(dt_opd[is.na(fee)])
head(dt_opd[is.na(fee)])



# ~ 資料篩選：取得想要的觀察值《指定條件》 ----

# 對科別(func_name)進行次數分析
table(dt_opd$func_name)

# 比較運算子(operator)
# == 是將左右兩者進行全等比較
# %in% 是比對左邊物件是否在右邊的清單當中
# a != b 或 !(a == b)都表示a不等於b
# !(a %in% b)表示a不屬於b清單當中
# 文字內容都要被雙引號包住

# func_name變項的內容為內科
ot_select_func_name_1 <- dt_opd[func_name == "內科"]

head(ot_select_func_name_1)
table(ot_select_func_name_1$func_name)

# func_name變項的內容為心臟血管內科或是心臟血管外科
ot_select_func_name_2 <- dt_opd[func_name %in% c("心臟血管內科", "心臟血管外科")]

head(ot_select_func_name_2)
table(ot_select_func_name_2$func_name)

# func_name變項的內容"不為"復健科
ot_select_func_name_3 <- dt_opd[!(func_name == "復健科")]
ot_select_func_name_4 <- dt_opd[func_name != "復健科"]

table(ot_select_func_name_3$func_name)
table(ot_select_func_name_4$func_name)

# func_name變項的內容"不為"中醫科或是牙科
ot_select_func_name_5 <- dt_opd[!(func_name %in% c("中醫科", "牙科"))]

table(ot_select_func_name_5$func_name)



# ~ 資料篩選：取得想要的觀察值《比大小》 ----

# a > b               a大於b
# a < b               a小於b
# a >= b              a大於等於b
# a <= b              a小於等於b
# (a <= b) & (b <= c) a小於等於b 而且(AND)  b小於等於c
# (a <= b) & (c == d) a小於等於b 而且(AND)) c等於d
# (a <= b) | (c == d) a小於等於b 或者(OR) c等於d
# 多個邏輯條件連接時請善用小括號()

# 對單一欄位(向量)進行描述性統計分析
summary(dt_opd$fee)

# fee變項的內容為大於等於100,000
ot_select_fee_100k <- dt_opd[100000 <= fee]
head(ot_select_fee_100k)
summary(ot_select_fee_100k$fee)

# fee介於50,000到100,000之間的觀察值, "&" = AND 代表條件交集, "|" = OR 代表條件聯集
ot_select_fee_range_1 <- dt_opd[(50000 <= fee) & (fee <= 100000)]
summary(ot_select_fee_range_1$fee)

# fee介於50,000到100,000之間的觀察值
ot_select_fee_range_2 <- dt_opd[inrange(x = fee, lower = 50000, upper = 100000)]
summary(ot_select_fee_range_2$fee)
table(ot_select_fee_range_2$func_name)

# fee介於50,000到100,000之間而且是放射腫瘤科觀察值
ot_select_fee_range_3 <- dt_opd[inrange(x = fee, lower = 50000, upper = 100000) & func_name == "放射腫瘤科"]
summary(ot_select_fee_range_3$fee)
table(ot_select_fee_range_3$func_name)

# fee介於50,000到100,000之間或是放射腫瘤科觀察值
ot_select_fee_range_4 <- dt_opd[inrange(x = fee, lower = 50000, upper = 100000) | func_name == "放射腫瘤科"]
summary(ot_select_fee_range_4$fee)
table(ot_select_fee_range_4$func_name)



# ~ 資料篩選：取得想要的觀察值《字串模式》 ----

table(dt_opd$func_name)

# grep
# 用於判斷哪些位置(1, 2, 3, ... n)的觀察值符合指定文字條件
# 回傳的值為位置序號向量(1, 2, 62, 359 ...)
# 只有符合的才會回傳

# 找出門診檔func_name字串裡面含有"內科"的觀察值回傳位置序號向量
ot_select_string_1_vector <- grep(pattern = "內科", x = dt_opd$func_name)
head(ot_select_string_1_vector)

# 找出門診檔func_name字串裡面含有"內科"的觀察值
head(dt_opd)
ot_select_string_1 <- dt_opd[grep(pattern = "內科", x = func_name)]
head(ot_select_string_1)
table(ot_select_string_1$func_name)

# 找出門診檔func_name字串裡面含有"內科"的觀察值回傳原始內容向量
ot_select_string_1_value <- grep(pattern = "內科", x = dt_opd$func_name, value = T)
head(ot_select_string_1_value)
table(ot_select_string_1_value)

# grepl
# 用於判斷哪些位置(1, 2, 3, ... n)的觀察值符合指定文字條件
# 回傳的值為邏輯向量(T, T, F, T, ... T)
# 所有判斷邏輯值都會回傳

# 找出門診檔func_name字串裡面含有"腫瘤"的觀察值回傳位置序號向量
ot_select_string_2_vector <- grepl(pattern = "腫瘤", x = dt_opd$func_name)
head(ot_select_string_2_vector)
table(ot_select_string_2_vector)

# 找出門診檔func_name字串裡面含有"腫瘤"的觀察值
head(dt_opd)
ot_select_string_2 <- dt_opd[grepl(pattern = "腫瘤", x = func_name)]
head(ot_select_string_2)
table(ot_select_string_2$func_name)

# regexp
# regular expression 正規表達式

# grepl + regexp 基本型態 ^ 代表"開頭"
# 找出門診檔func_name字串裡面"開頭"為心臟的觀察值
ot_select_string_3 <- dt_opd[grepl(pattern = "^心臟", x = func_name)]
head(ot_select_string_3)
table(ot_select_string_3$func_name)

# grepl + regexp 基本型態 $ 代表"結尾"
# 找出門診檔func_name字串裡面"結尾"為外科的觀察值
ot_select_string_4 <- dt_opd[grepl(pattern = "外科$", x = func_name)]
head(ot_select_string_4)
table(ot_select_string_4$func_name)

# grepl + regexp 變化型態
# 找出icd_code字串裡面為433開頭或是434開頭的觀察值
ot_select_string_5 <- dt_opd[grepl(pattern = "^433|^434", x = icd_code)]
head(ot_select_string_5)
table(ot_select_string_5$icd_code)

# 找出icd_code字串裡面為43開頭，後面接著3或4的觀察值
ot_select_string_6 <- dt_opd[grepl(pattern = "^43[34]", x = icd_code)]
head(ot_select_string_6)
table(ot_select_string_6$icd_code)

# 找出icd_code字串裡面為43開頭，後面接著0 ~ 8的觀察值
ot_select_string_7 <- dt_opd[grepl(pattern = "^43[0-8]", icd_code)]
head(ot_select_string_7)
table(ot_select_string_7$icd_code)



# ~ 資料篩選：取得想要的變項 ----

# i索引處空白，寫個逗號分隔i和j，在j索引處寫一個句點，一對小括號，裡面寫入想要留下的變項名稱
# 從dt_opd中留下三個變項id, opd_date, hosp_id
ot_variable_select <- dt_opd[, .(id, opd_date, hosp_id)]
head(ot_variable_select)



# ~ 資料排序：讓資料成為想要的順序 ----

ot_sort <- dt_opd
head(ot_sort)

# order 依照指定的變項將資料排序

# 依照就醫日期排序，預設為遞增（ascending）排序
ot_sort <- ot_sort[order(opd_date)]
head(ot_sort)

# 依照多個排序欄位(病患編號,就醫醫院,就醫日期)之間以逗點隔開
ot_sort <- ot_sort[order(id, hosp_id, opd_date)]
head(ot_sort)

# 若要改為遞減（descending）排序，在變項前面加上負號（-）
ot_sort <- ot_sort[order(id, hosp_id, -fee)]
head(ot_sort)



# ~ 資料排序：依據排序將觀察值選出 ----

# SD = sub-data within BY variable
# 先依照病人身分證號及就醫日期進行排序
# 再取出每人在門診就醫的第N筆

# 第1筆
ot_obs_order_1 <- dt_opd
ot_obs_order_1 <- ot_obs_order_1[order(id, opd_date)]
ot_obs_order_1 <- ot_obs_order_1[, .SD[1], by = .(id)]
head(ot_obs_order_1)

# 第k筆，例如說K = 3
ot_obs_order_3 <- dt_opd
ot_obs_order_3 <- ot_obs_order_3[order(id, opd_date)]
ot_obs_order_3 <- ot_obs_order_3[, .SD[3], by = .(id)]
head(ot_obs_order_3)

# 第m:n筆
ot_obs_order_m <- dt_opd
ot_obs_order_m <- ot_obs_order_m[order(id, opd_date)]
ot_obs_order_m <- ot_obs_order_m[, .SD[1:3], by = .(id)]
head(ot_obs_order_m)

# 第N筆（最後一筆）
ot_obs_order_n <- dt_opd
ot_obs_order_n <- ot_obs_order_n[order(id, opd_date)]
ot_obs_order_n <- ot_obs_order_n[, .SD[.N], by = .(id)]
head(ot_obs_order_n)



# ~ 資料修改：產生變數 ----

# 想要找出超長住院天數的觀察值(long length-of-stay, long LOS)
# 新增一個變項名為long_los，內容為數值9
ot_variable_generate <- dt_ipd
ot_variable_generate$long_los <- 9
head(ot_variable_generate)
table(ot_variable_generate$long_los)



# ~ 資料修改：改變變數 ----

# 把既有的變項內容修改（更新）為0
ot_variable_generate <- ot_variable_generate[, `:=`(long_los = 0)]
head(ot_variable_generate)
table(ot_variable_generate$long_los)



# ~ 資料修改：有條件改變變數 ----

# 如果是住院天數大於90天，判斷為超長住院天數(long LOS = 1)
ot_variable_generate <- ot_variable_generate[90 <= bed_day, `:=`(long_los = 1)]
head(ot_variable_generate)
table(ot_variable_generate$long_los)

head(ot_variable_generate[long_los == 1])



# ~ 資料修改：計算變數 ----

# 把總費用扣掉掛號費500元
ot_variable_caculate <- ot_variable_generate[, .(id, fee, fee_appl = fee - 500)]
head(ot_variable_caculate)

# 其他加減乘除相同操作概念



# ~ 資料修改：去除重複 ----

# unique 依據所有資料內的變項進行比對和去重複

# 依照data.table內所有的變數進行比對，刪除重複的觀察值
ot_unique_1 <- unique(dt_ipd[, .(id, func_name)])
ot_unique_2 <- unique(dt_ipd[, .(id)])

head(ot_unique_1)
head(ot_unique_2)

nrow(ot_unique_1)
nrow(ot_unique_2)



# ~ 資料歸戶：依據變項內容相同者歸納資訊 ----

# 計算每人平均看診費用

# 將分群統計資料存為新的變項，觀察值總數變少(aggregate to a summary dataset)
ot_groupby_aggregate <- dt_ipd[, .(fee_m = mean(fee)), by = .(id)]
head(ot_groupby_aggregate)
nrow(ot_groupby_aggregate)
nrow(dt_ipd)



# ~ 資料歸戶：依據變項內容相同者統計資訊 ----

# 將分群統計資料存為新的變項，觀察值總數不變(re-merge to original dataset)
ot_groupby_remerge <- dt_ipd[, `:=`(fee_m = mean(fee)), by = .(id)]
head(ot_groupby_remerge)
nrow(ot_groupby_remerge)
nrow(dt_ipd)



# ~ 資料合併：依據變項內容相同者串聯不同資料集 ----

# 將門診和住院的變項(病人身分證號、醫院ID、醫師ID、就醫日期)留下
# 想要看同一醫病之間(id, hosp_id, prsn_id)在住院前後的門診就醫紀錄

ot_merge_x <- dt_ipd[, .(id, hosp_id, prsn_id, ipd_date)]
ot_merge_y <- dt_opd[, .(id, hosp_id, prsn_id, opd_date, opd_fee = fee)]

head(ot_merge_x)
head(ot_merge_y)

ot_merge <- merge.data.table(x = ot_merge_x, y = ot_merge_y, by = c("id", "hosp_id", "prsn_id"))
ot_merge <- ot_merge[order(id, hosp_id, prsn_id, ipd_date, opd_date)]
head(ot_merge, 20)



# ~ 資料合併：依據變項名稱相同者堆疊不同資料集 ----

# 將門診和住院的變項(病人身分證號、就醫日期、醫師ID、診斷代碼、就醫日期)留下
ot_bind_1 <- dt_ipd[, .(id, func_name, prsn_id, icd_code, visit_date = ipd_date, dep = "ipd")]
ot_bind_2 <- dt_opd[, .(id, func_name, prsn_id, icd_code, visit_date = opd_date, dep = "opd")]

head(ot_bind_1)
head(ot_bind_2)

ot_bind <- rbind(ot_bind_1, ot_bind_2)
ot_bind <- ot_bind[order(id, func_name, prsn_id, icd_code, visit_date)]
head(ot_bind, 20)
nrow(ot_bind) == nrow(ot_bind_1) + nrow(ot_bind_2)



# ~ 資料寫出：儲存資料到硬碟 ----

# 指定寫出路徑
setwd(path_temp)

# 將ot_merge寫出至硬碟儲存為檔案data_out.csv
fwrite(ot_merge, "data_out.csv")



# ~ 工作環境整理 ----

# 刪除指定名稱的物件
rm(ot_bind)

# 用ls()函數找出環境中名稱符合指定模式的物件，刪除
rm(list = ls(pattern = "^ot_bind"))

# 用ls()函數找出環境中全部的物件名稱，刪除
rm(list = ls())

# 記憶體空間釋放
gc()





### END ###
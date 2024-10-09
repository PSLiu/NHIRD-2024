/* 專案設定 */

/* 路徑建立 */
libname base "D:/laboratory/project/HDMRP-2024/data-management-technical-programming/sas/base";
libname temp "D:/laboratory/project/HDMRP-2024/data-management-technical-programming/sas/temp";



/* 中風住院合併內出血 */

/* 讀取住院費用檔 */
data stroke_1;
  set base.h_nhi_ipdte2014;

  keep id in_date icd9cm_1 icd9cm_2 icd9cm_3 icd9cm_4 icd9cm_5;
run;

proc print data = stroke_1(obs = 10);
run;

/* 選取必要欄位 */
data stroke_1;
  set stroke_1(rename = (in_date = in_date_old));

  in_date = input(in_date_old, yymmdd8.);
  format in_date yymmdd10.;
  drop in_date_old;
run;

proc print data = stroke_1(obs = 10);
run;

/* 撈出主診斷欄位為缺血性中風患者 */
data stroke_2;
  set stroke_1;

  if prxmatch('/^43[34]/', icd9cm_1);
run;

proc print data = stroke_2(obs = 10);
run;

/* 依據樣本ID及住院日期排序後，選取每個樣本今年的首次住院 */
proc sort data = stroke_2;
  by id in_date;
run;

data stroke_3;
  set stroke_2;

  by id;
    if first.id then output;
run;

/* 標記其他診斷具有出血性疾病診斷碼 */
/* ^4230|^43[0-2]|^455[258]|^456[02]|^4590|^5307|^53082|^531[0–6]|^532[0-6]|^533[0-6]|^534[0–6]|^535[0-6]|^53783|^5620[23]|^5621[23]|^56881|^5693|^56985|^578[019]|^56881|^59381 |^5997|^6238|^62632|^6266|^7191|^784[78]|^7863|^852|^853 */

data stroke_4;
  set stroke_3;

  bleeding = 0;
  if    prxmatch('/^4230|^43[0-2]|^455[258]|^456[02]|^4590|^5307|^53082|^531[0–6]|^532[0-6]|^533[0-6]|^534[0–6]|^535[0-6]|^53783|^5620[23]|^5621[23]|^56881|^5693|^56985|^578[019]|^56881|^59381 |^5997|^6238|^62632|^6266|^7191|^784[78]|^7863|^852|^853/', icd9cm_2)
     or prxmatch('/^4230|^43[0-2]|^455[258]|^456[02]|^4590|^5307|^53082|^531[0–6]|^532[0-6]|^533[0-6]|^534[0–6]|^535[0-6]|^53783|^5620[23]|^5621[23]|^56881|^5693|^56985|^578[019]|^56881|^59381 |^5997|^6238|^62632|^6266|^7191|^784[78]|^7863|^852|^853/', icd9cm_3)
     or prxmatch('/^4230|^43[0-2]|^455[258]|^456[02]|^4590|^5307|^53082|^531[0–6]|^532[0-6]|^533[0-6]|^534[0–6]|^535[0-6]|^53783|^5620[23]|^5621[23]|^56881|^5693|^56985|^578[019]|^56881|^59381 |^5997|^6238|^62632|^6266|^7191|^784[78]|^7863|^852|^853/', icd9cm_4)
     or prxmatch('/^4230|^43[0-2]|^455[258]|^456[02]|^4590|^5307|^53082|^531[0–6]|^532[0-6]|^533[0-6]|^534[0–6]|^535[0-6]|^53783|^5620[23]|^5621[23]|^56881|^5693|^56985|^578[019]|^56881|^59381 |^5997|^6238|^62632|^6266|^7191|^784[78]|^7863|^852|^853/', icd9cm_5) 
  then bleeding = 1;
run;

proc print data = stroke_4(obs = 10);
run;

proc freq data = stroke_4;
  table bleeding;
run;

/* 整理環境 */
data stroke;
  set stroke_4;
run;

proc datasets library = work nodetails nolist;
   delete stroke_:;
quit;





/* 研究族群基本資料 */

/* 建立處理函數 */
%macro idfile_1_fm(yy, mm);

/* 測試用指標 */
/*
%let yy = 2014;
%let mm = 1;
*/

/* 將1-9月的月份前面補0 */
%let mm = %sysfunc(putn(&mm., z2.));

/* 讀取資料 */
data otdt_1;
  set base.h_nhi_enrol&yy.&mm.;
run;

/* 過濾資料 */
proc sort data = otdt_1;
  by id;
data otdt_1;
  merge
    stroke(keep = id in = x)
    otdt_1(in = y);
  by id;

  if x = 1 & y = 1;
run;

/* 保留欄位 */
data idfile_1_&yy.&mm.;
  set otdt_1(rename = (id_birth_y = id_birth_y_old));

  id_birth_y = input(id_birth_y_old, 4.);
  keep id id_birth_y id_s;
run;

%mend;

/* 設定迴圈範圍 */
%macro idfile_1_fn(ys, ye, ms, me);
%do yy = &ys. %to &ye. %by 1;
  %do mm = &ms. %to &me. %by 1;
    %idfile_1_fm(&yy., &mm.);
  %end;
%end;
%mend;

/* 執行迴圈函數 */
%idfile_1_fn(2014, 2014, 1, 12);

proc datasets library = work nodetails nolist;
   delete otdt_1;
quit;

/* 將清單資料堆疊後去重複 */
data idfile_2;
  set idfile_1_:;
run;

proc sort data = idfile_2 nodupkey;
  by id id_birth_y id_s;
run;

proc print data = idfile_2(obs = 10);
run;

/* 計算年齡，若有年齡小於20歲(不含)或大於100歲者(不含)者則刪除 */
data idfile_3;
  set idfile_2;

  age = 2014 - id_birth_y;
run;

proc means data = idfile_3;
  var age;
run;

/* 確認性別，若有性別編碼非男女者則刪除 */
proc freq data = idfile_3;
  table id_s;
run;

data idfile_4;
  set idfile_3;

  if id_s in ("1", "2");

  male = 0;
  if id_s = "1" then male = 1;
run;

proc freq data = idfile_4;
  table id_s;
run;

proc print data = idfile_4(obs = 10);
run;

/* 整理環境 */
data idfile;
  set idfile_4;
run;

proc datasets library = work nodetails nolist;
   delete idfile_:;
quit;



/* 中風住院前藥物病史 */

/* 目標藥物清單 */
proc import
  datafile = "D:/laboratory/project/HDMRP-2024/data-management-technical-programming/sas/temp/druglist.csv"
  dbms = csv out = drug_1_list replace;
run;

proc sort data = drug_1_list;
  by drug_no;
run;

proc freq data = drug_1_list;
  table drug_type;
run;

/* 建立處理函數 */
%macro drug_1_fm(yy, mm);

/* 測試用指標 */
/*
%let yy = 2014;
%let mm = 1;
*/

/* 將1-9月的月份前面補0 */
%let mm = %sysfunc(putn(&mm., z2.));

/* 讀取門診費用檔(opdte) */
data indt_1_a;
  set base.h_nhi_opdte&yy.&mm._10(rename = (func_date = func_date_old));

  func_date = input(func_date_old, yymmdd8.);
  keep id func_date hosp_id fee_ym appl_date appl_type case_type seq_no;
  format func_date yymmdd10.;
run;

/* 篩選研究樣本在住院日之前的資料 */
proc sort data = indt_1_a;
  by id;
data indt_1_a;
  merge
    stroke(keep = id in_date in = x)
    indt_1_a(in = y);
  by id;

  if x = 1 & y = 1;
  if func_date < in_date;
run;

/* 讀取門診醫令檔(opdto) */
data indt_1_b;
  set base.h_nhi_opdto&yy.&mm._10;

  keep drug_no hosp_id fee_ym appl_date appl_type case_type seq_no;
run;

/* 篩選目標藥物資料 */
proc sort data = indt_1_b;
  by drug_no;
data indt_1_b;
  merge
    indt_1_b(in = x)
    drug_1_list(in = y);
  by drug_no;

  if x = 1 & y = 1;
run;

/* 合併門診費用檔 */
proc sort data = indt_1_a;
  by hosp_id fee_ym appl_date appl_type case_type seq_no;
proc sort data = indt_1_b;
  by hosp_id fee_ym appl_date appl_type case_type seq_no;
data indt_2;
  merge
    indt_1_a(in = x)
    indt_1_b(in = y);
  by hosp_id fee_ym appl_date appl_type case_type seq_no;

  if x = 1 & y = 1;
run;

data indt_2;
  set indt_2;

  drug_use = 1;
  keep id drug_use;
run;

proc sort data = indt_2 nodupkey out = drug_1_data_&yy.&mm.;
  by id drug_use;
run;

%mend;

/* 設定迴圈範圍 */
%macro drug_1_fn(ys, ye, ms, me);
%do yy = &ys. %to &ye. %by 1;
  %do mm = &ms. %to &me. %by 1;
    %drug_1_fm(&yy., &mm.);
  %end;
%end;
%mend;

/* 執行迴圈函數 */
%drug_1_fn(2014, 2014, 1, 12);

proc datasets library = work nodetails nolist;
   delete indt_:;
quit;

/* 資料堆疊 */
data drug_2;
  set drug_1_data_:;
run;

proc sort data = drug_2 nodupkey;
  by id drug_use;
run;

proc print data = drug_2(obs = 10);
run;

/* 整理環境 */
data drug;
  set drug_2;
run;

proc datasets library = work nodetails nolist;
   delete drug_:;
quit;



/* 資料合併 */
data masterfile_1;
  set stroke;
run;

proc sort data = masterfile_1;
  by id;
proc sort data = idfile;
  by id;
data masterfile_1;
  merge
    masterfile_1(in = x)
    idfile(in = y);
  by id;

  if x = 1;
run;

proc sort data = masterfile_1;
  by id;
proc sort data = drug;
  by id;
data masterfile_1;
  merge
    masterfile_1(in = x)
    drug(in = y);
  by id;

  if x = 1;
  if drug_use = . then drug_use = 0;
run;

proc print data = masterfile_1(obs = 10);
run;



/* 樣本篩選 */
data masterfile_2;
  set masterfile_1;
run;

proc means data = masterfile_2;
  var age male drug_use;
run;

/* 排除承保基本資料缺失的樣本 */
data masterfile_2;
  set masterfile_2;

  if age = . then delete;
run;

proc means data = masterfile_2;
  var age male drug_use;
run;

/* 排除今年4月(不含)以前的樣本(無法有足夠時間確認抗凝血藥物使用) */
data masterfile_2;
  set masterfile_2;

  if in_date < input("20140401", yymmdd8.) then delete;
run;

proc means data = masterfile_2;
  var age male drug_use;
run;

/* 整理環境 */
data masterfile;
  set masterfile_2;
run;

proc datasets library = work nodetails nolist;
   delete masterfile_:;
   delete stroke:;
   delete idfile:;
   delete drug:;
quit;



/* 背景特性表格 */
proc tabulate data = masterfile;
  var   age;
  class bleeding drug_use male;

  table (drug_use male), bleeding * (n colpctn);
  table (age), bleeding * (mean std);
run;



/* 統計分析 */

/* 兩組人數及用藥比例 */
proc sql;
  create table tab_2_ratio as
  select bleeding, count(drug_use) as totN, sum(drug_use) as totDRUG, round(mean(drug_use) * 100, 0.01) as pctDRUG
  from masterfile
  group by bleeding
  order by bleeding;
quit;

proc print data = tab_2_ratio;
run;

/* logistic model - 檢視輸出報表每一個項目的名稱 */
ods trace on;
proc logistic data = masterfile;
  class bleeding(ref = "0");
  model bleeding = drug_use age male;
run;
ods trace off;

/* logistic model - 輸出參數估計值另存為資料集進行處理 */
proc logistic data = masterfile;
  class bleeding(ref = "0");
  model bleeding = drug_use age male;
  ods output ParameterEstimates = tab_2_coefs;
run;

/* 確認報表物件的架構 */
proc print data = tab_2_coefs;
run;

/* 組合成為報表 */
data tab_2;
  set tab_2_coefs;

  vars = Variable;
  or_point = exp(Estimate);
  or_lower = exp(Estimate - 1.96 * StdErr);
  or_upper = exp(Estimate + 1.96 * StdErr);
  p_value  = ProbChiSq;

  keep vars or_point or_lower or_upper p_value;
run;

proc print data = tab_2;
run;



/* 分層分析子群資料 */
data tab_3_subdata_1; set masterfile; if male = 1;  run;
data tab_3_subdata_2; set masterfile; if male = 0;  run;
data tab_3_subdata_3; set masterfile; if age < 65;  run;
data tab_3_subdata_4; set masterfile; if age >= 65; run;

/* 分層分析 */
%macro tab_3_fm(dseq);

/* 測試用參數 */
/*
%let dseq = 1;
*/
data tab_3_subdata;
  set tab_3_subdata_&dseq;
run;

/* 兩組人數及用藥比例 */
proc sql;
  create table tab_ratio as
  select bleeding, count(drug_use) as totN, sum(drug_use) as totDRUG, round(mean(drug_use) * 100, 0.01) as pctDRUG
  from tab_3_subdata
  group by bleeding
  order by bleeding;
quit;

proc print data = tab_ratio;
run;

/* logistic model - 輸出參數估計值另存為資料集進行處理 */
proc logistic data = tab_3_subdata;
  class bleeding(ref = "0");
  model bleeding = drug_use age male;
  ods output ParameterEstimates = tab_coefs;
run;

/* 組合成為報表 */
data tab_coefs;
  set tab_coefs;

  if Variable = "drug_use";
  bleeding = 1;

  or_point = exp(Estimate);
  or_lower = exp(Estimate - 1.96 * StdErr);
  or_upper = exp(Estimate + 1.96 * StdErr);
  p_value  = ProbChiSq;

  keep bleeding or_point or_lower or_upper p_value;
run;

/* 組合成為結果 */
proc sort data = tab_ratio;
  by bleeding;
proc sort data = tab_coefs;
  by bleeding;
data tab_merge;
  merge
    tab_ratio(in = x)
	tab_coefs(in = y);
  by bleeding;
  if x = 1;
run;

/* 儲存結果 */
data tab_3_subfile_&dseq;
  set tab_merge;
run;

%mend;
%tab_3_fm(1);
%tab_3_fm(2);
%tab_3_fm(3);
%tab_3_fm(4);

/* 將分析結果進行堆疊 */
data tab_3;
  set tab_3_subfile_:;
run;

proc print data = tab_3;
run;



/* 資料儲存 */

/* 寫出分析主檔 */
data temp.masterfile;
  set masterfile;
run;

/* 寫出table 3 */
proc export 
  data = tab_3
  outfile = "D:/laboratory/project/HDMRP-2024/data-management-technical-programming/sas/temp/table_3.csv"
  dbms = csv replace;
run;





/* END */

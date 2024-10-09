/* �M�׳]�w */

/* ���|�إ� */
libname base "D:/laboratory/project/HDMRP-2024/data-management-technical-programming/sas/base";
libname temp "D:/laboratory/project/HDMRP-2024/data-management-technical-programming/sas/temp";



/* ������|�X�֤��X�� */

/* Ū����|�O���� */
data stroke_1;
  set base.h_nhi_ipdte2014;

  keep id in_date icd9cm_1 icd9cm_2 icd9cm_3 icd9cm_4 icd9cm_5;
run;

proc print data = stroke_1(obs = 10);
run;

/* ������n��� */
data stroke_1;
  set stroke_1(rename = (in_date = in_date_old));

  in_date = input(in_date_old, yymmdd8.);
  format in_date yymmdd10.;
  drop in_date_old;
run;

proc print data = stroke_1(obs = 10);
run;

/* ���X�D�E�_��쬰�ʦ�ʤ����w�� */
data stroke_2;
  set stroke_1;

  if prxmatch('/^43[34]/', icd9cm_1);
run;

proc print data = stroke_2(obs = 10);
run;

/* �̾ڼ˥�ID�Φ�|����Ƨǫ�A����C�Ӽ˥����~��������| */
proc sort data = stroke_2;
  by id in_date;
run;

data stroke_3;
  set stroke_2;

  by id;
    if first.id then output;
run;

/* �аO��L�E�_�㦳�X��ʯe�f�E�_�X */
/* ^4230|^43[0-2]|^455[258]|^456[02]|^4590|^5307|^53082|^531[0�V6]|^532[0-6]|^533[0-6]|^534[0�V6]|^535[0-6]|^53783|^5620[23]|^5621[23]|^56881|^5693|^56985|^578[019]|^56881|^59381 |^5997|^6238|^62632|^6266|^7191|^784[78]|^7863|^852|^853 */

data stroke_4;
  set stroke_3;

  bleeding = 0;
  if    prxmatch('/^4230|^43[0-2]|^455[258]|^456[02]|^4590|^5307|^53082|^531[0�V6]|^532[0-6]|^533[0-6]|^534[0�V6]|^535[0-6]|^53783|^5620[23]|^5621[23]|^56881|^5693|^56985|^578[019]|^56881|^59381 |^5997|^6238|^62632|^6266|^7191|^784[78]|^7863|^852|^853/', icd9cm_2)
     or prxmatch('/^4230|^43[0-2]|^455[258]|^456[02]|^4590|^5307|^53082|^531[0�V6]|^532[0-6]|^533[0-6]|^534[0�V6]|^535[0-6]|^53783|^5620[23]|^5621[23]|^56881|^5693|^56985|^578[019]|^56881|^59381 |^5997|^6238|^62632|^6266|^7191|^784[78]|^7863|^852|^853/', icd9cm_3)
     or prxmatch('/^4230|^43[0-2]|^455[258]|^456[02]|^4590|^5307|^53082|^531[0�V6]|^532[0-6]|^533[0-6]|^534[0�V6]|^535[0-6]|^53783|^5620[23]|^5621[23]|^56881|^5693|^56985|^578[019]|^56881|^59381 |^5997|^6238|^62632|^6266|^7191|^784[78]|^7863|^852|^853/', icd9cm_4)
     or prxmatch('/^4230|^43[0-2]|^455[258]|^456[02]|^4590|^5307|^53082|^531[0�V6]|^532[0-6]|^533[0-6]|^534[0�V6]|^535[0-6]|^53783|^5620[23]|^5621[23]|^56881|^5693|^56985|^578[019]|^56881|^59381 |^5997|^6238|^62632|^6266|^7191|^784[78]|^7863|^852|^853/', icd9cm_5) 
  then bleeding = 1;
run;

proc print data = stroke_4(obs = 10);
run;

proc freq data = stroke_4;
  table bleeding;
run;

/* ��z���� */
data stroke;
  set stroke_4;
run;

proc datasets library = work nodetails nolist;
   delete stroke_:;
quit;





/* ��s�ڸs�򥻸�� */

/* �إ߳B�z��� */
%macro idfile_1_fm(yy, mm);

/* ���եΫ��� */
/*
%let yy = 2014;
%let mm = 1;
*/

/* �N1-9�몺����e����0 */
%let mm = %sysfunc(putn(&mm., z2.));

/* Ū����� */
data otdt_1;
  set base.h_nhi_enrol&yy.&mm.;
run;

/* �L�o��� */
proc sort data = otdt_1;
  by id;
data otdt_1;
  merge
    stroke(keep = id in = x)
    otdt_1(in = y);
  by id;

  if x = 1 & y = 1;
run;

/* �O�d��� */
data idfile_1_&yy.&mm.;
  set otdt_1(rename = (id_birth_y = id_birth_y_old));

  id_birth_y = input(id_birth_y_old, 4.);
  keep id id_birth_y id_s;
run;

%mend;

/* �]�w�j��d�� */
%macro idfile_1_fn(ys, ye, ms, me);
%do yy = &ys. %to &ye. %by 1;
  %do mm = &ms. %to &me. %by 1;
    %idfile_1_fm(&yy., &mm.);
  %end;
%end;
%mend;

/* ����j���� */
%idfile_1_fn(2014, 2014, 1, 12);

proc datasets library = work nodetails nolist;
   delete otdt_1;
quit;

/* �N�M���ư��|��h���� */
data idfile_2;
  set idfile_1_:;
run;

proc sort data = idfile_2 nodupkey;
  by id id_birth_y id_s;
run;

proc print data = idfile_2(obs = 10);
run;

/* �p��~�֡A�Y���~�֤p��20��(���t)�Τj��100����(���t)�̫h�R�� */
data idfile_3;
  set idfile_2;

  age = 2014 - id_birth_y;
run;

proc means data = idfile_3;
  var age;
run;

/* �T�{�ʧO�A�Y���ʧO�s�X�D�k�k�̫h�R�� */
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

/* ��z���� */
data idfile;
  set idfile_4;
run;

proc datasets library = work nodetails nolist;
   delete idfile_:;
quit;



/* ������|�e�Ī��f�v */

/* �ؼ��Ī��M�� */
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

/* �إ߳B�z��� */
%macro drug_1_fm(yy, mm);

/* ���եΫ��� */
/*
%let yy = 2014;
%let mm = 1;
*/

/* �N1-9�몺����e����0 */
%let mm = %sysfunc(putn(&mm., z2.));

/* Ū�����E�O����(opdte) */
data indt_1_a;
  set base.h_nhi_opdte&yy.&mm._10(rename = (func_date = func_date_old));

  func_date = input(func_date_old, yymmdd8.);
  keep id func_date hosp_id fee_ym appl_date appl_type case_type seq_no;
  format func_date yymmdd10.;
run;

/* �z���s�˥��b��|�餧�e����� */
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

/* Ū�����E��O��(opdto) */
data indt_1_b;
  set base.h_nhi_opdto&yy.&mm._10;

  keep drug_no hosp_id fee_ym appl_date appl_type case_type seq_no;
run;

/* �z��ؼ��Ī���� */
proc sort data = indt_1_b;
  by drug_no;
data indt_1_b;
  merge
    indt_1_b(in = x)
    drug_1_list(in = y);
  by drug_no;

  if x = 1 & y = 1;
run;

/* �X�֪��E�O���� */
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

/* �]�w�j��d�� */
%macro drug_1_fn(ys, ye, ms, me);
%do yy = &ys. %to &ye. %by 1;
  %do mm = &ms. %to &me. %by 1;
    %drug_1_fm(&yy., &mm.);
  %end;
%end;
%mend;

/* ����j���� */
%drug_1_fn(2014, 2014, 1, 12);

proc datasets library = work nodetails nolist;
   delete indt_:;
quit;

/* ��ư��| */
data drug_2;
  set drug_1_data_:;
run;

proc sort data = drug_2 nodupkey;
  by id drug_use;
run;

proc print data = drug_2(obs = 10);
run;

/* ��z���� */
data drug;
  set drug_2;
run;

proc datasets library = work nodetails nolist;
   delete drug_:;
quit;



/* ��ƦX�� */
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



/* �˥��z�� */
data masterfile_2;
  set masterfile_1;
run;

proc means data = masterfile_2;
  var age male drug_use;
run;

/* �ư��ӫO�򥻸�Ưʥ����˥� */
data masterfile_2;
  set masterfile_2;

  if age = . then delete;
run;

proc means data = masterfile_2;
  var age male drug_use;
run;

/* �ư����~4��(���t)�H�e���˥�(�L�k�������ɶ��T�{�ܾ����Ī��ϥ�) */
data masterfile_2;
  set masterfile_2;

  if in_date < input("20140401", yymmdd8.) then delete;
run;

proc means data = masterfile_2;
  var age male drug_use;
run;

/* ��z���� */
data masterfile;
  set masterfile_2;
run;

proc datasets library = work nodetails nolist;
   delete masterfile_:;
   delete stroke:;
   delete idfile:;
   delete drug:;
quit;



/* �I���S�ʪ�� */
proc tabulate data = masterfile;
  var   age;
  class bleeding drug_use male;

  table (drug_use male), bleeding * (n colpctn);
  table (age), bleeding * (mean std);
run;



/* �έp���R */

/* ��դH�ƤΥ��Ĥ�� */
proc sql;
  create table tab_2_ratio as
  select bleeding, count(drug_use) as totN, sum(drug_use) as totDRUG, round(mean(drug_use) * 100, 0.01) as pctDRUG
  from masterfile
  group by bleeding
  order by bleeding;
quit;

proc print data = tab_2_ratio;
run;

/* logistic model - �˵���X����C�@�Ӷ��ت��W�� */
ods trace on;
proc logistic data = masterfile;
  class bleeding(ref = "0");
  model bleeding = drug_use age male;
run;
ods trace off;

/* logistic model - ��X�ѼƦ��p�ȥt�s����ƶ��i��B�z */
proc logistic data = masterfile;
  class bleeding(ref = "0");
  model bleeding = drug_use age male;
  ods output ParameterEstimates = tab_2_coefs;
run;

/* �T�{�����󪺬[�c */
proc print data = tab_2_coefs;
run;

/* �զX�������� */
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



/* ���h���R�l�s��� */
data tab_3_subdata_1; set masterfile; if male = 1;  run;
data tab_3_subdata_2; set masterfile; if male = 0;  run;
data tab_3_subdata_3; set masterfile; if age < 65;  run;
data tab_3_subdata_4; set masterfile; if age >= 65; run;

/* ���h���R */
%macro tab_3_fm(dseq);

/* ���եΰѼ� */
/*
%let dseq = 1;
*/
data tab_3_subdata;
  set tab_3_subdata_&dseq;
run;

/* ��դH�ƤΥ��Ĥ�� */
proc sql;
  create table tab_ratio as
  select bleeding, count(drug_use) as totN, sum(drug_use) as totDRUG, round(mean(drug_use) * 100, 0.01) as pctDRUG
  from tab_3_subdata
  group by bleeding
  order by bleeding;
quit;

proc print data = tab_ratio;
run;

/* logistic model - ��X�ѼƦ��p�ȥt�s����ƶ��i��B�z */
proc logistic data = tab_3_subdata;
  class bleeding(ref = "0");
  model bleeding = drug_use age male;
  ods output ParameterEstimates = tab_coefs;
run;

/* �զX�������� */
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

/* �զX�������G */
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

/* �x�s���G */
data tab_3_subfile_&dseq;
  set tab_merge;
run;

%mend;
%tab_3_fm(1);
%tab_3_fm(2);
%tab_3_fm(3);
%tab_3_fm(4);

/* �N���R���G�i����| */
data tab_3;
  set tab_3_subfile_:;
run;

proc print data = tab_3;
run;



/* ����x�s */

/* �g�X���R�D�� */
data temp.masterfile;
  set masterfile;
run;

/* �g�Xtable 3 */
proc export 
  data = tab_3
  outfile = "D:/laboratory/project/HDMRP-2024/data-management-technical-programming/sas/temp/table_3.csv"
  dbms = csv replace;
run;





/* END */

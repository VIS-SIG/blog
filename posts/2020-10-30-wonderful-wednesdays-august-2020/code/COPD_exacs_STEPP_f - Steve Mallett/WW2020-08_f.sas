libname outdata "H:\arwork\gsk1278863\mid209676\present_2020_01\code\COPD";

PROC IMPORT OUT= WORK.copd 
            DATAFILE= "C:\Users\sam31676\OneDrive - GSK\_Non Project\Data Visualisation\PSI data vis SIG\Wonderful Wednesdays\2020 08\ww08.txt" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

data copdx;
  set copd;
  fu_years=study_days_n/365.25;
  logfu=log(fu_years);
run;

* Fit overall model;

proc genmod data=copdx;
  class rand_trt prev_exac region;
  model anthonisen = rand_trt prev_exac region fev1_rv /dist=negbin link=log offset=logfu type3; 
  lsmeans rand_trt / diff exp cl om;
run;

* Main macro to create plot data;

%macro create(id=, var=, class=, covars=);
proc sort data=copdx;
  by &var;
run;

options nosymbolgen mprint;
%let r1=50;
%let r2=350;
%let n = 1221;

%macro split;
%global m;
%let i = 0;
%let low = 1;
%let high = %eval(&r2);

%do %until (&high > &n);
  %let i = %eval(&i+1);

  data copd&i;
    set copdx;
	if _n_ >= &low and _n_ <= &high;
    group = &i;
	run;

  %let low = %eval(&low+&r1);
  %let high = %eval(&low+&r2-1);
  %let m = &i;
  %end;

%mend split;
%split;

%macro combine;
data copd_all;
  set 
  %do i = 1 %to &m;
    copd&i
	%end;
	;
run;
%mend combine;
%combine;

proc means data=copd_all noprint;
  by group;
  var &var;
  output out=med(keep=group median) median(&var)=median;
run;

ods output diffs=diffs;

proc genmod data=copd_all;
  by group;
  class &class;
  model anthonisen = &covars/dist=negbin link=log offset=lnstudyday; 
  lsmeans rand_trt / diff exp cl om;
run;
quit;

data plot&id;
  merge diffs(keep=group ExpEstimate lowerexp upperexp) med;
  by group;
  var="&var";
run;
%mend create;
%create(id=1, var=fev1_rv, class=%str(rand_trt prev_exac region), covars=%str(rand_trt prev_exac region));
%create(id=2, var=age_n, class=%str(rand_trt prev_exac region), covars=%str(rand_trt prev_exac region));

data outdata.plot_ww2020_08;
  set plot1 plot2;
run;



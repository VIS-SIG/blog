libname out ".";

* Download macros from https://www.lshtm.ac.uk/research/centres-projects-groups/missing-data#dia-missing-data;

%inc "Part1A_34.sas";
%inc "Part1B_47.sas";
%inc "Part2A_40.sas";
%inc "Part2B_31.sas";


PROC IMPORT OUT= WORK.a 
            DATAFILE= "WWW_AUG2025.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="Sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

proc sort data=a;
  by usubjid avisit;
run;

* Treatment policy - ignore ICEs;
data tp(where=(avisitn>0));
  set a(rename=(aval=avalc));
  by usubjid;
  length aval avisitn 8;
  aval=avalc;
  if trt="Treatment A" then treat=1;
  else treat=2;
  retain base;
  if first.usubjid then do;
    avisitn=0;
	base=aval;
	end;
  else avisitn=compress(avisit, "MONTH");
  chg=aval-base;
  keep usubjid treat avisitn base aval chg ice;
run;

ods output lsmeans=lsm_tp;
proc mixed data=tp;
  class usubjid avisitn treat;
  model chg = treat avisitn base treat*avisitn base*avisitn/ ddfm=kr;
  repeated avisitn / subject=usubjid type=UN;
  lsmeans treat*avisitn;
run;

* Hypothetical set data after ICE to missing;
data hypo;
  set tp;
  if ice="Y" then chg=.;
run;

* Missing at Random;
%part1A(jobname=MI, data=hypo, subject=usubjid, response=chg, time=avisitn, treat=treat, id=base);
%part1B(jobname=MI, ndraws=10, thin=100, seed=230815);
%part2A(inname=MI, jobname=MAR, method=MAR);
%part2B(jobname=MAR, seed=230815);

ods output lsmeans=lsm_mar;
proc mixed data=mar_datafull;
  by draw;
  class usubjid avisitn treat;
  model chg = treat avisitn base treat*avisitn base*avisitn/ ddfm=kr;
  repeated avisitn / subject=usubjid type=UN;
  lsmeans treat*avisitn;
run;

proc sort data=lsm_mar;
  by avisitn treat draw;
run;

ods output ParameterEstimates=lsm_mar2;
proc mianalyze parms=lsm_mar(rename=(draw=_imputation_));
  by avisitn treat;
  modeleffects avisitn*treat;
run;


* Jump to reference;
%part2A(inname=MI, jobname=J2R, method=J2R, ref=2);
%part2B(jobname=J2R, seed=230815);

ods output lsmeans=lsm_j2r;
proc mixed data=j2r_datafull;
  by draw;
  class usubjid avisitn treat;
  model chg = treat avisitn base treat*avisitn base*avisitn/ ddfm=kr;
  repeated avisitn / subject=usubjid type=UN;
  lsmeans treat*avisitn;
run;

proc sort data=lsm_j2r;
  by avisitn treat draw;
run;

ods output ParameterEstimates=lsm_j2r2;
proc mianalyze parms=lsm_j2r(rename=(draw=_imputation_));
  by avisitn treat;
  modeleffects avisitn*treat;
run;

* Copy increment to reference;
%part2A(inname=MI, jobname=CIR, method=CIR, ref=2);
%part2B(jobname=CIR, seed=230815);

ods output lsmeans=lsm_cir;
proc mixed data=cir_datafull;
  by draw;
  class usubjid avisitn treat;
  model chg = treat avisitn base treat*avisitn base*avisitn/ ddfm=kr;
  repeated avisitn / subject=usubjid type=UN;
  lsmeans treat*avisitn;
run;

proc sort data=lsm_cir;
  by avisitn treat draw;
run;

ods output ParameterEstimates=lsm_cir2;
proc mianalyze parms=lsm_cir(rename=(draw=_imputation_));
  by avisitn treat;
  modeleffects avisitn*treat;
run;

data out.lsm_all;
  set lsm_tp(in=a) lsm_mar2 (in=b) lsm_cir2(in=c) lsm_j2r2(in=d);
  select;
    when(a) method=1;
	when(b) method=2;
	when(c) method=3;
	when(d) method=4;
	otherwise;
	end;
  keep method treat avisitn estimate;
run;





filename urlFile url 'https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2020/2020-04-08/2020-04-08-psi-vissig-adtte.csv';
*--------------------------------
Import Procedure
---------------------------------;


data ADTTE 
	(label='Time-to-Event'
	 compress=yes
);

infile urlFile 
  delimiter = ',' 
  missover 
  dsd 
  lrecl=32767 
  firstobs=2;
attrib 
	STUDYID length=$20 format=$20. informat=$20. label='the study identifier'
	SUBJID length=8 format=8. informat=8. label='subject identifier'
	USUBJID length=$40 format=$40. informat=$40. label='unique subject iddentifier'
	AGE length=8 format=8. informat=8. label='age at randomisation (years)'
	STR01 length=$8 format=$8. informat=8. label='Hormone receptor status at randomisation'
	STR01N length=3 format=3. informat=3. label='Hormone receptor positive (Numeric)'
	STR01L length=$30 format=$30. informat=$30. label='Hormone receptor positive (Long format)'
	STR02 length=$30 format=$30. informat=$30. label='Prior Radiotherapy at randomisation'
	STR02N length=3 format=3. informat=3. label='Prior Radiotherapy at randomisation (Numeric)'
	STR02L length=$40 format=$40. informat=$40. label='Prior Radiotherapy at randomisation (Long format)'
	TRT01P length=$40 format=$40. informat=$40. label='Planned treatment assigned at randomisation'
	TRT01PN length=8 format=8. informat=8. label='Planned treatment assigned at randomisation (Numeric)'
	PARAM length=$40 format=$40. informat=40. label='Analysis parameter - Progression free survival'
	PARAMCD length=$10 format=$10. informat=$10. label='Analysis parameter code'
	AVAL length=8 format=8. informat=8. label='Analysis value (time to event [days])'
	CNSR length=3 format=3. informat=3. label='Censoring (0 = Event, 1 = censored)'
	EVNTDESC length= $34 format=$34. informat=$34. label='Event description'
	CNSDTDSC length=$33 format=$33. informat=$34. label='Censoring description'
	DCTREAS length=$25 format=$25. informat=34. label='Discontinuation from study reason'
;
input
  STUDYID SUBJID USUBJID AGE STR01 STR01N STR01L	
  STR02 STR02N STR02L	TRT01P TRT01PN PARAM PARAMCD	
  AVAL CNSR	EVNTDESC CNSDTDSC DCTREAS;
run;


data TIME_TO_EVENT
( keep =  Patient Age_at_Start Age_Group Age_at_End Survival_Days Survival_Weeks
		  Full_Years Hormone_Receptor_Status Prior_Radiotherapy
		Treatment_No Treatment Censoring Death Discontinuation Discontinuation_Reason
);

attrib
	Patient length=8 
	Age_at_Start length=8
	Age_Group length=$5
	Age_at_End length=8
	Survival_Days length=8 label='Survival Days'
	Full_Years length=8
	Hormone_Receptor_Status length=$8 
	Prior_Radiotherapy length=$1
	Treatment_No length=8
	Treatment length=$40
	Censoring length=8
	Death length=8
	Discontinuation length=$1
	Discontinuation_Reason length=$25 
;
set RECD.ADTTE;

	Patient = SUBJID;
	Age_at_Start = AGE;
	Survival_Days = AVAL;
	Survival_Weeks = int(Survival_Days/7);
	Full_Years = int(Survival_Days/365.25);
	Age_at_End = Age_at_Start + Full_Years;
	Hormone_Receptor_Status = STR01;
	Prior_Radiotherapy = ifc(STR02N=1,'Y','N');
	Treatment_No = TRT01PN;
	Treatment = TRT01P;
	Censoring = CNSR;
	Death = ifc(EVNTDESC = 'Death',1,0);
	Discontinuation = ifc(DCTREAS = 'NA','N','Y');
	Discontinuation_Reason = propcase(DCTREAS);

	if Discontinuation = 'N' then Discontinuation_Reason = 'Not applicable';

	if Age_at_Start <= 50 then Age_Group = '25-50';
	else if 51 <= Age_at_Start <= 60 then Age_Group = '51-60' ;
	else if 61 <= Age_at_Start <= 70 then Age_Group = '61-70' ;
	else if 71 <= Age_at_Start <= 80 then Age_Group = '71-80' ;
	else  Age_Group = '81+' ;
run;


ods _all_ close;
ods graphics on;

ods output Survivalplot=SurvivalPlotData;
proc lifetest data=TIME_TO_EVENT plots=survival(atrisk=0 to 2000 by 10); 
	time Survival_Days * Death(0);
	strata Treatment/ test=logrank adjust=sidak;
run; 






proc sql;
create table time
as
select distinct time
from SurvivalPlotData;
quit;


data time;
set time end=eof;
i=_n_;
if eof then call symput('n',_n_);
run;


%macro km;
%do i=1 %to &n+10;

proc sql noprint;
select max(Time) into: Time
from Time
where i <= &i.;
quit;

data SPD;
set SurvivalPlotData (where=(Time le &Time.));
run;

proc sql;
create table Survival_Pct
as
select		StratumNum,
			min(Survival) as Survival,
			compress(put(min(Survival),percent8.1)) as Survival_txt
from 		SPD 
where		Survival not=.
group by	StratumNum;
quit;

data _null_;
set Survival_Pct;
if StratumNum = 1 then do;
	call symput('Trt_val_1',Survival);
	call symputx('Trt_txt_1',Survival_txt);
end;
if StratumNum = 2 then do;
	call symput('Trt_val_2',Survival);
	call symputx('Trt_txt_2',Survival_txt);
end;
if StratumNum = 3 then do;
	call symput('Trt_val_3',Survival);
	call symputx('Trt_txt_3',Survival_txt);
end;
if StratumNum = 4 then do;
	call symput('Trt_val_4',Survival);
	call symputx('Trt_txt_4',Survival_txt);
end;
run;

title j=center font=calibri height=10pt "Survival Day = &Time";
proc sgplot data=SPD noborder nowall;
 styleattrs datacontrastcolors=(CX003469 CX69b937 CXef7d04 CX00a6cb);
 step x=time y=survival / group=stratum name='s';
  scatter x=time y=censored / markerattrs=(symbol=plus) name='c';
  scatter x=time y=censored / markerattrs=(symbol=plus) GROUP=stratum;

  keylegend 'c' / location=inside position=topright valueattrs = (family=calibri size=10pt);
  keylegend 's' / linelength=20 valueattrs = (family=calibri size=10pt);
  xaxis
  	values = (0 to 2000 by 30)
	valueattrs = (family=calibri size=10pt) 
	labelattrs = (family=calibri size=10pt) 
	label = 'x - Survival Days / y - *Survival Probability'
  ;
  yaxis 
	valueattrs = (family=calibri size=10pt) 
	labelattrs = (family=calibri size=10pt) 
	values = (0 to 1 by 0.1)
	labelpos = top
	label='*'
  ;

%if &Time. ge 84 %then %do;
refline 84 / axis=x 
	lineattrs=(pattern=dash color=lightgrey) label='12 Weeks' labelattrs=(family=calibri size=10pt);
%end;

%if &Time. ge 365 %then %do;
refline 365 / axis=x 
	lineattrs=(pattern=dash color=lightgrey) label='52 Weeks / 1 Year' labelattrs=(family=calibri size=10pt);
%end;

%if &Time. ge 730 %then %do;
refline 730 / axis=x  
	lineattrs=(pattern=dash color=lightgrey) label='2 Years' labelattrs=(family=calibri size=10pt);
%end;

%if &Time. ge 1095 %then %do;
refline 1095 / axis=x 
	lineattrs=(pattern=dash color=lightgrey) label='3 Years' labelattrs=(family=calibri size=10pt)  ;
%end;

%if &Time. ge 1461 %then %do;
refline 1461 / axis=x 
	lineattrs=(pattern=dash color=lightgrey) label='4 Years' labelattrs=(family=calibri size=10pt)  ;
%end;

%if &Time. ge 1826 %then %do;
refline 1826 / axis=x 
	lineattrs=(pattern=dash color=lightgrey) label='5 Years' labelattrs=(family=calibri size=10pt)  ;
%end;

refline &Trt_val_1. / axis=y
	lineattrs=(pattern=thindot color=CX003469) label="&Trt_txt_1" labelattrs=(family=calibri size=10pt color=CX003469);

refline &Trt_val_2. / axis=y
	lineattrs=(pattern=thindot color=CX69b937) label="&Trt_txt_2" labelattrs=(family=calibri size=10pt color=CX69b937);

refline &Trt_val_3. / axis=y
	lineattrs=(pattern=thindot color=CXef7d04) label="&Trt_txt_3" labelattrs=(family=calibri size=10pt color=CXef7d04);

refline &Trt_val_4. / axis=y
	lineattrs=(pattern=thindot color=CX00a6cb) label="&Trt_txt_4" labelattrs=(family=calibri size=10pt color=CX00a6cb);
run;

title;


%end;
%mend;

/* Create the combined graph. */


options papersize=a4 printerpath=gif animation=start animduration=0.05 animloop=no noanimoverlay nodate nonumber spool;
ods printer file="c:\temp\KM_Treatment.gif";

ods graphics / width=10in height=10in  scale=on imagefmt=GIF;
%km;

options printerpath=gif animation=stop;
ods printer close;
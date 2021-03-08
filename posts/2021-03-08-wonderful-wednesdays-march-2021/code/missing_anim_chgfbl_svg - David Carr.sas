
option mprint symbolgen mlogic;

proc format;
   value treat
      1 = 'Active'
      2 = 'Placebo'
      ;
   run;

%macro graph(dset=,x=,fname=); 

options missing=' ';

  title; 
  footnote ; 

/* Determine last study day of any patient */

proc univariate data=&dset noprint;
     var ordnr; 
     output out=_tmp1 max=maxord;
run;
 
%global maxday;

data _null_;
     set _tmp1;
     call symput('maxord',put(maxord,4.));
run;

proc sort data=&dset.;
     by astdy ordnr subjid aval2;
run;

/*--Create animation--*/

options papersize=('24cm', '18cm') printerpath=svg animation=start animduration=0.01 animloop=no noanimoverlay;

ods printer dpi=400 file="&fname..svg";

ods graphics / reset imagefmt=svg  attrpriority=color width=24cm height=18cm  imagefmt=svg;

/*
options papersize=('24cm', '18cm') printerpath=gif animation=start animduration=0.01 animloop=yes noanimoverlay;
ods printer file="&fname..gif";

ods graphics / width=24cm height=18cm imagefmt=GIF;
*/

/*--This program requires SAS 9.4--*/

%do inc = 0 %to &maxday.;

    proc sgpanel data=&dset. noautolegend;
         panelby treat /  novarname rows=1 spacing=10 headerattrs=(size=9)
                    headerbackcolor=lightgray; 

         format treat treat.;

         where astdy eq &inc.;
      
         refline 0 / axis=y;

         band x=ordnr lower=-70 upper=-30 / fillattrs=(color=CXD1E5F0) transparency=0.3 outline fill;

         scatter x=ordnr y=aval1/jitter markerattrs=(color=red symbol=circlefilled size=5);

	     inset txttime / position=bottomright nolabel textattrs=(size=40 weight=bold color=lightgray);

         colaxis display=(noticks novalues nolabel) offsetmin=0.05 offsetmax=0.08 min=1 max=&maxord.;

         rowaxis offsetmin=0.05 offsetmax=0.05 grid values=(-70 to 10 by 10)
               label='Change from Baseline of Pain'  valueattrs=(size=8) labelattrs=(size=9);

   run;

%end;

options printerpath=gif animation=stop;
ods printer close;

ods graphics off;

%mend graph;

%macro interp(dset=,oset=,xvar=,yvar=);

/*
Linear interpolation implies fitting joined, straight line segments 
between adjacent points in your data and then,
for any new X value, obtaining its Y value from the line segment above it. This can be done using PROC EXPAND in 
SAS/ETS or using PROC TRANSREG in SAS/STAT as shown later in this note. 

Suppose you have the following data with observed values of Y at X=0, 1, 2, 3, and 4. You are interested in 
getting linearly interpolated values at some intermediate points (X=0.7, 1.3, 2.2, 2.6, and 3.9) that 
appear in the data set with missing values for Y. 

   data mydata;
      input x y;
      datalines;
   0.0  1
   0.7  .
   1.0  3
   1.3  .
   2.0  4
   2.2  .
   2.6  .
   3.0  6
   3.9  .
   4.0 10
   ;
*/

/*
Using PROC EXPAND

To linearly interpolate between the observed data points use PROC EXPAND in SAS/ETS. Note that the data must be 
sorted by the ID variable if it is not already. Also, only missing values that fall between non-missing 
values can be interpolated. Missing observations before the first non-missing value or after the last non-missing 
value will not be estimated. 
*/

proc sort data=&dset;
     by treat ordnr subjid &xvar.;
run;

proc expand data=&dset. out=_tmp1;
     by treat ordnr subjid;
     convert &yvar.=&yvar._interp / method=join;
     id &xvar.;
run;

data &oset.(rename=(&yvar._interp=&yvar.));
     set _tmp1(keep=treat ordnr subjid &xvar. &yvar._interp);
run;

/*
ods listing;

proc print data=&oset. width=minimum noobs;
var subjid astdy &yvar._interp &yvar.;
run;

endsas;

A plot of the data shows that the new points, denoted by blue stars, fall on straight line segments between the 
observed data points and therefore are linearly interpolated between adjacent observed values.

   symbol1 v=dot i=join c=red;
   symbol2 v=star i=none c=blue;

   proc gplot data=LinInterp;
      plot y*x=1 linear*x=2 / overlay;
      run; quit
*/

%mend  ;

/* CSV file - first row: variable name */

proc import datafile='ww_missing.csv' out=missing0 dbms=csv
replace;
delimiter=',';
guessingrows=5000000;
run;

/*
proc contents data=missing0; run;
*/
proc freq     data=missing0; 
table pain0;
run;

/*
endsas;
*/

data tmp1;
     set missing0;

     if trt eq 'act' then treat=1;
     if trt eq 'pbo' then treat=2;

     subjid=1000+_N_;

run;

/* 4 time points per day will be interpolated */

data tmp2(drop=pain0-pain10);
     set tmp1;
     array a{11} pain0-pain10;
     do i = 1 to 11;
        aval= a[i]-pain0;
        bval= a[i];
        week=i-1;
        astdy=week*28; 
        output;
     end;
run;

data alrs1;
     set tmp2(where=(aval ne .));
run;

proc sort data=alrs1;
     by treat subjid astdy;
run;

/* ALRS_ST0: first value per subjid */

data alrs_st0(keep=treat subjid bval_first);
     set alrs1;
     by treat subjid;
     if first.subjid;
     bval_first=bval;
run;

/* ALRS_ST1: ORDNR: order variable per subjid within TREAT for first value */

proc sort data=alrs_st0;
     by treat bval_first subjid;
run;

data alrs_st1;
     set alrs_st0;
     by treat bval_first subjid;
     retain ordnr; 
     if first.treat then ordnr=0;
     ordnr+1;
run;

/* Merge order variable onto ALRS1 to creat ALRS_ALL */

proc sort data=alrs1;
     by treat subjid;
run;

proc sort data=alrs_st1;
     by treat subjid;
run;

data alrs2;
     merge alrs1(in=c)
           alrs_st1(in=d);
     by treat subjid;
     if c and d;
run;

/* Extract highest value per study day */

proc sort data=alrs2 nodupkey;
     by treat ordnr subjid astdy aval;
run;

/* save highest value per day to ALRS2 */

data alrs3;
     set alrs2;
     by treat ordnr subjid astdy aval;
     if last.astdy;
run;

/* Save last value per patient ALRS_LAST */

data alrs_last(keep=treat ordnr subjid aval2);
     set alrs3;
     by treat ordnr subjid;
     if last.subjid;
     aval2=aval;
run;

ods listing close;

proc univariate data=alrs3 noprint;
     var astdy; 
     by treat ordnr subjid; 
     output out=stmp1 max=maxsub;
run;
 
proc freq data=stmp1; 
     table maxsub; 
run;

/* Determine last study day of any patient */

proc univariate data=alrs3 noprint;
     var astdy; 
     output out=stmp2 max=maxday;
run;
 
%global maxday;

data _null_;
     set stmp2;
     call symput('maxday',put(maxday,5.));
run;

/* Create matrix per subjid of all days from 1 to maxusb */

data skel1(keep=treat ordnr subjid astdy);
     set stmp1;
     do astdy=0 to maxsub;
        output;
     end;
run;
 
/* Create matrix per subjid of all days from maxsub+1 to maxday */

data skel2(keep=treat ordnr subjid astdy);
     set stmp1;
     thresh=&maxday.;
     if thresh gt maxsub then do;
        do astdy=maxsub+1 to thresh;
           output;
        end;
     end;
run;
 
%global ntrt1 ntrt2 ntrt3 ntrt4;
 
/* merge matrix with values */

proc sort data=skel1 nodupkey;
     by treat ordnr subjid astdy;
run;

data both1(drop=aval);
     merge skel1(in=c)
           alrs3(in=d);
     by treat ordnr subjid astdy;
     if c;
     aval1=aval;
run;

ods listing;

title1 'dataset: both1';
proc freq data=both1; 
     table astdy; 
run;

/* merge matrix with last value */

proc sort data=skel2 nodupkey;
     by treat ordnr subjid astdy;
run;

data both2(keep=treat ordnr subjid astdy aval2);
     merge skel2(in=c)
           alrs_last(in=d);
     by treat ordnr subjid;
     if c and d;
run;

/*
ods listing;

title3 'dataset: BOTH2';
proc freq data=both2;
table aval2; run;
*/

/* macro for linear interpolation */

ods listing close;

%interp(dset=both1,oset=tst1,xvar=astdy,yvar=aval1);

data tst3;
     set tst1(in=c)
         both2(in=d);

     format txttime $6.;

     if treat eq 1 then do;
        if astdy eq 0 then txttime='Day 0';

        if astdy gt 0 then do;
           j=int(astdy/4)+1;
           if 1<=j<=9   then txttime='Day '||put(j,1.);
          if 10<=j<=70 then txttime='Day '||put(j,2.);
        end;
     end;

run;

proc sort data=tst3 nodupkey;
     by treat ordnr subjid astdy;
run;

ods listing close;

%graph(dset=tst3,x=ordnr,fname=missing_anim_chgfbl_svg);



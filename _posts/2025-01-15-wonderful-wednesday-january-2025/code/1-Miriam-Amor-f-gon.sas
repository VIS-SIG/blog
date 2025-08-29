*=================================================================================;
*													                              ;
* Programm name: f-gon.sas					                        	      	  ;
* Author: Miriam Amor						                                      ;
* Date: December-2024															  ;
* Purpouse of the code: Creates two figures to display results by time and rater  ;
*=================================================================================;

*===========================================================;
*	IMPORT FILE												;
*===========================================================;


%web_drop_table(WORK.GON);


FILENAME REFFILE '/home/miriam_amor_meri0/goniometer.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=WORK.GON;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.GON; RUN;


%web_open_table(WORK.GON);


*===========================================================;
* CREATE PLOTS BY SUBJECT USING SGPANEL						;
*===========================================================;

ods pdf file='/home/miriam_amor_meri0/Output/f-gon.pdf';
options nodate nonumber orientation=landscape;
ods graphics / reset imagename = 'gon' imagefmt =jpeg height =15cm width = 28cm;
ods listing gpath = '/home/miriam_amor_meri0/Output/' ;

proc sgpanel data=gon;
 panelby id/onepanel novarname columns=15;
 scatter x=time y=y/group=rater name='scatter' markerattrs=(symbol=circlefilled);
 series x=time y=y/group=rater name ='serie';

 colaxis label='Time';
 rowaxis label='Knee joint angles' values=(-15 to 20 by 5) ;
 keylegend 'scatter';
 title 'Goniometer results by time and rater - pannel by subject';
run;

proc sgpanel data=gon;
 panelby rater time/onepanel   layout=lattice ;
 hbarparm category=id response=y;
 rowaxis  display=(novalues noticks) label="Subjects"; 
 colaxis label='Knee joint angles' values=(-15 to 20 by 5) ;
  title 'Goniometer results by subject - pannel by time and rater';
run;
ods listing close;
ods graphics off;
ods pdf close;

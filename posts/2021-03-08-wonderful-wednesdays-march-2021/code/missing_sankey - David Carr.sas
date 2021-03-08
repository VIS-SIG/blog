/*--------------------------------------------------------------------------------------------------

SAS RawToSankey macro created by Shane Rosanbalm of Rho, Inc. 2015

*---------- high-level overview ----------;

-  The Sankey diagram macro requires data in two structures:
   -  The NODES dataset must be one record per bar segment.
   -  The LINKS dataset must be one record per connection between bar segments. 
-  This macro transforms a vertical dataset (i.e., one record per SUBJECT and XVAR) into the 
   Sankey NODES and LINKS structures.

*---------- required parameters ----------;

data=             vertical dataset to be converted to sankey structures

subject=          subject identifier

yvar=             categorical y-axis variable
                  converted to values 1-N for use in plotting
                  
xvar=             categorical x-axis variable
                  converted to values 1-N for use in plotting

*---------- optional parameters ----------;

completecases=    whether or not to require non-missing yvar at all xvar values
                  valid values: yes/no.
                  default: yes.
                  
outlib=           library in which to save NODES and LINKS datasets
                  default is the WORK library
                  
yvarord=          sort order for y-axis conversion, in a comma separated list
                     e.g., yvarord=%quote(red rum, george smith, tree)
                  default sort is equivalent to ORDER=DATA
                  
xvarord=          sort order for x-axis conversion, in a comma separated list
                     e.g., xvarord=%quote(pink plum, fred funk, grass)
                  default sort is equivalent to ORDER=DATA

-------------------------------------------------------------------------------------------------*/

option mprint notes source source2;

%macro rawtosankey
   (data=
   ,subject=
   ,yvar=
   ,xvar=
   ,completecases=
   ,missflag=
   ,outlib=work
   ,yvarord=
   ,xvarord=
   );


   %*---------- localization ----------;
   
   %local i;
   
   
   %*---------- return code ----------;
   
   %global rts;
   %let rts = 0;
   

   %*-----------------------------------------------------------------------------------------;
   %*---------- display parameter values at the top (for debugging) ----------;
   %*-----------------------------------------------------------------------------------------;
   
   %put &=data;
   %put &=subject;
   %put &=yvar;
   %put &=xvar;
   %put &=outlib;
   %put &=yvarord;
   %put &=xvarord;
   
   
   
   %*-----------------------------------------------------------------------------------------;
   %*---------- basic parameter checks ----------;
   %*-----------------------------------------------------------------------------------------;
   
   
   %*---------- dataset exists ----------;
   
   %let _dataexist = %sysfunc(exist(&data));
   %if &_dataexist = 0 %then %do;
      %put %str(W)ARNING: RawToSankey -> DATASET [&data] DOES NOT EXIST;
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   
   %*---------- variables exist ----------;
   
   %macro varexist(data,var);
      %let dsid = %sysfunc(open(&data)); 
      %if &dsid %then %do; 
         %let varnum = %sysfunc(varnum(&dsid,&var));
         %if &varnum %then &varnum; 
         %else 0;
         %let rc = %sysfunc(close(&dsid));
      %end;
      %else 0;
   %mend varexist;
   
   %if %varexist(&data,&subject) = 0 %then %do;
      %put %str(W)ARNING: RawToSankey -> VARIABLE [&subject] DOES NOT EXIST;
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %if %varexist(&data,&yvar) = 0 %then %do;
      %put %str(W)ARNING: RawToSankey -> VARIABLE [&yvar] DOES NOT EXIST;
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %if %varexist(&data,&xvar) = 0 %then %do;
      %put %str(W)ARNING: RawToSankey -> VARIABLE [&xvar] DOES NOT EXIST;
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   

   %*---------- eject missing yvar records ----------;
   
   data _nodes00;
      set &data;
      %if &completecases eq yes %or &missflag eq yes %then %do;
         where not missing(&yvar);
      %end;
   run;
   
   
   %*---------- convert numeric yvar to character (for easier processing) ----------;
   
   %let dsid = %sysfunc(open(&data)); 
   %let varnum = %sysfunc(varnum(&dsid,&yvar));
   %let vartype = %sysfunc(vartype(&dsid,&varnum));
   %if &vartype = N %then %do; 
      data _nodes00;
         set _nodes00 (rename=(&yvar=_&yvar));
         &yvar = compress(put(_&yvar,best.));
         drop _&yvar;
      run;
   %end;
   %let rc = %sysfunc(close(&dsid));
   
   
   %*---------- convert numeric xvar to character (for easier processing) ----------;
   
   %let dsid = %sysfunc(open(&data)); 
   %let varnum = %sysfunc(varnum(&dsid,&xvar));
   %let vartype = %sysfunc(vartype(&dsid,&varnum));
   %if &vartype = N %then %do; 
      data _nodes00;
         set _nodes00 (rename=(&xvar=_&xvar));
         &xvar = compress(put(_&xvar,best.));
         drop _&xvar;
      run;
   %end;
   %let rc = %sysfunc(close(&dsid));
   
   
   %*---------- left justify xvar and yvar values (inelegant solution) ----------;
   
   data _nodes00;
      set _nodes00;
      &yvar = left(&yvar);
      &xvar = left(&xvar);
   run;
   
   
   %*---------- if no yvarord specified, build one using ORDER=DATA model ----------;
   
   proc sql noprint;
      select   distinct &yvar
      into     :yvar1-
      from     _nodes00
      ;
      %global n_yvar;
      %let n_yvar = &sqlobs;
      %put &=n_yvar;
   quit;
      
   %if &yvarord eq %str() %then %do;
   
      proc sql noprint;
         select   max(length(&yvar))
         into     :ml_yvar
         from     _nodes00
         ;
         %put &=ml_yvar;
      quit;
   
      data _null_;
         set _nodes00 (keep=&yvar) end=eof;
         array ordered {&n_yvar} $&ml_yvar;
         retain filled ordered1-ordered&n_yvar;
      
         *--- first record seeds array ---;
         if _N_ = 1 then do;
            filled = 1;
            ordered[filled] = &yvar;
         end;
      
         *--- if subsequent records not yet in array, add them ---;
         else do;
            hit = 0;
            do i = 1 to &n_yvar;
               if ordered[i] = &yvar then hit = 1;
            end;
            if hit = 0 then do;
               filled + 1;
               ordered[filled] = &yvar;
            end;
         end;
      
         *--- concatenate array elements into one variable ---;
         if eof then do;
            yvarord = catx(', ',of ordered1-ordered&n_yvar);
            call symputx('yvarord',yvarord);
         end;
      run;
      
   %end;

   %put &=yvarord;


   %*---------- if no xvarord specified, build one using ORDER=DATA model ----------;
   
   proc sql noprint;
      select   distinct &xvar
      into     :xvar1-
      from     _nodes00
      ;
      %global n_xvar;
      %let n_xvar = &sqlobs;
      %put &=n_xvar;
   quit;
      
   %if &xvarord eq %str() %then %do;
   
      proc sql noprint;
         select   max(length(&xvar))
         into     :ml_xvar
         from     _nodes00
         ;
         %put &=ml_xvar;
      quit;
   
      data _null_;
         set _nodes00 (keep=&xvar) end=eof;
         array ordered {&n_xvar} $&ml_xvar;
         retain filled ordered1-ordered&n_xvar;
      
         *--- first record seeds array ---;
         if _N_ = 1 then do;
            filled = 1;
            ordered[filled] = &xvar;
         end;
      
         *--- if subsequent records not yet in array, add them ---;
         else do;
            hit = 0;
            do i = 1 to &n_xvar;
               if ordered[i] = &xvar then hit = 1;
            end;
            if hit = 0 then do;
               filled + 1;
               ordered[filled] = &xvar;
            end;
         end;
      
         *--- concatenate array elements into one variable ---;
         if eof then do;
            xvarord = catx(', ',of ordered1-ordered&n_xvar);
            call symputx('xvarord',xvarord);
         end;
      run;
      
   %end;

   %put &=xvarord;


   %*---------- parse yvarord ----------;
   
   %let commas = %sysfunc(count(%bquote(&yvarord),%bquote(,)));
   %let n_yvarord = %eval(&commas + 1);
   %put &=commas &=n_yvarord;
   
   %do i = 1 %to &n_yvarord;
      %global yvarord&i;      
      %let yvarord&i = %scan(%bquote(&yvarord),&i,%bquote(,));
      %put yvarord&i = [&&yvarord&i];      
   %end;
   
   
   %*---------- parse xvarord ----------;
   
   %let commas = %sysfunc(count(%bquote(&xvarord),%bquote(,)));
   %let n_xvarord = %eval(&commas + 1);
   %put &=commas &=n_xvarord;
   
   %do i = 1 %to &n_xvarord;      
      %global xvarord&i;
      %let xvarord&i = %scan(%bquote(&xvarord),&i,%bquote(,));
      %put xvarord&i = [&&xvarord&i];      
   %end;
      
   
   %*-----------------------------------------------------------------------------------------;
   %*---------- yvarord vs. yvar ----------;
   %*-----------------------------------------------------------------------------------------;
   
   
   %*---------- same number of values ----------;

   %if &n_yvarord ne &n_yvar %then %do;
      %put %str(W)ARNING: RawToSankey -> NUMBER OF yvarord= VALUES [&n_yvarord];
      %put %str(W)ARNING: RawToSankey -> DOES NOT MATCH NUMBER OF yvar= VALUES [&n_yvar];
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %*---------- put yvarord and yvar into quoted lists ----------;
   
   proc sql noprint;
      select   distinct quote(trim(left(&yvar)))
      into     :_yvarlist
      separated by ' '
      from     _nodes00
      ;
   quit;
   
   %put &=_yvarlist;
   
   data _null_;
      length _yvarordlist $2000;
      %do i = 1 %to &n_yvarord;
         _yvarordlist = trim(_yvarordlist) || ' ' || quote("&&yvarord&i");
      %end;
      call symputx('_yvarordlist',_yvarordlist);
   run;
   
   %put &=_yvarordlist;
   
   %*---------- check lists in both directions ----------;
   
   data _null_;
      array yvarord (&n_yvarord) $200 (&_yvarordlist);
      array yvar (&n_yvar) $200 (&_yvarlist);
      call symputx('_badyvar',0);
      %do i = 1 %to &n_yvarord;
         if "&&yvarord&i" not in (&_yvarlist) then call symputx('_badyvar',1);
      %end;
      %do i = 1 %to &n_yvar;
         if "&&yvar&i" not in (&_yvarordlist) then call symputx('_badyvar',2);
      %end;
   run;
   
   %if &_badyvar eq 1 %then %do;
      %put %str(W)ARNING: RawToSankey -> VALUE WAS FOUND IN yvarord= [&_yvarordlist];
      %put %str(W)ARNING: RawToSankey -> THAT IS NOT IN yvar= [&_yvarlist];
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %if &_badyvar eq 2 %then %do;
      %put %str(W)ARNING: RawToSankey -> VALUE WAS FOUND IN yvar= [&_yvarlist];
      %put %str(W)ARNING: RawToSankey -> THAT IS NOT IN yvarord= [&_yvarordlist];
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
      

   %*-----------------------------------------------------------------------------------------;
   %*---------- xvarord vs. xvar ----------;
   %*-----------------------------------------------------------------------------------------;
   
   
   %*---------- same number of values ----------;
   
   %if &n_xvarord ne &n_xvar %then %do;
      %put %str(W)ARNING: RawToSankey -> NUMBER OF xvarord= VALUES [&n_xvarord];
      %put %str(W)ARNING: RawToSankey -> DOES NOT MATCH NUMBER OF xvar= VALUES [&n_xvar];
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %*---------- put xvarord and xvar into quoted lists ----------;
   
   proc sql noprint;
      select   distinct quote(trim(left(&xvar)))
      into     :_xvarlist
      separated by ' '
      from     _nodes00
      ;
   quit;
   
   %put &=_xvarlist;
   
   data _null_;
      length _xvarordlist $2000;
      %do i = 1 %to &n_xvarord;
         _xvarordlist = trim(_xvarordlist) || ' ' || quote("&&xvarord&i");
      %end;
      call symputx('_xvarordlist',_xvarordlist);
   run;
   
   %put &=_xvarordlist;
   
   %*---------- check lists in both directions ----------;
   
   data _null_;
      array xvarord (&n_xvarord) $200 (&_xvarordlist);
      array xvar (&n_xvar) $200 (&_xvarlist);
      call symputx('_badxvar',0);
      %do i = 1 %to &n_xvarord;
         if "&&xvarord&i" not in (&_xvarlist) then call symputx('_badxvar',1);
      %end;
      %do i = 1 %to &n_xvar;
         if "&&xvar&i" not in (&_xvarordlist) then call symputx('_badxvar',2);
      %end;
   run;
   
   %if &_badxvar eq 1 %then %do;
      %put %str(W)ARNING: RawToSankey -> VALUE WAS FOUND IN xvarord= [&_xvarordlist];
      %put %str(W)ARNING: RawToSankey -> THAT IS NOT IN xvar= [&_xvarlist];
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %if &_badxvar eq 2 %then %do;
      %put %str(W)ARNING: RawToSankey -> VALUE WAS FOUND IN xvar= [&_xvarlist];
      %put %str(W)ARNING: RawToSankey -> THAT IS NOT IN xvarord= [&_xvarordlist];
      %put %str(W)ARNING: RawToSankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
      

   %*-----------------------------------------------------------------------------------------;
   %*---------- enumeration ----------;
   %*-----------------------------------------------------------------------------------------;


   %*---------- enumerate yvar values ----------;
   
   proc sort data=_nodes00 out=_nodes05;
      by &yvar;
   run;
   
   data _nodes10;
      set _nodes05;
      by &yvar;
      %do i = 1 %to &n_yvarord;
         if &yvar = "&&yvarord&i" then y = &i;
      %end;
   run;
   
   %*---------- enumerate xvar values ----------;
   
   proc sort data=_nodes10 out=_nodes15;
      by &xvar;
   run;   
   
   data _nodes20;
      set _nodes15;
      by &xvar;
      %do i = 1 %to &n_xvarord;
         if &xvar = "&&xvarord&i" then x = &i;
      %end;
   run;
   
   %*---------- subset if doing complete cases ----------;
   
   proc sql noprint;
      select   max(x)
      into     :xmax
      from     _nodes20
      ;
      %put &=xmax;
   quit;
   
   proc sql;
      create table _nodes30 as
      select   *
      from     _nodes20
      %if &completecases eq yes %then 
         group by &subject
         having   count(*) eq &xmax
         ;
      ;
   quit;

   %*---------- count subjects in case not doing complete cases ----------;

   %global subject_n;
   
   proc sql noprint;
      select   count(distinct &subject)
      into     :subject_n

      %if &completecases eq yes %then
         from     _nodes30
         ;

      %if &completecases eq no %then
         from     _nodes10
         ;      
      ;

      %put &=subject_n;

   quit;
   
   
   %*-----------------------------------------------------------------------------------------;
   %*---------- transform raw data to nodes structure ----------;
   %*-----------------------------------------------------------------------------------------;


   proc sql;
      create table _nodes40 as
      select   x, y, count(*) as size
      from     _nodes30
      group by x, y
      ;
   quit;
   
   data &outlib..nodes;
      set _nodes40;
      length xc yc $200;
      %do i = 1 %to &n_xvarord;
         if x = &i then xc = "&&xvarord&i";
      %end;
      %do i = 1 %to &n_yvarord;
         if y = &i then yc = "&&yvarord&i";
      %end;
   run;

   
   %*-----------------------------------------------------------------------------------------;
   %*---------- transform raw data to links structure ----------;
   %*-----------------------------------------------------------------------------------------;


   proc sort data=_nodes30 out=_links00;
      by &subject x;
   run;
   
   data _links10;
      set _links00;
      by &subject x;
      retain lastx lasty;
      if first.&subject then call missing(lastx,lasty);
      else if lastx + 1 eq x then do;
         x1 = lastx;
         y1 = lasty;
         x2 = x;
         y2 = y;
         output;
      end;
      lastx = x;
      lasty = y;
   run;

   proc sql noprint;
      create table &outlib..links as
      select   x1, y1, x2, y2, count(*) as thickness
      from     _links10
      group by x1, y1, x2, y2
      ;
   quit;
   
   
   %*--------------------------------------------------------------------------------;
   %*---------- clean up ----------;
   %*--------------------------------------------------------------------------------;
   
   
   %if &debug eq no %then %do;
   
      proc datasets library=work nolist;
         delete _nodes: _links:;
      run; quit;
   
   %end;
   
   
   %*---------- return code ----------;
   
   %let rts = 1;
   


%mend rawtosankey;

/*--------------------------------------------------------------------------------------------------

SAS Sankey macro created by Shane Rosanbalm of Rho, Inc. 2015

*---------- high-level overview ----------;

-  This macro creates a stacked bar chart with sankey-like links between the stacked bars. 
   It is intended to display the change over time in subject endpoint values.
   These changes are depicted by bands flowing from left to right between the stacked bars. 
   The thickness of each band corresponds to the number of subjects moving from the left to 
   the right.
-  The macro assumes two input datasets exist: NODES and LINKS.
   -  Use the macro %RawToSankey to help build NODES and LINKS from a vertical dataset.
   -  The NODES dataset must be one record per bar segment, with variables:
      -  X and Y (the time and response), 
      -  XC and YC (the character versions of X and Y),
      -  SIZE (the number of subjects represented by the bar segment).
      -  The values of X and Y should be integers starting at 1.
      -  Again, %RawToSankey will build this dataset for you.
   -  The LINKS dataset must be one record per link, with variables:
      -  X1 and Y1 (the time and response to the left), 
      -  X2 and Y2 (the time and response to the right), 
      -  THICKNESS (the number of subjects represented by the band). 
      -  The values of X1, Y1, X2, and Y2 should be integers starting at 1.
      -  Again, %RawToSankey will build this dataset for you.
-  The chart is produced using SGPLOT. 
   -  The procedure contains one HIGHLOW statement per node (i.e., per bar segment).
   -  The procedure contains one BAND statement per link (i.e., per connecting band).
   -  The large volume of HIGHLOW and BAND statements is necessary to get color consistency in 
      v9.3 (in v9.4 we perhaps could have used attribute maps to clean things up a bit).
-  Any ODS GRAPHICS adjustments (e.g., HEIGHT=, WIDTH=, IMAGEFMT=, etc.) should be made prior to 
   calling the macro.
-  Any fine tuning of axes or other appearance options will need to be done in (a copy of) the 
   macro itself.

*---------- required parameters ----------;

There are no required parameters for this macro.

*---------- optional parameters ----------;

sankeylib=        Library where NODES and LINKS datasets live.
                  Default: WORK
                  
colorlist=        A space-separated list of colors: one color per response group.
                  Not compatible with color descriptions (e.g., very bright green).
                  Default: the qualitative Brewer palette.

barwidth=         Width of bars.
                  Values must be in the 0-1 range.
                  Default: 0.25.
                  
yfmt=             Format for yvar/legend.
                  Default: values of yvar variable in original dataset.

xfmt=             Format for x-axis/time.
                  Default: values of xvar variable in original dataset.

legendtitle=      Text to use for legend title.
                     e.g., legendtitle=%quote(Response Value)

interpol=         Method of interpolating between bars.
                  Valid values are: cosine, linear.
                  Default: cosine.

stat=             Show percents or frequencies on y-axis.
                  Valid values: percent/freq.
                  Default: percent.
                  
datalabel=        Show percents or frequencies inside each bar.
                  Valid values: yes/no.
                  Default: yes.
                  Interaction: will display percents or frequences per stat=.
                  
*---------- depricated parameters ----------;

percents=         Show percents inside each bar.
                  This has been replaced by datalabel=. 

-------------------------------------------------------------------------------------------------*/



%macro sankey
   (sankeylib=work
   ,colorlist=
   ,barwidth=0.25
   ,yfmt=
   ,xfmt=
   ,legendtitle=
   ,interpol=cosine
   ,stat=percent
   ,datalabel=yes
   ,percents=
   );



   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   %*---------- some preliminaries ----------;
   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;



   %*---------- localization ----------;
   
   %local i j;
   
   %*---------- deal with percents= parameter ----------;
   
   %if &percents ne %then %do;
      %put %str(W)ARNING: Sankey -> PARAMETER percents= HAS BEEN DEPRICATED.;
      %put %str(W)ARNING: Sankey -> PLEASE SWITCH TO PARAMETER datalabel=.;
      %put %str(W)ARNING: Sankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %*---------- dataset exists ----------;
   
   %let _dataexist = %sysfunc(exist(&sankeylib..nodes));
   %if &_dataexist = 0 %then %do;
      %put %str(W)ARNING: Sankey -> DATASET [&sankeylib..nodes] DOES NOT EXIST.;
      %put %str(W)ARNING: Sankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   data nodes;
      set &sankeylib..nodes;
   run;
   
   %let _dataexist = %sysfunc(exist(&sankeylib..links));
   %if &_dataexist = 0 %then %do;
      %put %str(W)ARNING: Sankey -> DATASET [&sankeylib..links] DOES NOT EXIST.;
      %put %str(W)ARNING: Sankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   data links;
      set &sankeylib..links;
   run;
   
   %*---------- variables exist ----------;
   
   %macro varexist(data,var);
      %let dsid = %sysfunc(open(&data)); 
      %if &dsid %then %do; 
         %let varnum = %sysfunc(varnum(&dsid,&var));
         %if &varnum %then &varnum; 
         %else 0;
         %let rc = %sysfunc(close(&dsid));
      %end;
      %else 0;
   %mend varexist;
   
   %if %varexist(nodes,x) = 0 or %varexist(nodes,y) = 0 or %varexist(nodes,size) = 0 %then %do;
      %put %str(W)ARNING: Sankey -> DATASET [work.nodes] MUST HAVE VARIABLES [x y size].;
      %put %str(W)ARNING: Sankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %if %varexist(links,x1) = 0 or %varexist(links,y1) = 0 or %varexist(links,x2) = 0 
         or %varexist(links,y2) = 0 or %varexist(links,thickness) = 0 %then %do;
      %put %str(W)ARNING: Sankey -> DATASET [work.links] MUST HAVE VARIABLES [x1 y1 x2 y2 thickness].;
      %put %str(W)ARNING: Sankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   
   %*---------- preliminary sorts (and implicit dataset/variable checking) ----------;
   
   proc sort data=nodes;
      by y x size;
   run;

   proc sort data=links;
      by x1 y1 x2 y2 thickness;
   run;
   
   %*---------- break apart colors ----------;

   %if &colorlist eq %str() 
      %then %let colorlist = cxa6cee3 cx1f78b4 cxb2df8a cx33a02c cxfb9a99 cxe31a1c 
                             cxfdbf6f cxff7f00 cxcab2d6 cx6a3d9a cxffff99 cxb15928;
   %let n_colors = %sysfunc(countw(&colorlist));
   %do i = 1 %to &n_colors;
      %let color&i = %scan(&colorlist,&i,%str( ));
      %put color&i = [&&color&i];
   %end;
   
   %*---------- xfmt ----------;
   
   %if &xfmt eq %str() %then %do;
   
      %let xfmt = xfmt.;
      
      proc format;
         value xfmt
         %do i = 1 %to &n_xvar;
            &i = "&&xvarord&i"
         %end;
         ;
      run;
      
   %end;
   
   %put &=xfmt;
   
   %*---------- number of rows ----------;

   proc sql noprint;
      select   max(y)
      into     :maxy
      from     nodes
      ;
   quit;
   
   %*---------- number of time points ----------;

   proc sql noprint;
      select   max(x)
      into     :maxx
      from     nodes
      ;
   quit;
   
   %*---------- corresponding text ----------;
   
   proc sql noprint;
      select   distinct y, yc
      into     :dummy1-, :yvarord1-
      from     nodes
      ;
   quit;
   
   %do i = 1 %to &sqlobs;
      %put yvarord&i = [&&yvarord&i];
   %end;
   
   %*---------- validate interpol ----------;
   
   %let _badinterpol = 0;
   data _null_;
      if      upcase("&interpol") = 'LINEAR' then call symput('interpol','linear');
      else if upcase("&interpol") = 'COSINE' then call symput('interpol','cosine');
      else call symput('_badinterpol','1');
   run;
   
   %if &_badinterpol eq 1 %then %do;
      %put %str(W)ARNING: Sankey -> THE VALUE INTERPOL= [&interpol] IS INVALID.;
      %put %str(W)ARNING: Sankey -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;
   


   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   %*---------- convert counts to percents for nodes ----------;
   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   
   
   
   ods select none;
   ods output crosstabfreqs=_ctfhl (where=(_type_='11'));

   proc freq data=nodes;
      table x*y;
      weight size;
   run;

   ods select all;
   
   data _highlow;
      set _ctfhl;
      by x;
      node = _N_;
      retain cumpercent;
      if first.x then cumpercent = 0;
      lowpercent = cumpercent;
      highpercent = cumpercent + 100*frequency/&subject_n;
      cumpercent = highpercent;   
      retain cumcount;
      if first.x then cumcount = 0;
      lowcount = cumcount;
      highcount = cumcount + frequency;
      cumcount = highcount;   
      keep x y node lowpercent highpercent lowcount highcount;   
   run;
   
   proc sql noprint;
      select   max(node)
      into     :maxhighlow
      from     _highlow
      ;
   quit;



   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   %*---------- write a bunch of highlow statements ----------;
   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;



   data _highlow_statements;
      set _highlow;
      by x;
      length highlow $200 color $20 legendlabel $40 scatter $200;

      %*---------- choose color based on y ----------;
      %do c = 1 %to &maxy;
         if y = &c then color = "&&color&c";
      %end;

      %*---------- create node specific x, low, high variables and write highlow statement ----------;

      %do j = 1 %to &maxhighlow;

         %let jc = %sysfunc(putn(&j,z%length(&maxhighlow).));
         %let jro = %sysfunc(mod(&j,&maxy));
         %if &jro = 0 %then %let jro = &maxy;

         if node = &j then do;
            xb&jc = x;

            lowb&jc = lowpercent;
            %if &stat eq freq %then
               lowb&jc = lowb&jc*&subject_n/100;;

            highb&jc = highpercent;
            %if &stat eq freq %then
               highb&jc = highb&jc*&subject_n/100;;

            %if &yfmt eq %then 
               legendlabel = "&&yvarord&jro" ;
            %else %if &yfmt ne %then
               legendlabel = put(y,&yfmt.) ;
            ;

            highlow = "highlow x=xb&jc low=lowb&jc high=highb&jc / type=bar barwidth=&barwidth" ||
               " fillattrs=(color=" || trim(color) || ")" ||
               " lineattrs=(color=black pattern=solid)" ||
               " name='" || trim(color) || "' legendlabel='" || trim(legendlabel) || "';";

            *--- sneaking in a scatter statement for percent annotation purposes ---;

            mean = mean(lowpercent,highpercent);
            %if &stat eq freq %then
               mean = mean(lowcount,highcount);;

            percent = highpercent - lowpercent;
            %if &stat eq freq %then
               percent = highcount - lowcount;;

            if percent >= 1 then do;

               meanb&jc = mean;

               textb&jc = compress(put(percent,3.)) || '%';
               %if &stat eq freq %then
                  textb&jc = compress(put(percent,3.));;

               scatter = "scatter x=xb&jc y=meanb&jc / x2axis markerchar=textb&jc;";

            end;
         end;
      %end;

   run;

   proc sql noprint;
      select   distinct trim(highlow)
      into     :highlow
      separated by ' '
      from     _highlow_statements
      where    highlow is not missing
      ;
   quit;

   %put highlow = [%nrbquote(&highlow)];

   proc sql noprint;
      select   distinct trim(scatter)
      into     :scatter
      separated by ' '
      from     _highlow_statements
      where    scatter is not missing
      ;
   quit;

   %put scatter = [%nrbquote(&scatter)];
   
   
   %*---------- calculate offset based on bar width and maxx ----------;
   
   data _null_;
      if &maxx = 2 then offset = 0.25;
      else if &maxx = 3 then offset = 0.15;
      else offset = 0.05 + 0.03*((&barwidth/0.25)-1);
      call symputx ('offset',offset);
   run;   

   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   %*---------- convert counts to percents for links ----------;
   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;

   %*---------- left edge of each band ----------;
   
   proc sql;
      create   table _links1 as
      select   a.*, b.highcount as cumthickness 
      from     links as a
               inner join _highlow (where=(highcount~=lowcount)) as b
               on a.x1 = b.x 
                  and a.y1 = b.y
      order by x1, y1, x2, y2
      ;
   quit;
   
   data _links2;
      set _links1;
      by x1 y1 x2 y2;
      link = _N_;
      xt1 = x1;
      retain lastybhigh1;
      if first.x1 then 
         lastybhigh1 = 0;
      yblow1 = lastybhigh1;
      ybhigh1 = lastybhigh1 + thickness/&subject_n;
      lastybhigh1 = ybhigh1;
      if last.y1 then
         lastybhigh1 = cumthickness/&subject_n;
   run;
   
   %*---------- right edge of each band ----------;
   
   proc sql;
      create   table _links3 as
      select   a.*, b.highcount as cumthickness 
      from     links as a
               inner join _highlow (where=(highcount~=lowcount)) as b
               on a.x2 = b.x 
                  and a.y2 = b.y
      order by x2, y2, x1, y1
      ;
   quit;
   
   data _links4;
      set _links3;
      by x2 y2 x1 y1;
      retain lastybhigh2;
      if first.x2 then 
         lastybhigh2 = 0;
      xt2 = x2;
      yblow2 = lastybhigh2;
      ybhigh2 = lastybhigh2 + thickness/&subject_n;
      lastybhigh2 = ybhigh2;
      if last.y2 then
         lastybhigh2 = cumthickness/&subject_n;
   run;
   
   %*---------- make vertical ----------;
   
   proc sort data=_links2 out=_links2b;
      by x1 y1 x2 y2;
   run;
   
   proc sort data=_links4 out=_links4b;
      by x1 y1 x2 y2;
   run;
   
   data _links5;
      merge
         _links2b (keep=x1 y1 x2 y2 xt1 yblow1 ybhigh1 link)
         _links4b (keep=x1 y1 x2 y2 xt2 yblow2 ybhigh2)
         ;
      by x1 y1 x2 y2;
   run;
   
   data _links6;
      set _links5;
      
      xt1alt = xt1 + &barwidth*0.48;
      xt2alt = xt2 - &barwidth*0.48;
      
      %if &interpol eq linear %then %do;
      
         do xt = xt1alt to xt2alt by 0.01;
            *--- low ---;
            mlow = (yblow2 - yblow1) / (xt2alt - xt1alt);
            blow = yblow1 - mlow*xt1alt;
            yblow = mlow*xt + blow;
            *--- high ---;
            mhigh = (ybhigh2 - ybhigh1) / (xt2alt - xt1alt);
            bhigh = ybhigh1 - mhigh*xt1alt;
            ybhigh = mhigh*xt + bhigh;
            output;
         end;
         
      %end;

      %if &interpol eq cosine %then %do;
      
         do xt = xt1alt to xt2alt by 0.01;
            b = constant('pi')/(xt2alt-xt1alt);
            c = xt1alt;
            *--- low ---;
            alow = (yblow1 - yblow2) / 2;
            dlow = yblow1 - ( (yblow1 - yblow2) / 2 );
            yblow = alow * cos( b*(xt-c) ) + dlow;
            *--- high ---;
            ahigh = (ybhigh1 - ybhigh2) / 2;
            dhigh = ybhigh1 - ( (ybhigh1 - ybhigh2) / 2 );
            ybhigh = ahigh * cos( b*(xt-c) ) + dhigh;
            output;
         end;
         
      %end;
      
      keep xt yblow ybhigh link y1;
   run;
   
   proc sort data=_links6;
      by link xt;
   run;
   
   %*---------- number of links ----------;

   proc sql noprint;
      select   max(link)
      into     :maxband
      from     _links6
      ;
   quit;
   
   %*---------- write the statements ----------;
   
   data _band_statements;
      set _links6;
      by link xt;
      length band $200 color $20;

      %*---------- choose color based on y1 ----------;
      %do c = 1 %to &maxy;
         if y1 = &c then color = "&&color&c";
      %end;

      %*---------- create link specific x, y variables and write series statements ----------;
      %do j = 1 %to &maxband;
         %let jc = %sysfunc(putn(&j,z%length(&maxband).));
         if link = &j then do;
            xt&jc = xt;
            yblow&jc = 100*yblow;
            %if &stat eq freq %then
               yblow&jc = yblow&jc*&subject_n/100;;
            ybhigh&jc = 100*ybhigh;
            %if &stat eq freq %then
               ybhigh&jc = ybhigh&jc*&subject_n/100;;
            band = "band x=xt&jc lower=yblow&jc upper=ybhigh&jc / x2axis transparency=0.5" || 
               " fill fillattrs=(color=" || trim(color) || ")" ||
               " ;";
         end;
      %end;

   run;

   proc sql noprint;
      select   distinct trim(band)
      into     :band
      separated by ' '
      from     _band_statements
      where    band is not missing
      ;
   quit;

   %put band = [%nrbquote(&band)];
   
                     
   
   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   %*---------- plot it ----------;
   %*----------------------------------------------------------------------------------------------;
   %*----------------------------------------------------------------------------------------------;
   
   
   
   data _all;
      set _highlow_statements _band_statements;
   run;
   
   ods graphics on / reset=all height=16cm width=24.0cm ANTIALIASMAX=7100 imagefmt=png border=off imagename="&plotname.";

   goptions targetdevice=png device=png gsfmode=replace goutmode=replace xmax=23cm ymax=15cm gunit=pct
         horigin=0cm vorigin=0cm htext=0.4cm noborder ftext='Albany AMT' ftitle='Albany AMT' 
         FONTRES=PRESENTATION cback=white gwait=2 rotate=landscape ;

   ods listing image_dpi=600;

   proc sgplot data=_all noautolegend;
      %*---------- plotting statements ----------;
      &band;
      &highlow;
      %if &datalabel eq yes %then &scatter;;
      %*---------- axis and legend statements ----------;
      x2axis display=(nolabel noticks) min=1 max=&maxx integer offsetmin=&offset offsetmax=&offset 
         tickvalueformat=&xfmt;
      xaxis display=none type=discrete offsetmin=&offset offsetmax=&offset 
         tickvalueformat=&xfmt;
      yaxis offsetmin=0.02 offsetmax=0.02 grid 
         %if &stat eq freq %then label="Frequency";
         %else label="Percentage (%)";
         ;

    %if &legendtitle ne %then %do;

      keylegend %do i = 1 %to &maxy; "&&color&i" %end; / title="&legendtitle";

   %end;
   run;
   
   %*--------------------------------------------------------------------------------;
   %*---------- clean up ----------;
   %*--------------------------------------------------------------------------------;
   
   
   %if &debug eq no %then %do;
   
      proc datasets library=work nolist nodetails;
         delete _nodes: _links: _all: _band: _highlow: _ctfhl _denom:;
      run; quit;
   
   %end;
   


%mend sankey;

/*--------------------------------------------------------------------------------------------------

SAS Sankey macro created by Shane Rosanbalm of Rho, Inc. 2015

*---------- high-level overview ----------;

-  This macro creates a stacked bar chart with Sankey-like links between the stacked bars. 
   The graphic is intended to display the change over time in categorical subject endpoint 
   values. These changes are depicted by bands flowing from left to right between the stacked 
   bars. The thickness of each band corresponds to the number of subjects moving from the left 
   to the right.
-  This macro is actually just a wrapper macro that contains two smaller macros. 
   -  The first inner macro, %RawToSankey, performs a data transformation. Assuming an input  
      dataset that is vertical (i.e., one record per subject and visit), the macro 
      generates two sets of counts:
      (a)   The number of subjects at each endpoint*visit combination (aka, NODES).
            E.g., how many subjects had endpoint=1 at visit=3?
      (b)   The number of subjects transitioning between endpoint categories at adjacent 
            visits (aka LINKS).
            E.g., how many subjects had endpoint=1 at visit=3 and endpoint=3 at visit=4?
      -  By default the endpoint and visit values are sorted using the ORDER=DATA principle.
         The optional parameter yvarord= and xvarord= can be used to change the display order.
   -  The second inner macro, %Sankey, uses SGPLOT to generate the bar chart (using the NODES 
      dataset) and the Sankey-like connectors (using the LINKS dataset).
      -  Any ODS GRAPHICS adjustments (e.g., HEIGHT=, WIDTH=, IMAGEFMT=, etc.) should be made 
         prior to calling the macro.
      -  There are a few optional parameters for changing the appearance of the graph (colors, 
         bar width, x-axis format, etc.), but it is likely that most seasoned graphers will want 
         to further customize the resulting figure. In that case, it is probably best to simply 
         make a copy of the %Sankey macro and edit away.

*---------- required parameters ----------;

data=             Vertical dataset to be converted to sankey structures

subject=          Subject identifier

yvar=             Categorical y-axis variable
                  Converted to values 1-N for use in plotting
                  
xvar=             Categorical x-axis variable
                  Converted to values 1-N for use in plotting

*---------- optional parameters ----------;

completecases=    Whether or not to require non-missing yvar at all xvar values for a subject
                  Valid values: yes/no.
                  Default: yes.
                  
yvarord=          Sort order for y-axis conversion, in a comma separated list
                     e.g., yvarord=%quote(red rum, george smith, tree)
                  Default sort is equivalent to ORDER=DATA
                  
xvarord=          Sort order for x-axis conversion, in a comma separated list
                     e.g., xvarord=%quote(pink plum, fred funk, grass)
                  Default sort is equivalent to ORDER=DATA

colorlist=        A space-separated list of colors: one color per yvar group.
                  Not compatible with color descriptions (e.g., very bright green).
                  Default: the qualititive Brewer palette.

barwidth=         Width of bars.
                  Values must be in the 0-1 range.
                  Default: 0.25.
                  
yfmt=             Format for yvar/legend.
                  Default: values of yvar variable in original dataset.
                  Gotcha: user-defined formats must utilize converted yvar values 1-N.

xfmt=             Format for x-axis/time.
                  Default: values of xvar variable in original dataset.
                  Gotcha: user-defined formats must utilize converted xvar values 1-N.

legendtitle=      Text to use for legend title.
                     e.g., legendtitle=%quote(Response Value)

interpol=         Method of interpolating between bars.
                  Valid values are: cosine, linear.
                  Default: cosine.

stat=             Show percents or frequencies on y-axis.
                  Valid values: percent/freq.
                  Default: percent.
                  
datalabel=        Show percents or frequencies inside each bar.
                  Valid values: yes/no.
                  Default: yes.
                  Interaction: will display percents or frequences per stat=.
                  
plotname=         Name of PNG file
                  
debug=            Keep work datasets.
                  Valid values: yes/no.
                  Default: no.                  
                  
*---------- depricated parameters ----------;

percents=         Show percents inside each bar.
                  This has been replaced by datalabel=. 

-------------------------------------------------------------------------------------------------*/


%macro sankeybarchart
   (data=
   ,subject=
   ,yvar=
   ,xvar=
   ,completecases=yes
   ,missflag=
   ,yvarord=
   ,xvarord=
   ,colorlist=
   ,barwidth=0.25
   ,yfmt=
   ,xfmt=
   ,legendtitle=
   ,interpol=cosine
   ,stat=percent
   ,datalabel=yes
   ,debug=no
   ,percents=
   ,plotname=sankey
   );
   

   %*---------- first inner macro ----------;

   %if &data eq %str() or &subject eq %str() or &yvar eq %str() or &xvar eq %str() %then %do;
      %put %str(W)ARNING: SankeyBarChart -> AT LEAST ONE REQUIRED PARAMETER IS MISSING.;
      %put %str(W)ARNING: SankeyBarChart -> THE MACRO WILL STOP EXECUTING.;
      %return;
   %end;

   %rawtosankey
      (data=&data
      ,subject=&subject
      ,yvar=&yvar
      ,xvar=&xvar
      %if &completecases ne %then 
         ,completecases=&completecases;
      %if &missflag ne %then 
         ,missflag=&missflag;
      %if &yvarord ne %then 
         ,yvarord=&yvarord;
      %if &xvarord ne %then 
         ,xvarord=&xvarord;
      );


   %*---------- second inner macro ----------;

   %if &rts = 1 %then %do;
   
      %sankey
         (barwidth=&barwidth
         ,interpol=&interpol
         ,stat=&stat
         ,datalabel=&datalabel
         %if &colorlist ne %then 
            ,colorlist=&colorlist;
         %if &yfmt ne %then 
            ,yfmt=&yfmt;
         %if &xfmt ne %then 
            ,xfmt=&xfmt;
         %if &legendtitle ne %then 
            ,legendtitle=&legendtitle;
         %if &percents ne %then 
            ,percents=&percents;
         );
      
   %end;

%mend;

proc format;

value agecat
    1 = '<= 40'
    2 = '>40-55'
    3 = '>55'
    ;

value bmicat
    1 = 'ideal'
    2 = 'overweight'
    3 = 'obese'
    ;

value treat
   1 = 'Active'
   2 = 'Placebo'
   ;

run;

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

     if  .< bmi<25 then bmicat=1;
     if 25<=bmi<30 then bmicat=2;
     if 30<bmi     then bmicat=3;

     if .<age<40   then agecat=1;
     if 40<age<=55 then agecat=2;
     if 55<age     then agecat=3;

subject=1000+_N_;

run;

data tmp2(drop=pain0-pain10);
     set tmp1;
     array a{11} pain0-pain10;
     do i = 1 to 11;
        yval= a[i];
        week=i-1;
        output;
     end;
run;

data tmp3;
     set tmp2;
     if  0<=yval<=11 then ycat=1;
     if 12<=yval<=22 then ycat=2;
     if 23<=yval<=33 then ycat=3;
     if 34<=yval<=44 then ycat=4;
     if 45<=yval<=55 then ycat=5;
     if 56<=yval<=66 then ycat=6;
run;

proc sort data=tmp3;
by subject week;
run;

data tmp3b;
     set tmp3(where=(yval ne .));
run;

ods listing close;

proc univariate data=tmp3b;
by subject;
var week;
output out=desc max=weekmax;
run;

ods listing;

data dummy2;
     merge tmp3(in=c)
           desc(in=d);
     by subject;
     if ycat eq . then do;
        if week le weekmax then ycat=11;
        if week gt weekmax then ycat=12;
     end;
     if week in (0 2 4 6 8 10);
run;

data dummy2_act
     dummy2_pbo;
     set dummy2;
     if treat eq 1 then output dummy2_act;
     if treat eq 2 then output dummy2_pbo;
run;

data dummy3;
     set dummy2;
     if ycat ge 10 then ycat=.;
run;

/*
%sankeybarchart
   (data=dummy
   ,subject=subject
   ,yvar=biomarker
   ,xvar=visit
   );

*------------------------------------------------------------;
*---------- adding yvarord ----------;
*------------------------------------------------------------;

%sankeybarchart
   (data=dummy
   ,subject=subject
   ,yvar=biomarker
   ,xvar=visit
   ,yvarord=%quote(0, 1, 2, 3)
   );

*------------------------------------------------------------;
*---------- more optional parameters for aesthetics ----------;
*------------------------------------------------------------;

*/

proc format;
   value xfmt
   1 = 'Baseline'
   2 = 'Week 2'
   3 = 'Week 4'
   4 = 'Week 6'
   5 = 'Week 8'
   6 = 'Week 10'
   ;

   value yfmt
   1 = '1-11'
   2 = '12-22'
   3 = '23-33'
   4 = '34-44'
   5 = '44-55'
   6 = '56-66'
   7 = 'Missed visit'
   8 = 'No subsequent visit'
   ;

   value yfmtb
   1 = '1-11'
   2 = '12-22'
   3 = '23-33'
   4 = '34-44'
   5 = '44-55'
   6 = '56-66'
   ;
run;

title1 'Treatment: Active';

%sankeybarchart
   (data=dummy2_act
   ,colorlist=CX2166AC CX67A9CF CXD1E5F0 CXFDDBC7 CXEF8A62 CXB2182B CXE6F598 CXABDDA4
   ,subject=subject
   ,yvar=ycat
   ,xvar=week
   ,yvarord=%quote(1, 2, 3, 4, 5, 6, 11, 12)
   ,barwidth=0.45
   ,xfmt=xfmt.
   ,yfmt=yfmt.
   ,legendtitle=%quote(Pain)
   ,plotname=missing_sankey_act
   );


title1 'Treatment: Placebo';

%sankeybarchart
   (data=dummy2_pbo
   ,colorlist=CX2166AC CX67A9CF CXD1E5F0 CXFDDBC7 CXEF8A62 CXB2182B CXE6F598 CXABDDA4
   ,subject=subject
   ,yvar=ycat
   ,xvar=week
   ,yvarord=%quote(1, 2, 3, 4, 5, 6, 11, 12)
   ,barwidth=0.45
   ,xfmt=xfmt.
   ,yfmt=yfmt.
   ,legendtitle=%quote(Pain)
   ,plotname=missing_sankey_pbo
   );

/*
title1 'Sankey plot III - Missings categorised';
title2 'Percentage axis/no value within each bar';
%sankeybarchart
   (data=dummy2
   ,colorlist=CX2166AC CX67A9CF CXD1E5F0 CXFDDBC7 CXEF8A62 CXB2182B CXE6F598 CXABDDA4
   ,subject=subject
   ,yvar=ycat
   ,xvar=week
   ,yvarord=%quote(1, 2, 3, 4, 5, 6, 11, 12)
   ,barwidth=0.45
   ,xfmt=xfmt.
   ,yfmt=yfmt.
   ,datalabel=no
   ,legendtitle=%quote(Pain)
   ,plotname=missing_sankey_III
   );

title1 'Sankey plot IV - Missings categorised';
title2 'Frequency axis/value within each bar';
%sankeybarchart
   (data=dummy2
   ,colorlist=CX2166AC CX67A9CF CXD1E5F0 CXFDDBC7 CXEF8A62 CXB2182B CXE6F598 CXABDDA4
   ,subject=subject
   ,yvar=ycat
   ,xvar=week
   ,yvarord=%quote(1, 2, 3, 4, 5, 6, 11, 12)
   ,barwidth=0.45
   ,xfmt=xfmt.
   ,yfmt=yfmt.
   ,stat=freq
   ,datalabel=yes
   ,legendtitle=%quote(Pain)
   ,plotname=missing_sankey_IV
   );

title1 'Sankey plot V - Missings categorised';
title2 'Frequency axis/no value within each bar';
%sankeybarchart
   (data=dummy2
   ,colorlist=CX2166AC CX67A9CF CXD1E5F0 CXFDDBC7 CXEF8A62 CXB2182B CXE6F598 CXABDDA4
   ,subject=subject
   ,yvar=ycat
   ,xvar=week
   ,yvarord=%quote(1, 2, 3, 4, 5, 6, 11, 12)
   ,barwidth=0.45
   ,xfmt=xfmt.
   ,yfmt=yfmt.
   ,stat=freq
   ,datalabel=no
   ,legendtitle=%quote(Pain)
   ,plotname=missing_sankey_V
   );

title1 'Sankey plot VI - Missings not included';
title2 'Percentage axis/value within each bar';
%sankeybarchart
   (data=dummy3
   ,colorlist=CX2166AC CX67A9CF CXD1E5F0 CXFDDBC7 CXEF8A62 CXB2182B
   ,subject=subject
   ,completecases=no
   ,missflag=yes
   ,yvar=ycat
   ,xvar=week
   ,yvarord=%quote(1, 2, 3, 4, 5, 6)
   ,barwidth=0.45
   ,xfmt=xfmt.
   ,yfmt=yfmtb.
   ,legendtitle=%quote(Pain)
   ,plotname=missing_sankey_VI
   );

title1 'Sankey plot VII - Missings not included';
title2 'Percentage axis/no value within each bar';
%sankeybarchart
   (data=dummy3
   ,colorlist=CX2166AC CX67A9CF CXD1E5F0 CXFDDBC7 CXEF8A62 CXB2182B
   ,subject=subject
   ,completecases=no
   ,missflag=yes
   ,yvar=ycat
   ,xvar=week
   ,yvarord=%quote(1, 2, 3, 4, 5, 6)
   ,barwidth=0.45
   ,xfmt=xfmt.
   ,yfmt=yfmtb.
   ,datalabel=no
   ,legendtitle=%quote(Pain)
   ,plotname=missing_sankey_VII
   );

title1 'Sankey plot VIII - Missings not included';
title2 'Frequency axis/value within each bar';
%sankeybarchart
   (data=dummy3
   ,colorlist=CX2166AC CX67A9CF CXD1E5F0 CXFDDBC7 CXEF8A62 CXB2182B
   ,subject=subject
   ,completecases=no
   ,missflag=yes
   ,yvar=ycat
   ,xvar=week
   ,yvarord=%quote(1, 2, 3, 4, 5, 6)
   ,barwidth=0.45
   ,xfmt=xfmt.
   ,yfmt=yfmtb.
   ,stat=freq
   ,datalabel=yes
   ,legendtitle=%quote(Pain)
   ,plotname=missing_sankey_VIII
   );

title1 'Sankey plot IX - Missings not included';
title2 'Frequency axis/no value within each bar';
%sankeybarchart
   (data=dummy3
   ,colorlist=CX2166AC CX67A9CF CXD1E5F0 CXFDDBC7 CXEF8A62 CXB2182B
   ,subject=subject
   ,completecases=no
   ,missflag=yes
   ,yvar=ycat
   ,xvar=week
   ,yvarord=%quote(1, 2, 3, 4, 5, 6)
   ,barwidth=0.45
   ,xfmt=xfmt.
   ,yfmt=yfmtb.
   ,stat=freq
   ,datalabel=no
   ,legendtitle=%quote(Pain)
   ,plotname=missing_sankey_IX
   );
*/




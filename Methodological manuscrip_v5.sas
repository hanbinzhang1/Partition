libname methodo 'C:\Users\localuser\OneDrive - Imperial College London\SASUniversityEdition\folders\myfolders\Methodological manuscript';
*--The main objective of this piece of code was to 
combine summer and winter codes together, in other words, to incoporate
the small-increase/decrease peak censoring in the algorithm--*;

data methodo.AIRLESS;
/* 	infile 'C:\Users\localuser\OneDrive - Imperial College London*/
/*\SASUniversityEdition\folders\myfolders\RO_PM_outdoor.csv' dlm=',' FIRSTOBS=2;  */
/*	infile 'C:\Users\localuser\OneDrive - Imperial College London*/
/*\SASUniversityEdition\folders\myfolders\RO_PM_Ambient.csv' dlm=',' FIRSTOBS=2; */
	infile 'C:\Users\localuser\OneDrive - Imperial College London/
\SASUniversityEdition\folders\myfolders\Residential_PM.csv' dlm=',' FIRSTOBS=2; 
	input home $ season $ In_pm25 Out_pm25 mion hour day month year min;
run;

data methodo.ro;
	set methodo.AIRLESS;
	date=mdy(month,day,year);
	time=hms(hour,min,0);
	datetime = dhms(date,hour,min,0);
	mydatetime = round(datetime,60*15);
	format date mmddyy. time time. datetime datetime. mydatetime datetime.;
	drop month day year hour min;
/*	if season = "Summer" then delete;*/
run;

proc sort data = methodo.ro; by home mydatetime season; run;

proc summary data = methodo.ro; by home mydatetime season;
var In_pm25 Out_pm25;
output out = methodo.thirty mean = in out;
run;

data methodo.new;
	set methodo.thirty;
	if _FREQ_ > 7;
	drop _TYPE_ _FREQ_;
	if in = . or out = . then delete;
	if in < 0 or out < 0 then delete;
	home_season = (trim(home))||"_"||season;
run;

proc sort data = methodo.new; by home_season mydatetime; run;

proc export data=methodo.new
    outfile='C:\Users\hzhang6\OneDrive - Imperial College London\R projects\COPE\export-csv\AIRLESS_RO_15min_sas_to_r.csv'
    dbms=csv
    replace;
run;

data methodo.censor; 
	set methodo.new; by home_season;

	mytime = timepart(mydatetime);
	format mytime time.;

	if mytime - lag(mytime) = (60*15) and home_season = lag(home_season) then no_gap = 1;
	if mytime = hms(0,0,0) and lag(mytime) = hms(23,45,0) and home_season = lag(home_season) 
	then no_gap = 1;
	
	*--sort out the preparation for lag and lead--*;
	
	prev_in = lag(in);
	prev_out = lag(out);
    prev2_out = lag2(out); 
    
	if eof1=0 then
    set methodo.new (firstobs=2 keep=in rename=(in=in_lead1)) end=eof1;
    else in_lead1=.;
    if eof1=0 then
    set methodo.new (firstobs=2 keep=out rename=(out=out_lead1)) end=eof1;
    else out_lead1=.;
    
    if eof2=0 then
    set methodo.new (firstobs=3 keep=in rename=(in=in_lead2)) end=eof2;
    else in_lead2=.;
    if eof2=0 then
    set methodo.new (firstobs=3 keep=out rename=(out=out_lead2)) end=eof2;
    else out_lead2=.;  
    
    if eof3=0 then
    set methodo.new (firstobs=4 keep=in rename=(in=in_lead3)) end=eof3;
    else in_lead3=.;
    if eof3=0 then
    set methodo.new (firstobs=4 keep=out rename=(out=out_lead3)) end=eof3;
    else out_lead3=.;  
    
    if eof4=0 then
    set methodo.new (firstobs=5 keep=in rename=(in=in_lead4)) end=eof4;
    else in_lead4=.;
    if eof3=0 then
    set methodo.new (firstobs=5 keep=out rename=(out=out_lead4)) end=eof4;
    else out_lead4=.;        

	if no_gap~=1 then prev_in = . and prev_out = . and in_lead1 = .
	and out_lead1 = . and in_lead2 = . and out_lead2 = . and
	in_lead3 = . and out_lead3 = . and in_lead4 = . and out_lead4 = .;   
	
	if first.home_season=1 then prev_in = . and prev_out = .; 
	
	if last.home_season=1 then in_lead1 = . and out_lead1 = . and in_lead2 = . and out_lead2 = . 
	and in_lead3 = . and out_lead3 = . and in_lead4 = . and out_lead4 = .; 
	
	if lag(last.home_season)=1 then in_lead2 = . and out_lead2 = . and
	in_lead3 = . and out_lead3 = . and in_lead4 = . and out_lead4 = .; 
	
	if lag2(last.home_season)=1 then in_lead3 = . and out_lead3 = . and 
	in_lead4 = . and out_lead4 = .; 
	
	if eof1=0 then
    set methodo.new (firstobs=2 keep=home_season rename=(home_season=home_season_lead1)) end=eof1;
    else home_season_lead1=.;
    
    if eof2=0 then
    set methodo.new (firstobs=3 keep=home_season rename=(home_season=home_season_lead2)) end=eof2;
    else home_season_lead2=.; 
    
    if eof2=0 then
    set methodo.new (firstobs=4 keep=home_season rename=(home_season=home_season_lead3)) end=eof2;
    else home_season_lead3=.;
    
    if eof2=0 then
    set methodo.new (firstobs=5 keep=home_season rename=(home_season=home_season_lead4)) end=eof2;
    else home_season_lead4=.;
	
*------------------correct time lag effect--------------------;	

	if first.home_season=1 then prev_in = .;
	if first.home_season=1 then prev_out = .; 
	if first.home_season=1 then prev2_out = .;
	
	if last.home_season=1 then in_lead1 = .;
	if last.home_season=1 then out_lead1 = .; 
	if last.home_season=1 then out_lead2 = .;
	if last.home_season=1 then out_lead3 = .;  
	if last.home_season=1 then out_lead4 = .; 

	if home_season_lead2 ne home_season then out_lead2 = .;
	if home_season_lead2 ne home_season then out_lead3 = .;  
	if home_season_lead2 ne home_season then out_lead4 = .; 
    if home_season_lead3 ne home_season then out_lead3 = .;
    if home_season_lead3 ne home_season then out_lead4 = .;
    if home_season_lead4 ne home_season then out_lead4 = .;  

*-----------only correct lag effect when plotting ambient-------------------;		
	if home_season = 'R008_Summer' then out = prev_out; 
	if home_season = 'R161_Winter' then out = out_lead4;
	if home_season = 'R162_Winter' then out = out_lead2;
	if home_season = 'U010_Summer' then out = out_lead2; 	
	if home_season = 'U097_Summer' then out = out_lead3; 
    if home_season = 'U101_Winter' then out = out_lead4;
    if home_season = 'U107_Summer' then out = out_lead1;
/*     if home_season = 'U117_Summer' then out = out_lead3; */
    if home_season = 'U163_Summer' then out = out_lead3;
    if home_season = 'U170_Winter' then out = out_lead2;   
	
    *------------------The original method (just for plot)--------------------;
	if in>(1.5*prev_in) & in-prev_in> 4 & out<=(1.5*prev_out) then original = 1;
	if no_gap = . or last.home_season=1 then original =.;
	
    *------------------identify the "rising edge" of large peaks--------------------;
	if in>(1.5*prev_in) & in-prev_in> 4 then injump=1;
	if out>(1.5*prev_out) and in<out then outjump = 1;	
		
    *--------------classify the "continue rising" points after large peaks----------------*;	
	retain injump2; 
	if injump = 1 and outjump = . then injump2 = 1;
	if in < prev_in and no_gap = 1 then injump2 =.;
	if no_gap = . or last.home_season=1 then injump2 =.;
	
	if injump = 1 or injump2 = 1 then injump_either = 1;
	if injump_either = 1 & outjump = . then rising_edge = 1;
	if no_gap = . or last.home_season=1 then rising_edge = .;

	*------------------identify the "falling edge" of large peaks--------------------;
	if in<(0.8*prev_in) then infall=1; 
	if out<(0.8*prev_out) and in<out then outfall = 1; 

	*-------------classify the "continue falling" points after large peaks--------------;	
	retain infall2; 
	if infall = 1 and outfall = . then infall2 = 1;
	if prev_in < in and in-out<0 then infall2 = .;
	if no_gap = . or last.home_season=1 then infall2 =.;
	   
	if infall = 1 or infall2 = 1 then infall_either = 1;
	if infall_either = 1 & outfall = . then falling_edge = 1;
	if no_gap = . or last.home_season=1 then falling_edge =.;
	
	*-------------------Add absolute threshold---------------------*;
    if (in_lead1-out_lead1>13.5 and in_lead2-out_lead2>9 and in_lead3-out_lead3>6
    and in-out>0) or (lag(in)-lag(out)>13.5 and lag2(in)-lag2(out)>9 and lag3(in)-lag3(out)>6) 
    and in-out>0 then absolute = 1; 
    if no_gap = . or last.home_season=1 then absolute =.;
	
	*--------------taking out the lag effect in rising_edge---------------;		
	if rising_edge = 1 & in<out & (prev_out/lag2(out) >1.25 or lag2(out)/lag3(out) >1.25 
	or lag3(out)/lag4(out) > 1.25)
	then rising_edge = .;
	
	*--------------define event for falling edge misclassification correction
	here abosolute was regarded to be error-free hence no correction needed---------------;	
	if rising_edge = 1 or absolute = 1 then event = 1; 
	if no_gap = . or last.home_season=1 then event =.;	
	
	*--------------taking out the lag effect on the falling_edge---------------;	
	if falling_edge = 1 & in<1.5*out     
	and lag3(event) ^=1 and lag4(event) ^=1 and lag5(event) ^=1 
	and lag6(event) ^=1 and lag7(event) ^=1 and lag8(event) ^=1 
	and lag9(event) ^=1 and lag10(event) ^=1
	then not_falling_edge = 1;
	if no_gap = . or last.home_season = 1 then not_falling_edge = .;
	
	*------------------excluding/identifying the lagged out-generated indoor peaks-------------;
	if (lag5(out)>1.5*lag5(in) or lag4(out)>1.5*lag4(in) or lag3(out)>1.5*lag3(in) 
	or lag2(out)>1.5*lag2(in) or prev_out>1.5*prev_in) and falling_edge=1 
	and -0.5<((in_lead1/out_lead1)-(in/out))<0.5 
	and -0.5<((in_lead2/out_lead2)-(in_lead1/out_lead1))<0.5
	and in-out>0 then false_positive=1;
	if no_gap = . or last.home_season = 1 then false_positive =.;
	
	*---classify the "continue lagging" points after identified out-generated indoor peaks---;	
	retain false_positive2; 
	if false_positive = 1 then false_positive2 = 1;
	if prev_in < in then false_positive2 = .;
	if no_gap = . or last.home_season = 1 then false_positive2 =.;
	     
    *----censor the selected data -----;
	if original = 1 or rising_edge = 1  or falling_edge = 1 
	or absolute = 1 then censor = 1;	
    
	*----solve misclassification here for different plot purposes -----;
    if false_positive =1 or 
    false_positive2 = 1 or not_falling_edge = 1 then censor =.;
    
    *----Get rid of unknown ---;
    if censor ne 1 and in-out>0 then unknown = 1; 
    if censor ne 1 and unknown ne 1 and in > 300 then influence = 1; 
    *--could add: for each home_season, unknown > Q3+IQR then unknown = . and censor = 1 ---;   

*keep home_season mytime mydatetime in out amb censor;
run;

****when using residential outdoor;
/* data check; */
/* 	set censor; */
/* 	if censor = 1 then indoor_origin = in; */
/* 	if unknown = 1 then unknown = 1; */
/* run; */

****when using ambient;
data methodo.check;
	set methodo.censor;
	by home_season mydatetime; 
	if original = 1 then original_method = in;
	if rising_edge = 1 and not_rising_edge = . then rising_edgept = in;
	if rising_edge = 1 and original ne 1 then new_rising = in;
	if absolute = 1 then absolutept = in;
	if falling_edge =1 and false_positive =. and false_positive2 = . 
    and not_falling_edge = . then true_falling = 1;
	if absolute = 1 and original ne 1 and rising_edge ne 1 and
	falling_edge ne 1 and true_falling ne 1 then new_absolute = in;
	if falling_edge = 1 then falling_edgept = in;
	if falling_edge = 1 and not_falling_edge = . and false_positive = .
	and false_positive2 = . then new_falling = in;
	if false_positive = 1 then false_positivept = in; 
	if false_positive2 = 1 then false_positive2pt = in;
	if not_rising_edge = 1 then not_rising_edgept = in; 
	if not_falling_edge = 1 then not_falling_edgept = in;
	if unknown = 1 then UnOrigin = in;
run;

Data methodo.Inorigin (rename=(in=Inorigin out=out1))
     methodo.Outorigin(rename=(in=Outorigin out=out2));
     set methodo.censor;
     IF censor = '1' and unknown = '.' THEN OUTPUT methodo.Inorigin; 
        ELSE IF censor = '.' and unknown = '.' THEN OUTPUT methodo.Outorigin; 
RUN;

PROC TABULATE DATA = methodo.censor; 
   CLASS home_season;
   VAR in out;
   TABLE home_season ALL, (in Out) * (N MEAN STD); 
   TITLE 'Summary statistics'; 
ODS OUTPUT Table = methodo.tabout0;
RUN; 
PROC SORT DATA = methodo.tabout0; 
  BY home_season;
RUN;

PROC TABULATE DATA = methodo.Inorigin; 
   CLASS home_season;
   VAR Inorigin out1;
   TABLE home_season ALL, (Inorigin Out1) * (N MEAN STD); 
   TITLE 'Summary statistics'; 
ODS OUTPUT Table = methodo.tabout1;
RUN; 
PROC SORT DATA = methodo.tabout1; 
  BY home_season;
RUN;

PROC TABULATE DATA = methodo.Outorigin; 
   CLASS home_season;
   VAR Outorigin out2;
   TABLE home_season ALL, (Outorigin Out2) * (N MEAN STD); 
   TITLE 'Summary statistics'; 
ODS OUTPUT Table = methodo.tabout2;
RUN; 
PROC SORT DATA = methodo.tabout2; 
  BY home_season;
RUN;

Data methodo.Unknown(rename=(in=UnOrigin out=out3));
     set methodo.censor; 
     If unknown = '1' THEN OUTPUT methodo.Unknown;
Run; 

PROC TABULATE DATA = methodo.Unknown; 
   CLASS home_season;
   VAR UnOrigin out3;
   TABLE home_season ALL, (UnOrigin Out3) * (N MEAN STD); 
   TITLE 'Summary statistics'; 
ODS OUTPUT Table = methodo.tabout5;
RUN; 
PROC SORT DATA = methodo.tabout5; 
  BY home_season;
RUN;

Data methodo.Influence(rename=(in=InfOrigin out=out4));
     set methodo.censor; 
     IF Influence = '1' THEN OUTPUT methodo.Influence;
Run; 

*only run this if there are influential data points;
/* PROC TABULATE DATA = Influence;  */
/*    CLASS home_season; */
/*    VAR InfOrigin out4; */
/*    TABLE home_season ALL, (InfOrigin Out4) * (N MEAN STD);  */
/*    TITLE 'Summary statistics';  */
/* ODS OUTPUT Table = tabout6; */
/* RUN;  */
/*  */
/* PROC SORT DATA = tabout6;  */
/*   BY home_season; */
/* RUN; */
ODS PDF FILE = 'C:\Users\localuser\OneDrive - Imperial College London/
\SASUniversityEdition\folders\myfolders\Series.pdf';
proc sgplot data = methodo.check; by home_season;
    SERIES X = mydatetime Y = in / lineattrs=(color=green);
    SERIES X = mydatetime Y = out / lineattrs=(color=brown); 
    *SERIES X = mydatetime Y = amb / lineattrs=(color=black);
    SCATTER X = mydatetime Y = original_method 
    / MARKERATTRS= (symbol=circle color=black size = 6pt);
    SCATTER X = mydatetime Y = new_rising 
    / MARKERATTRS= (symbol=circlefilled color=red size = 6pt);
    SCATTER X = mydatetime Y = new_falling 
    / MARKERATTRS= (symbol=circlefilled color=blue size = 6pt);
    SCATTER X = mydatetime Y = new_absolute 
    / MARKERATTRS= (symbol=circlefilled color=purple size = 6pt);
    SCATTER X = mydatetime Y = UnOrigin/legendlabel= "unknown_origin" 
    MARKERATTRS= (symbol=circlefilled color=yellow size = 4pt);
    XAXIS LABEL = ' ' labelattrs=(size = 14)valueattrs = (size=20pt);
    YAXIS LABEL ="PM2.5 (ug/m3)";
    YAXIS LABEL ="PM2.5 (ug/m3)" VALUES = (0 TO 80 BY 20)valueattrs = (size=20pt);
    TITLE ' ';
    run; 


/* proc sgplot data = check; by home_season; */
/*     SERIES X = mydatetime Y = in / lineattrs=(color=green); */
/*     SERIES X = mydatetime Y = out / lineattrs=(color=brown);  */
/*     *SERIES X = mydatetime Y = amb / lineattrs=(color=black); */
/*     SCATTER X = mydatetime Y = originalpt 
/ MARKERATTRS= (symbol=circle color=yellow size = 10pt); */
/*     SCATTER X = mydatetime Y = rising_edgept 
/ MARKERATTRS= (symbol=circlefilled color=black size = 4pt); */
/*     SCATTER X = mydatetime Y = absolutept 
/ MARKERATTRS= (symbol=ciclefilled color=purple size = 5pt); */
/*     SCATTER X = mydatetime Y = falling_edgept 
/ MARKERATTRS= (symbol=circle color=black size = 6pt); */
/*     SCATTER X = mydatetime Y = not_rising_edgept 
/ MARKERATTRS= (symbol=square color=purple size = 8pt); */
/*     SCATTER X = mydatetime Y = not_falling_edgept 
/ MARKERATTRS= (symbol=square color=red size = 10pt); */
/*     SCATTER X = mydatetime Y = false_positivept 
/ MARKERATTRS= (symbol=diamond color=green size = 8pt); */
/*     SCATTER X = mydatetime Y = false_positive2pt 
/ MARKERATTRS= (symbol=triangle color=green size = 8pt); */
/*     SCATTER X = mydatetime Y = UnOrigin/legendlabel= "unknown_origin"  */
/*     MARKERATTRS= (symbol=circlefilled color=blue size = 3pt); */
/*     XAXIS LABEL = ' '; */
/*     YAXIS LABEL ="PM2.5 (µg/m3)"; */
/*     TITLE ' '; */
/*     run;  */

data methodo.calculate;
	set methodo.censor;
	if in =. or out = . then delete;

	if mydatetime - lag(mydatetime) = (60*15) and home_season = lag(home_season) then no_gap = 1;
	if mytime = hms(0,0,0) and lag(mytime) = hms(23,45,0) and home_season = lag(home_season) 
	then no_gap = 1;

	prev_in = lag(in);
	if no_gap~=1 then prev_in = .;

	if censor = 1 then do;
		prev_in = .;
	end;
	
    if unknown = 1 then do;
	   prev_in = .; 
	end;
	
	if influence = 1 then do; 
	   prev_in = .;
	end;
	
	*keep home_season in out prev_in mydatetime day delta_in no_gap;
run;

proc reg data = methodo.calculate tableout outest=methodo.results edf covout noprint; by home_season;
model in=out prev_in;
restrict intercept=0;
output out=methodo.predictions pred=yhat;
run;quit;

data methodo.calculations2;
	set methodo.results;
	if _TYPE_ ="PARMS";
	a1 = out;
	a2 = prev_in;
	N = _EDF_ + 2;
	Finf = a1/(1-a2);
	if Finf >1 then Finf = 1;
	if Finf <0 then Finf =0; 
 	*keep home_season Finf N;
run;
proc sort data = methodo.calculations2; by home_season; run;
proc print data = methodo.calculations2; run;

Data methodo.deltain; 
  Merge methodo.Inorigin methodo.calculations2;
  BY home_season;
  delta_in = inorigin-Finf*out1;
  if delta_in < 0 then delta_in = 0; 
  if delta_in = . then delta_in = 0; 
  keep home_season delta_in;
RUN;
PROC SORT DATA = methodo.deltain; 
  BY home_season;
RUN;

PROC TABULATE DATA = methodo.deltain; 
   CLASS home_season;
   VAR delta_in;
   TABLE home_season ALL, delta_in * (N MEAN STD); 
   TITLE 'Delta_in summary'; 
ODS OUTPUT Table = methodo.Deltain_out;
RUN; 
PROC SORT DATA = methodo.deltain_out; 
  BY home_season;
RUN;

Data methodo.deltaUnknown; 
  Merge methodo.Unknown methodo.calculations2;
  BY home_season;
  delta_unorigin = UnOrigin-Finf*out3;
  *if delta_unknown < 0 then delta_unknown = 0; 
  *if delta_unknown = . then delta_unknown = 0; 
  keep home_season delta_unorigin;
RUN;
PROC SORT DATA = methodo.deltaUnknown; 
  BY home_season;
RUN;

PROC TABULATE DATA = methodo.deltaUnknown; 
   CLASS home_season;
   VAR delta_unorigin;
   TABLE home_season ALL, delta_unorigin * (N MEAN STD); 
   TITLE 'Delta_in summary'; 
ODS OUTPUT Table = methodo.delta_unorigin_out;
RUN; 
PROC SORT DATA = methodo.delta_unorigin_out; 
  BY home_season;
RUN;

Data methodo.finalsummary;
/*   MERGE tabout0 tabout1 tabout2 calculations2 deltain_out tabout5 tabout6 delta_unorigin_out; */
  MERGE methodo.tabout0 methodo.tabout1 methodo.tabout2 
methodo.calculations2 methodo.deltain_out methodo.tabout5 
methodo.delta_unorigin_out;
  BY home_season; 
  if InfOrigin_N ne . then Percent = (Outorigin_N-InfOrigin_N)/in_N;
  if InfOrigin_N = . then Percent = Outorigin_N/in_N;
  Totalhrs = in_N/4;
  if Totalhrs < 43 then Finf = .;
  if Totalhrs < 43 then delete;
  Indoor_generated = delta_in_mean*delta_in_N/in_N;
  UnOrigin = delta_unorigin_mean*delta_unorigin_N/in_N;
  if Unorigin =. or UnOrigin <0 then UnOrigin=0;
  if Unorigin_N = . then Unorigin_N = 0;
  IndTime_percent = delta_in_N/In_N;
  OGtime_percent = Outorigin_N/in_N;
  UNtime_percent = Unorigin_N/in_N;
  outdoor_generated = Out_Mean*Finf;
  In_modelled = outdoor_generated + Indoor_generated + UnOrigin;
/*   IG_percent = Indoor_generated/In_mean; */
/*   OG_percent = outdoor_generated/In_mean; */
/*   UN_percent = Unorigin/In_mean; */
  IG_percent = Indoor_generated/In_mean;
  OG_percent = outdoor_generated/In_mean;
  UN_percent = Unorigin/In_mean;
  *outdoor_generated = outorigin_Mean*outorigin_n/in_N;
  *if percent <0.2 then Finf = .; 
  *if Indoor_generated =. or Indoor_generated <0 then Indoor_generated=0; 
  if IG_percent =. then IG_percent =0;
  if IG_percent <0 then IG_percent =0;
  if IndTime_Percent =. then IndTime_Percent=0;
  In_known = outdoor_generated + Indoor_generated;
RUN;
PROC SORT DATA = methodo.finalsummary; 
  BY home_season;
RUN;

PROC TABULATE format=6. DATA = methodo.finalsummary;
   CLASS home_season;
   VAR Totalhrs In_mean In_modelled In_known Finf indoor_generated outdoor_generated 
   UnOrigin IndTime_Percent IG_percent OG_percent UN_percent Percent 
   Ogtime_percent Untime_percent;
   TABLE home_season ALL = 'ALL (mean unweighted)', 
   (Totalhrs='Total measured hours')*mean*f=6.1  
   (In_mean='Total measured indoor exposure')*mean*f=6.1
   (In_modelled='Total modelled indoor exposure')*mean*f=6.1 
   (In_known='Total modelled known indoor exposure')*mean*f=6.1
   (Finf='Infiltration efficiency')*mean*f=6.2  
   (Percent='Percent of data points for calulating Finf')*mean*f=6.2
   (OG_percent='Percent of hours of og')*mean*f=6.2
   (UN_percent='Percent of hours of un')*mean*f=6.2 
   (indoor_generated='Indoor-generated exposure(ig)')*mean*f=6.1
   (outdoor_generated='Outdoor-generated exposure(og)')*mean*f=6.1 
   (unOrigin='Unknown source of exposure')*mean*f=6.1 
   (IndTime_percent='Percent of hours of ig')*mean*f=6.2 
   (IG_percent='Percent of concentration of ig')*mean*f=6.2 
   (OG_percent='Percent of concentration of og')*mean*f=6.2
   (UN_percent='Percent of concentration of un')*mean*f=6.2; 
   TITLE 'Full classifying algorithm output'; 
RUN; 

/*PROC TABULATE format=6. DATA = finalsummary;*/
/*   CLASS home_season;*/
/*   VAR indoor_generated outdoor_generated */
/*   UnOrigin IndTime_Percent IG_percent OG_percent UN_percent Percent */
/*   Ogtime_percent Untime_percent;*/
/*   TABLE home_season ALL = 'ALL (mean unweighted)', */
/*   (indoor_generated='Indoor-generated exposure(ig)')*mean*f=6.1*/
/*   (IndTime_Percent='Percent of hours of ig')*mean*f=6.2 */
/*   (IG_percent='Percent of concentration of ig')*mean*f=6.2 */
/*   (outdoor_generated='Outdoor-generated exposure(og)')*mean*f=6.1 */
/*   (OGtime_percent='Percent of hours of og')*mean*f=6.2*/
/*   (OG_percent='Percent of concentration of og')*mean*f=6.2*/
/*   (unOrigin='Unknown source of exposure')*mean*f=6.1 */
/*   (UNtime_percent='Percent of hours of unknown')*mean*f=6.2*/
/*   (UN_percent='Percent of concentration of un')*mean*f=6.2; */
/*   TITLE 'Full classifying algorithm output'; */
/*RUN; */

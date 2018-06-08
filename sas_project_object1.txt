FILENAME REFFILE '/home/jglin0/sasuser.v94/Project/recs2009_public_vlad_clean.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.IMPORT;
	GETNAMES=YES;
RUN;

proc means data=import n nmiss mean std max min;
run;
proc means data=log_import;
var totaldol log_dollar totsqft;
run;


proc ttest data=import;
var totaldol;
run;

data log_import;
set import;
log_dollar = log(totaldol);
dollar_sqft = totaldol/totsqft;
log_dollar_sqft = log(dollar_sqft);
run;
proc ttest data=log_import;
var log_dollar;
run;
proc ttest data=log_import;
var dollar_sqft;
run;
proc ttest data=log_import;
var log_dollar_sqft;
run;

/* proc sgscatter data=log_import; */
/*   title "Total Dollar Cost Scatter Matrix"; */
/*   matrix log_dollar hdd65 cdd65 gnd_hdd65 ; */
/* run; */
/* proc sgscatter data=log_import; */
/*   title "Total Dollar Cost Scatter Matrix"; */
/*   matrix log_dollar tothsqft totusqft totcsqft totucsqft; */
/* run; */
/* proc sgscatter data=log_import; */
/*   title "Total Dollar Cost Scatter Matrix"; */
/*   matrix log_dollar kownrent rooftype temphome tempgone tempnite; */
/* run; */
/* proc sgscatter data=log_import; */
/*   title "Total Dollar Cost Scatter Matrix"; */
/*   matrix log_dollar useng uselp usefo usekero; */
/* run; */
/* proc sgscatter data=log_import; */
/*   title "Total Dollar Cost Scatter Matrix"; */
/*   matrix log_dollar aircond equipnoheat hdd30yr cdd30yr; */
/* run; */

proc glmselect data = log_import plots(stepaxis = number) = (criterionpanel ASEPlot) seed = 1;
class aircond equipnoheat athome;
partition fraction(test = .5); 
model log_dollar= totsqft hhage athome temphome tempgone tempnite totrooms yearmade aircond equipnoheat hdd65 cdd65 hdd30yr cdd30yr aircond*hdd30yr equipnoheat*cdd30yr
hdd65*aircond equipnoheat*cdd65 tothsqft totcsqft aircond*totcsqft equipnoheat*tothsqft  / selection = stepwise( choose=cv stop = none);
run;
proc glmselect data = log_import plots(stepaxis = number) = (criterionpanel ASEPlot) seed = 1;
class aircond equipnoheat athome;
partition fraction(test = .5); 
model log_dollar= totsqft hhage athome temphome tempgone tempnite totrooms yearmade aircond equipnoheat hdd65 cdd65 hdd30yr cdd30yr aircond*hdd30yr equipnoheat*cdd30yr
hdd65*aircond equipnoheat*cdd65 tothsqft totcsqft aircond*totcsqft equipnoheat*tothsqft  / selection = forward( choose=cv stop = none);
run;
proc glmselect data = log_import plots(stepaxis = number) = (criterionpanel ASEPlot) seed = 1;
class aircond equipnoheat athome;
partition fraction(test = .5); 
model log_dollar= totsqft hhage athome temphome tempgone tempnite totrooms yearmade aircond equipnoheat hdd65 cdd65 hdd30yr cdd30yr aircond*hdd30yr equipnoheat*cdd30yr
hdd65*aircond equipnoheat*cdd65 tothsqft totcsqft aircond*totcsqft equipnoheat*tothsqft  / selection = backward(choose=cv stop = none);
run;
proc glmselect data = log_import plots(stepaxis = number) = (criterionpanel ASEPlot) seed = 1;
class aircond equipnoheat athome;
partition fraction(test = .5); 
model log_dollar= totsqft hhage athome temphome tempgone tempnite totrooms yearmade aircond equipnoheat hdd65 cdd65 hdd30yr cdd30yr aircond*hdd30yr equipnoheat*cdd30yr
hdd65*aircond equipnoheat*cdd65 tothsqft totcsqft aircond*totcsqft equipnoheat*tothsqft  / selection = lasso( choose=cv stop = none) cvdetails;
run;

/*Final Model*/   
proc glm data=log_import plots=all plots(maxpoints=20000);
class athome aircond; 
model log_dollar =  totsqft hhage athome tempgone totrooms yearmade aircond hdd65 cdd30yr hdd30yr*aircond /solution;
run;  
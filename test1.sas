data tab;
/* proc fake */
    v=1;
    d=2;
datarun=3;
  output;
v=2;
d=exp(67);
output;
run;
proc print data=tab;
run;

proc logistic data=blob; run;
/* test */
* icici ;

proc iml;
x=2;
show x;
quit;

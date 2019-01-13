%macro envelope(data=, predict=, resid=, m=1, pzero=, plinear=, zlinear=, class_v=,offset=,
				dispersion=,scale=,family=Normal,sim=10,link=identity,zlink=logit,
				type=HN, quasi=F,off=F);
proc sql;
select COUNT(*) into :nobs from &data;
quit;
%do i=1 %to &sim;
/*simulating the response variable*/
data rnorm;
do j=1 to &nobs;
norm=rannorm(&i);
output;
end;
drop j;
run;
data &data; set &data; set rnorm;
resp=&predict.+&scale.*norm;
run;
/*Residual generating*/
ods select none;
proc genmod data=&data;
class &class_v;
model resp= &plinear / dist=normal
link=&link;
output out=&data STDRESDEV=deviance&i;
run;%end;
data dev; set &data;
keep deviance1 -- deviance&sim;
run;
proc iml;
use dev;
read all into e;
close;
use &data;
read all var{ &resid. } into r;
close;
n=nrow(e);
%if &type=HN %then %do;
r=abs(r);
do i=1 to &sim;
m=abs(e[,i]);
call sort(m);
e[,i]=m;
end;
p=(n+(1:n)-0.125)/(2*n+0.5);
%end; %else %if &type=N %then %do;
do i=1 to &sim;
m=e[,i];
call sort(m);
e[,i]=m;
end;
p=((1:n)-0.375)/(n+0.25);
%end;
q=quantile("Normal", p);
te=t(e);
k=j(n,3,0);
do i=1 to n;
h=te[,i];
k[i,1]=min(h);
k[i,2]=median(h);
k[i,3]=max(h);
end;
call sort(r);
base=r||t(q)||k;
*print base;
create envelope from base;
append from base;
quit;
data envelope1; set envelope;
if col1 < col3 or col1 > col5 then color=1;
else color=2;
run;
%if &type=HN %then %do;
ods select all;
proc sgplot data=envelope1 noautolegend;
title "Half-Normal plot com envelope simulado";
scatter y=COL1 x=COL2 / group=color markerattrs=(symbol=circlefilled size=6) ;
series y=COL3 x=COL2 / lineattrs=(color=black);
series y=COL4 x=COL2 / lineattrs=(color=black pattern=2);
series y=COL5 x=COL2 / lineattrs=(color=black);
xaxis label="Percentil da N(0,1)";
yaxis label="Res�duo Studentizado";
run;
quit;
%end; %else %if &type=N %then %do;
ods select all;
proc sgplot data=envelope1 noautolegend;
title "Normal plot com envelope simulado";
scatter y=COL1 x=COL2 / group=color markerattrs=(simbol=circle size=6);
series y=COL3 x=COL2 / lineattrs=(color=black);
series y=COL4 x=COL2 / lineattrs=(color=black pattern=2);
series y=COL5 x=COL2 / lineattrs=(color=black);
xaxis label="Percentil da N(0,1)";
yaxis label="Res�duo Studentizado";
run;quit;%end;%mend envelope;


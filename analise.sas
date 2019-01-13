proc delete data=_all_;run; *limpando diretório;
*incluindo macro do envelope;
%include "C:\Users\User\Dropbox\UEM\3° Série\Análise de Regressão\Scripts\envelope-normal.sas";
*incluindo macro que fornece todas regressões possiveis;
%include "C:\Users\User\Dropbox\UEM\3° Série\Análise de Regressão\Scripts\allreg.sas";
*importando banco de dados;
PROC IMPORT OUT= WORK.dados 
            DATAFILE= "C:\Users\User\Dropbox\UEM\3° Série\Análise de Regressão\Trabalho\dados peso.xls" 
            DBMS=EXCEL REPLACE;
     RANGE="plan1"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
	 label pesonasc = "Peso ao nascer (kg)";
RUN;
data dados;
	set dados;
	identificacao = _n_;
run;

************************************* ANÁLISE DESCRITIVA *********************************************;

ods listing image_dpi=300;
ods graphics on / reset = all height=8 in width=8 in border=off imagefmt=pdf imagename = "histograma" ;
*análise descritiva do comportamento da variável resposta (peso do recém nascido);
proc univariate data=dados noprint;
	histogram pesonasc / normal (color=blue) kernel (color=red)
						cfill = ltgray
						ctext = green
						vaxislabel = "Porcentagem"
						odstitle=" ";
	inset n = "N° obs." nmiss = "N° obs. faltantes" mean = "Média" median = "Mediana" mode="Moda"
	std = "Desvio Padrão" / position = ne;
run;
ods graphics off;

ods tagsets.tablesonlylatex file="covariaveis.tex" (notop nobot) newfile=table;
*breve descritiva das covariáveis;
proc tabulate data=dados;
	var _numeric_;
	table (_numeric_),(mean median std cv);
quit;
ods tagsets.tablesonlylatex close;

ods _all_ close; options printerpath=png nodate;title; 
ods printer file="C:\Users\User\Dropbox\UEM\3° Série\Análise de Regressão\Trabalho\SAS-Output\pearson.png" 
dpi=500 style=myAnalysis; ods exclude VarInformation SimpleStats; *Excluir outputs indesejados; 
options nolabel;title " "; 
*avaliando a correlação entre as covariaveis;
proc corr data=dados pearson out=fullcorr; 
	var diacabeca altura idadegest--numcigpai; 
run; 
ods printer close;

ods listing image_dpi=300;
ods graphics on / reset = all height=9 in width=10 in border=off imagefmt=pdf imagename = "matrix-plot" ;
*matrix plot - resposta em função das outras variáveis;
proc sgscatter data=dados;
  plot pesonasc * (diacabeca altura idadegest--numcigpai)  / loess reg columns=4 rows=4 
					filledoutlinedmarkers markeroutlineattrs=(color=black thickness=2)
  					markerattrs=(symbol=circlefilled size=10);
run;
ods graphics off;

************************************* AVALIANDO MODELO COMPLETO ***************************************;
***** MULTICOLINEARIEDADE;
ods tagsets.tablesonlylatex file="tablesonly.tex" (notop nobot);
proc reg data=dados;
	model pesonasc = diacabeca altura idadegest--numcigpai / vif tol;
	output out=dados1 p=predito student=residuo;
quit;
ods tagsets.tablesonlylatex close;
*Observamos que os maiores vif ocorrem na idade do pai e mãe, entretanto não são preocupante;

***** NORMALIDADE;
*Half-Normal plot com envelope simulado;
ods listing image_dpi=300;
ods graphics on / reset = all height=4 in width=6 in border=off imagefmt=pdf imagename = "halfnormal-plot";
%envelope(data=dados1, predict=predito, resid=residuo, plinear=diacabeca altura idadegest--numcigpai,
		  scale=291.1256, family=normal, link=identity, sim=100, type=HN);
ods graphics off;
*Normal plot com envelope simulado;
ods listing image_dpi=300;
ods graphics on / reset = all height=4 in width=6 in border=off imagefmt=pdf imagename = "normal-plot";
%envelope(data=dados1, predict=predito, resid=residuo, plinear=diacabeca altura idadegest--numcigpai,
		  scale=291.1256, family=normal, link=identity, sim=100, type=N);
ods graphics off;
*Testes de normalidade;
proc univariate data=dados1 normaltest;
	var residuo;
	ods output TestsForNormality=NormaliltyTest(keep=Test Stat pValue);
run;
ods tagsets.tablesonlylatex file="NormaliltyTest.tex" (notop nobot) newfile=table;
proc print data=NormaliltyTest noobs;run;
ods tagsets.tablesonlylatex close;

***** HOMOCEDASTICIDADE;
ods listing image_dpi=300;
ods graphics on / reset = all height=6 in width=8 in border=off imagefmt=pdf imagename = "residuovspredito";
*Método gráfico, plotar resíduo pelos valores preditos;
proc sgplot data=dados1;
	title " ";
	scatter x=predito y=residuo / markerattrs=(color=black symbol=circle size=8);
	refline 0 / axis=y lineattrs=(color=red pattern=shortdash thickness=2);
	refline 3 -3 / axis=y lineattrs=(color=black thickness=2);
	xaxis label="Valores preditos";
	yaxis label="Resíduo studentizado" values = (-4 to 4 by 1);
quit;
ods graphics off;
*Teste de white e breusch: hipótese nula de que as variâncias são iguais;
proc model data=dados;
	parms b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11;
	pesonasc = b0 + b1*diacabeca + b2*altura + b3*idadegest + b4*idademae + b5*numcigmae+
		b6*alturamae + b7*pesomae + b8*idadepai + b9*escpai + b10*numcigpai + b11*alturapai;
	fit pesonasc / white breusch=(1 diacabeca altura idadegest idademae numcigmae alturamae pesomae 
								idadepai escpai numcigpai alturapai);
quit;


*Avaliando as matrizes de covariancias consistentes;
proc reg data=dados;
	model pesonasc = diacabeca altura idadegest idademae numcigmae alturamae pesomae idadepai 
					 escpai numcigpai alturapai / hccmethod=0 white;
	model pesonasc = diacabeca altura idadegest idademae numcigmae alturamae pesomae idadepai 
					 escpai numcigpai alturapai / hccmethod=1 white;
	model pesonasc = diacabeca altura idadegest idademae numcigmae alturamae pesomae idadepai 
					 escpai numcigpai alturapai / hccmethod=2 white;
	model pesonasc = diacabeca altura idadegest idademae numcigmae alturamae pesomae idadepai 
					 escpai numcigpai alturapai / hccmethod=3 white;
	ods output  ParameterEstimates=est(keep=HCStdErr);
quit;
ods tagsets.tablesonlylatex file="hcc.tex" (notop nobot) newfile=table;
proc print data=est noobs;run;
ods tagsets.tablesonlylatex close;

************************************* SELEÇÃO DE VARIÁVEIS ***************************************;

*ajustando todos os possiveis modelos;
%allsubsreg(data=dados,
            depvar=pesonasc,
            indepvar=diacabeca idadegest idademae numcigmae alturamae pesomae idadepai escpai 
					 alturapai numcigpai,
            sortvar=_press_,
            printvar=_P_ _CP_ _PRESS_ _RMSE_ _MSE_ _RSQ_ _ADJRSQ_ _AIC_ _BIC_ _SBC_);

proc reg data=dados;
	model pesonasc = diacabeca idadegest idademae numcigmae alturamae pesomae idadepai escpai alturapai numcigpai / 
						selection=cp start=1 stop=11 adjrsq aic bic;
	ods output SubsetSelSummary=Selecao(drop=control); 
quit;

data selecao(drop=model dependent RSquare);
format ModelIndex NumInModel VarsInModel Cp Adjrsq aic bic;
set selecao;
run;

ods tagsets.tablesonlylatex file="selecao.tex" (notop nobot) newfile=table;
proc print data=selecao (obs=6) noobs; run;
ods tagsets.tablesonlylatex close;

*stepwise;
proc reg data=dados;
	model pesonasc = diacabeca altura idadegest--numcigpai / 
						selection=stepwise;
quit;

*testa se a variável altura é significativa pelo teste de wald ou lr;
proc genmod data=dados;
	model pesonasc = diacabeca idadegest numcigmae alturamae pesomae alturapai/ dist = normal ;
*	contrast "H0"  numcigmae 1 alturamae 1 / e wald;
quit;

********************************** AVALIAÇÃO DO MODELO FINAL *************************************;
ods tagsets.tablesonlylatex file="modelofinal.tex" (notop nobot);
proc reg data=dados;
	model pesonasc = diacabeca idadegest numcigmae alturamae pesomae alturapai / vif;
	output out=dados2 p=predito student=residuo;
quit;
ods tagsets.tablesonlylatex close;

***** NORMALIDADE;
*Half-Normal plot com envelope simulado;
ods listing image_dpi=300;
ods graphics on / reset = all height=4 in width=6 in border=off imagefmt=pdf imagename = "halfnormal-plot-final";
%envelope(data=dados2, predict=predito, resid=residuo, plinear=diacabeca altura idadegest--numcigpai,
		  scale=351.5097, family=normal, link=identity, sim=100, type=HN);
ods graphics off;
*Normal plot com envelope simulado;
ods listing image_dpi=300;
ods graphics on / reset = all height=4 in width=6 in border=off imagefmt=pdf imagename = "normal-plot-final";
%envelope(data=dados2, predict=predito, resid=residuo, plinear=diacabeca altura idadegest--numcigpai,
		  scale=351.5097, family=normal, link=identity, sim=100, type=N);
ods graphics off;

***** HOMOCEDASTICIDADE;
ods listing image_dpi=300;
ods graphics on / reset = all height=6 in width=8 in border=off imagefmt=pdf imagename = "residuovspredito-final";
*Método gráfico, plotar resíduo pelos valores preditos;
proc sgplot data=dados2;
	title " ";
	scatter x=predito y=residuo / markerattrs=(color=black symbol=circle size=8);
	refline 0 / axis=y lineattrs=(color=red pattern=shortdash thickness=2);
	refline 3 -3 / axis=y lineattrs=(color=black thickness=2);
	xaxis label="Valores preditos";
	yaxis label="Resíduo studentizado" values = (-4 to 4 by 1);
quit;
ods graphics off;

*Teste de white e breusch: hipótese nula de que as variâncias são iguais;
proc model data=dados;
	parms b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11;
	pesonasc = b0 + b1*diacabeca + b2*idadegest + b3*numcigmae+ b4*alturamae + b5*pesomae + b6*alturapai;
	fit pesonasc / white breusch=(1 diacabeca idadegest numcigmae alturamae pesomae alturapai);
quit;

***** DIAGNÓSTICO DE INFLUÊNCIA;
proc reg data=dados;
	id identificacao;
	model pesonasc = diacabeca idadegest numcigmae alturamae pesomae alturapai / influence;
	ods output  OutputStatistics=estatisticas;
	output out=cook COOKD=cookd;
quit;

ods graphics on;
proc reg data=dados plots(label)=(CooksD RStudentByLeverage DFFITS DFBETAS);
	id identificacao;
	model pesonasc = diacabeca idadegest numcigmae alturamae pesomae alturapai;
quit;
ods graphics off;

*h_ii = 2p/n = 12/680 = 0.01764;
%let cutoff_leverage = 0.01764;
data leverage(keep=Observation rstudent HatDiagonal label modrstudent color id);
	set estatisticas;
	modrstudent = abs(RStudent);
	id=observation;
	if HatDiagonal > &cutoff_leverage and modrstudent > 3 then Label = "Outlier e Leverage";
	else if HatDiagonal > &cutoff_leverage then label = "Leverage";
	else if modrstudent > 3 then Label = "Outlier";
	if label = "Outlier" then color = 2;
	else if label = "Leverage" then color = 3;
	else if label = "Outlier e Leverage" then color = 4;
	else color = 1;
	if color = 1 then id = "";
run;

*grafico leverage;
ods listing image_dpi=300;
ods graphics on / reset = all height=6 in width=8 in border=off imagefmt=pdf imagename = "leverage";
proc sgplot data=leverage;
	scatter x = hatdiagonal y = rstudent / group=label datalabel=id
	markerattrs=(symbol=circlefilled);
	refline &cutoff_leverage/axis=X lineattrs=(color=black thickness=1);
	refline 3 -3 /axis=Y lineattrs=(color=black thickness=1);
	xaxis label='Diagonal matriz leverage';
	yaxis label='Resíduo studentizado' values=(-4 to 4 by 1);
quit;
ods graphics off;

*grafico dfbeta;
ods listing image_dpi=300;
ods graphics on / reset = all height=6 in width=8 in border=off imagefmt=pdf imagename = "dfbeta";
proc reg data=dados plots(label)=(DFBETAS);
	id identificacao;
	model pesonasc = diacabeca idadegest numcigmae alturamae pesomae alturapai;
quit;
ods graphics off;

*grafico dffit;
data dffit(keep = observation DFFITS id);
	set estatisticas;
	if dffits > 0.4 or dffits < -0.4 then id = observation;
run;
ods listing image_dpi=300;
ods graphics on / reset = all height=6 in width=8 in border=off imagefmt=pdf imagename = "dffit";
proc sgplot data=dffit;
	needle x=observation y=dffits / datalabel=id markerattrs=(symbol=circlefilled);
	refline 0.4 -0.4 /axis=y lineattrs=(color=black thickness=1);
	xaxis label="Observações";
	yaxis label="Distância de Cook";
quit;
ods graphics off;

*grafico distância de cook;
data cook(keep=id cookd obs);
	set cook;
	obs = _n_;
	if obs ^= 9 and obs ^= 60 and obs ^= 167 then id = "";
run;
ods listing image_dpi=300;
ods graphics on / reset = all height=6 in width=8 in border=off imagefmt=pdf imagename = "cook";
proc sgplot data=cook;
	needle x=Obs y=cookd / datalabel=id markerattrs=(symbol=circlefilled);
	xaxis label="Observações";
	yaxis label="Distância de Cook" values=(0 to 0.05 by 0.01);
quit;
ods graphics off;

* Modelo sem a observação 167;
proc reg data=dados;
	id identificacao;
	model pesonasc = diacabeca idadegest numcigmae alturamae pesomae alturapai;
	reweight identificacao = 167;
quit;

* Modelo sem a observação 60;
proc reg data=dados;
	id identificacao;
	model pesonasc = diacabeca idadegest numcigmae alturamae pesomae alturapai;
	reweight / reset;
	reweight identificacao = 60;
quit;

* Modelo sem a observação 9;
proc reg data=dados;
	id identificacao;
	model pesonasc = diacabeca idadegest numcigmae alturamae pesomae alturapai;
	reweight identificacao = 9;
quit;

* Modelo sem a observação 9, 60 e 167;
proc reg data=dados;
	id identificacao;
	model pesonasc = diacabeca idadegest numcigmae alturamae pesomae alturapai;
	reweight identificacao = 9; 
	reweight identificacao = 60;
	reweight identificacao = 167;
quit;

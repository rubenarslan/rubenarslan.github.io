﻿* Encoding: UTF-8.



**** Person-period dataset for GEE models ****


varstocases  
 /make OESTR from ZT1_OESTR ZT2_OESTR 
 /make OESTR_raw from T1_OESTR T2_OESTR 
 /make Sex_rating_Faces from T1_SR_Faces T2_SR_Faces 
 /make Sex_rating_Bodies from T1_SR_Bodies T2_SR_Bodies 
 /make Sex_rating_Kissing from T1_SR_Kissing T2_SR_Kissing 
 /make Sex_rating_Intercourse from T1_SR_Intercourse T2_SR_Intercourse 
 /index=time (2)
 /null keep.



**** Examples GEE models *****

**** Sex rating over time IVF ****

GENLIN Sex_rating_Faces BY time (ORDER=DESCENDING) 
  /MODEL time INTERCEPT=YES 
     DISTRIBUTION=NORMAL LINK=IDENTITY
  /CRITERIA METHOD=FISHER(1) MAXITERATIONS=1000 MAXSTEPHALVING=50 SCALE=MLE ANALYSISTYPE=3(WALD) CILEVEL=95 LIKELIHOOD=FULL 
  /REPEATED SUBJECT=ID WITHINSUBJECT=time SORT=YES CORRTYPE=UNSTRUCTURED ADJUSTCORR=YES COVB=ROBUST MAXITERATIONS=100  
  /EMMEANS SCALE=ORIGINAL 
  /EMMEANS TABLES=time SCALE=ORIGINAL COMPARE=time CONTRAST=PAIRWISE PADJUST=SEQBONFERRONI
  /MISSING CLASSMISSING=EXCLUDE 
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION WORKINGCORR.


**** Sex rating in association with oestrogen IVF ****

GENLIN Sex_rating_Kissing BY time WITH OESTR
  /MODEL time OESTR INTERCEPT=YES 
     DISTRIBUTION=NORMAL LINK=IDENTITY
  /CRITERIA METHOD=FISHER(1) MAXITERATIONS=1000 MAXSTEPHALVING=50 SCALE=MLE ANALYSISTYPE=3(WALD) CILEVEL=95 LIKELIHOOD=FULL 
  /REPEATED SUBJECT=ID WITHINSUBJECT=time SORT=YES CORRTYPE=UNSTRUCTURED ADJUSTCORR=YES COVB=ROBUST MAXITERATIONS=100  
  /MISSING CLASSMISSING=EXCLUDE 
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION WORKINGCORR.



**** Example Intraindividual change model IVF *****

REGRESSION 
  /MISSING LISTWISE 
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10) 
  /NOORIGIN 
  /DEPENDENT T2_T1_Kissing  
  /METHOD=ENTER ZT2_T1_Oestr 
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HIST(ZRESID) NORM(ZRESID) OUTLIERS.






